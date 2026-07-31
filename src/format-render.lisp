(in-package :cl-cc/vm)

;;; Native FORMAT control-string parser and top-level driver
;;; (%vm-format-render / %vm-format-render-to-string / %vm-format-native),
;;; built on format-render-directives.lisp's directive primitives (loads
;;; before this file).

(defun %vm-format-directive-char-p (char)
  (or (cl:alpha-char-p char)
      (cl:find char "%&~*?[]{}<>()^/;$|_I")))

(defun %vm-format-parse-param-token (token ctx)
  (cond
    ((string= token "") nil)
    ((string= token "#") (%vm-format-remaining-count ctx))
    ((string-equal token "V") (if (plusp (%vm-format-remaining-count ctx))
                                  (%vm-format-next-arg ctx)
                                  nil))
    ((and (> (cl:length token) 1) (char= (cl:char token 0) #\'))
     (cl:char token 1))
    (t (parse-integer token))))

(defun %vm-format-parse-directive (format-string start ctx)
  (let ((len (cl:length format-string))
        (pos start)
        (params nil)
        (token "")
        (colonp nil)
        (atsignp nil))
    (labels ((push-param ()
               (push (%vm-format-parse-param-token token ctx) params)
               (setf token "")))
      (loop while (< pos len)
            for ch = (cl:char format-string pos)
            do (cond
                 ((char= ch #\,)
                  (push-param)
                  (incf pos))
                 ((char= ch #\:)
                   (when (> (cl:length token) 0) (push-param))
                  (setf colonp t)
                  (incf pos))
                 ((char= ch #\@)
                   (when (> (cl:length token) 0) (push-param))
                  (setf atsignp t)
                  (incf pos))
                 ((and (char= ch #\Space)
                       (or params (> (cl:length token) 0)))
                   (when (> (cl:length token) 0) (push-param))
                  (incf pos))
                  ((%vm-format-directive-char-p ch)
                    (when (> (cl:length token) 0) (push-param))
                   (return-from %vm-format-parse-directive
                     (values (nreverse params) colonp atsignp ch (1+ pos))))
                 (t (setf token (concatenate 'string token (string ch)))
                    (incf pos))))
      (error "Unterminated FORMAT directive in ~S" format-string))))

(defun %vm-format-matching-close (open)
  (case open
    (#\[ #\])
    (#\{ #\})
    (#\< #\>)
    (#\( #\))
    (t (error "No matching close directive for ~A" open))))

(defun %vm-format-find-section-end (format-string start open)
  (let ((close (%vm-format-matching-close open))
        (depth 0)
        (pos start)
        (len (cl:length format-string)))
    (loop while (< pos len)
          do (if (char= (cl:char format-string pos) #\~)
                 (multiple-value-bind (_params _colonp _atsignp dir next)
                     (%vm-format-parse-directive format-string (1+ pos)
                                                 (%make-vm-format-context #()))
                   (declare (ignore _params _colonp _atsignp))
                   (cond
                     ((char= dir open) (incf depth))
                      ((char= dir close)
                       (if (zerop depth)
                           (return-from %vm-format-find-section-end
                             (values pos next))
                           (decf depth))))
                   (setf pos next))
                 (incf pos)))
    (error "Unterminated FORMAT section ~A in ~S" open format-string)))

(defun %vm-format-split-clauses (string)
  (let ((clauses nil)
        (else-index nil)
        (start 0)
        (pos 0)
        (len (cl:length string)))
    (loop while (< pos len)
          do (if (char= (cl:char string pos) #\~)
                 (multiple-value-bind (_params colonp _atsignp dir next)
                     (%vm-format-parse-directive string (1+ pos)
                                                 (%make-vm-format-context #()))
                   (declare (ignore _params _atsignp))
                   (if (char= dir #\;)
                        (progn
                          (push (cl:subseq string start pos) clauses)
                          (when colonp (setf else-index (cl:length clauses)))
                          (setf start next
                                pos next))
                       (setf pos next)))
                 (incf pos)))
    (push (cl:subseq string start) clauses)
    (values (nreverse clauses) else-index)))

(defun %vm-format-call-user-function (name arg colonp atsignp params stream)
  (let* ((slash (cl:position #\: name :from-end t))
         (pkg-name (and slash (cl:subseq name 0 slash)))
         (sym-name (if slash (cl:subseq name (1+ slash)) name))
         (package (if pkg-name (find-package (string-upcase pkg-name)) *package*)))
    (unless package (error "Unknown FORMAT function package: ~A" pkg-name))
    (let ((symbol (find-symbol (string-upcase sym-name) package)))
      (unless (and symbol (fboundp symbol))
        (error "Unknown FORMAT function: ~A" name))
      (apply (symbol-function symbol) stream arg colonp atsignp params))))

(defun %vm-format-render-iteration (format-string ctx stream params colonp atsignp next)
  "Implement the ~{...~} FORMAT directive: iterate BODY (the section text
between NEXT and its matching ~}) as ~@{ over remaining args, ~:{ over a
list of argument sublists, or ~{ over one shared sub-context. Returns the
position after the closing ~}."
  (multiple-value-bind (section-end after-section)
      (%vm-format-find-section-end format-string next #\{)
    (let* ((body (cl:subseq format-string next section-end))
           (max-iterations (%vm-format-param params 0 nil))
           (items (if atsignp nil (%vm-format-next-arg ctx))))
      (flet ((under-max (count) (or (null max-iterations)
                                    (< count max-iterations))))
        (cond
          ;; ~@{ ~}: iterate over the remaining FORMAT arguments
          ;; directly; ~^ / arg exhaustion ends it.
          (atsignp
           (loop with count = 0
                 while (and (under-max count)
                            (plusp (%vm-format-remaining-count ctx)))
                 do (multiple-value-bind (c term)
                        (%vm-format-render body ctx stream)
                      (declare (ignore c))
                      (when term (return)))
                    (incf count)))
          ;; ~:{ ~}: ITEMS is a list of sublists; each sublist is
          ;; one iteration step's argument set.
          (colonp
           (loop with count = 0
                 for sub in items
                 while (under-max count)
                 do (%vm-format-write stream
                                      (%vm-format-render-to-string body sub))
                    (incf count)))
          ;; ~{ ~}: ITEMS is the list. Render BODY repeatedly
          ;; against ONE shared sub-context so the body consumes
          ;; one-or-more args per pass and ~^ sees the remaining
          ;; args (terminating when the list is exhausted mid-pass).
          (t
           (let ((sub-ctx (%make-vm-format-context (coerce items 'vector))))
             (loop with count = 0
                   while (and (under-max count)
                              (plusp (%vm-format-remaining-count sub-ctx)))
                   do (let ((before (%vm-format-remaining-count sub-ctx)))
                        (multiple-value-bind (c term)
                            (%vm-format-render body sub-ctx stream)
                          (declare (ignore c))
                          (when term (return))
                          ;; a body that consumes no args would loop forever
                          (when (= (%vm-format-remaining-count sub-ctx) before)
                            (return))))
                      (incf count)))))))
    after-section))

(defun %vm-format-render-conditional (format-string ctx stream colonp atsignp next)
  "Implement the ~[...~] FORMAT directive: pick the SELECTOR-th clause
between NEXT and its matching ~], where SELECTOR is a boolean test under
~:[ (0/1), a presence test under ~@[ (peeks, does not consume), or a
consumed integer argument otherwise; an else clause (after ~:;) is used
when SELECTOR has no matching numbered clause. Returns the position after
the closing ~]."
  (multiple-value-bind (section-end after-section)
      (%vm-format-find-section-end format-string next #\[)
    (multiple-value-bind (clauses else-index)
        (%vm-format-split-clauses (cl:subseq format-string next section-end))
      (let* ((selector (cond
                          (colonp (if (%vm-format-next-arg ctx) 1 0))
                          (atsignp (let ((arg (%vm-format-peek-arg ctx)))
                                     (if arg 0 nil)))
                          (t (%vm-format-next-arg ctx))))
             (selected (cond
                         ((null selector) nil)
                         ((and (integerp selector)
                               (< -1 selector (cl:length clauses)))
                          (nth selector clauses))
                         (else-index (nth else-index clauses)))))
        (when selected
          (%vm-format-render selected ctx stream))))
    after-section))

(defun %vm-format-render (format-string ctx stream &key (start 0) end)
  (let ((pos start)
        (limit (or end (cl:length format-string))))
    (loop while (< pos limit)
          for ch = (cl:char format-string pos)
          do (if (char= ch #\~)
                 (multiple-value-bind (params colonp atsignp directive next)
                     (%vm-format-parse-directive format-string (1+ pos) ctx)
                   (let ((dir (char-upcase directive)))
                     (case dir
                        (#\A (%vm-format-pad stream (princ-to-string (%vm-format-next-arg ctx))
                                             (%vm-format-param params 0 nil)
                                             (%vm-format-param params 1 #\Space)
                                             atsignp ctx))
                        (#\S (%vm-format-pad stream (write-to-string (%vm-format-next-arg ctx))
                                             (%vm-format-param params 0 nil)
                                             (%vm-format-param params 1 #\Space)
                                             atsignp ctx))
                        (#\% (dotimes (_ (or (%vm-format-param params 0 1) 1))
                               (declare (ignore _))
              (cl:terpri stream)
                               (setf (%vm-format-context-column ctx) 0)))
                        (#\& (dotimes (_ (or (%vm-format-param params 0 1) 1))
                               (declare (ignore _))
              (cl:fresh-line stream)
                               (setf (%vm-format-context-column ctx) 0)))
                       (#\~ (dotimes (_ (or (%vm-format-param params 0 1) 1))
                              (declare (ignore _))
              (cl:write-char #\~ stream)
                               (incf (%vm-format-context-column ctx))))
                        (#\D (%vm-format-integer (%vm-format-next-arg ctx) 10 colonp atsignp params stream ctx))
                        (#\B (%vm-format-integer (%vm-format-next-arg ctx) 2 colonp atsignp params stream ctx))
                        (#\O (%vm-format-integer (%vm-format-next-arg ctx) 8 colonp atsignp params stream ctx))
                        (#\X (%vm-format-integer (%vm-format-next-arg ctx) 16 colonp atsignp params stream ctx))
                       (#\R (%vm-format-radix (%vm-format-next-arg ctx) colonp atsignp params stream ctx))
                       ((#\F #\E #\G #\$) (%vm-format-float dir (%vm-format-next-arg ctx) params stream ctx))
                        (#\C (%vm-format-character (%vm-format-next-arg ctx) colonp atsignp stream ctx))
                        (#\P (%vm-format-plural ctx colonp atsignp stream))
                        (#\W (%vm-format-write-directive ctx colonp atsignp stream))
                        (#\T (let* ((colnum (or (%vm-format-param params 0 1) 1))
                                     (colinc (max 1 (or (%vm-format-param params 1 1) 1)))
                                     (current (%vm-format-context-column ctx))
                                     (spaces (if atsignp
                                                 colnum
                                                 (if (< current colnum)
                                                     (- colnum current)
                                                     (let ((offset (mod (- current colnum) colinc)))
                                                       (if (zerop offset)
                                                           colinc
                                                           (- colinc offset)))))))
                                  (dotimes (_ spaces) (declare (ignore _))
                     (cl:write-char #\Space stream))
                                  (setf (%vm-format-context-column ctx) (+ current spaces))))
                        (#\| (dotimes (_ (or (%vm-format-param params 0 1) 1))
                               (declare (ignore _))
              (cl:write-char #\Page stream)
                                (incf (%vm-format-context-column ctx))))
                       (#\* (let ((n (or (%vm-format-param params 0 1) 1)))
                              (cond
                                (colonp (decf (%vm-format-context-index ctx) n))
                                (atsignp (setf (%vm-format-context-index ctx) n))
                                (t (incf (%vm-format-context-index ctx) n)))
                              (setf (%vm-format-context-index ctx)
                                    (max 0 (min (%vm-format-context-index ctx)
                                                 (cl:length (%vm-format-context-args ctx)))))))
                       (#\? (let ((subfmt (%vm-format-next-arg ctx)))
                              (if atsignp
                                  (%vm-format-render subfmt ctx stream)
                                  (let ((subargs (%vm-format-next-arg ctx)))
                                    (%vm-format-write
                                     stream
                                     (%vm-format-render-to-string
                                       subfmt
                                       (cond
                                         ((stringp subargs) (list subargs))
                                         ((vectorp subargs) (coerce subargs 'list))
                                         ((listp subargs) subargs)
                                         (t (list subargs))))
                                     ctx)))))
                       (#\[ (setf next (%vm-format-render-conditional
                                        format-string ctx stream colonp atsignp next)))
                       (#\{ (setf next (%vm-format-render-iteration
                                        format-string ctx stream params colonp atsignp next)))
                       (#\< (multiple-value-bind (section-end after-section)
                                (%vm-format-find-section-end format-string next #\<)
                               (let* ((body (cl:subseq format-string next section-end))
                                      (saved-column (%vm-format-context-column ctx))
                                     (text (with-output-to-string (out)
                                             (%vm-format-render body ctx out))))
                                (setf (%vm-format-context-column ctx) saved-column)
                                (%vm-format-pad stream text (%vm-format-param params 0 0)
                                                (%vm-format-param params 1 #\Space)
                                                atsignp ctx))
                              (setf next after-section)))
                       (#\( (multiple-value-bind (section-end after-section)
                                (%vm-format-find-section-end format-string next #\()
                              (let* ((body (cl:subseq format-string next section-end))
                                     (saved-column (%vm-format-context-column ctx))
                                     (text (with-output-to-string (out)
                                             (%vm-format-render body ctx out)))
                                     (converted (%vm-format-convert-case text colonp atsignp)))
                                (setf (%vm-format-context-column ctx) saved-column)
                                (%vm-format-write stream converted ctx))
                              (setf next after-section)))
                       (#\^ (when (%vm-format-escape-terminate-p params ctx)
                              (return-from %vm-format-render (values ctx t))))
                         (#\/ (let ((slash (cl:position #\/ format-string :start next :end limit)))
                                (unless slash (error "Unterminated ~~/ FORMAT directive"))
                                (%vm-format-call-user-function (cl:subseq format-string next slash)
                                                             (%vm-format-next-arg ctx)
                                                             colonp atsignp params stream)
                               (setf next (1+ slash))))
                        ;; ─── ~I (indent) ────────────────────────────────────────
                        ;; ~nI → indent relative. ~n:I → indent to column n.
                        ;; ~n@I → indent relative to current column + n.
                        (#\I (let ((n (or (%vm-format-param params 0 0) 0)))
                               (cond
                                 (colonp
                                  ;; ~n:I — indent to absolute column n
                                  (let* ((current (%vm-format-context-column ctx))
                                         (spaces (if (> n current) (- n current) 0)))
                                    (dotimes (_ spaces) (declare (ignore _))
                  (cl:write-char #\Space stream))
                                    (setf (%vm-format-context-column ctx)
                                          (max (%vm-format-context-column ctx) n))))
                                 (atsignp
                                  ;; ~n@I — newline then indent n relative to current position
              (cl:terpri stream)
                                  (setf (%vm-format-context-column ctx) 0)
                                  (dotimes (_ n) (declare (ignore _))
                  (cl:write-char #\Space stream))
                                  (incf (%vm-format-context-column ctx) n))
                                 (t
                                  ;; ~nI — indent n spaces relative to current position (no newline)
                                  (dotimes (_ n) (declare (ignore _))
                  (cl:write-char #\Space stream))
                                  (incf (%vm-format-context-column ctx) n)))))
                        ;; ─── ~_ (conditional newline) ───────────────────────────
                        ;; ~_ → newline. ~n_ → n newlines.
                        ;; ~:_ → process like ~% but at section start (pprint).
                        ;; ~@_ → call pprint-newline :fill.
                        (#\_ (let ((n (or (%vm-format-param params 0 1) 1)))
                               (cond
                                 (colonp
                                  ;; ~:_ — like ~% for pprint (basic: just newline)
                                  (dotimes (_ n) (declare (ignore _))
                  (cl:terpri stream)
                                    (setf (%vm-format-context-column ctx) 0)))
                                 (atsignp
                                  ;; ~@_ — pprint-newline :fill (basic: conditional newline)
                                  (when (plusp n)
                   (cl:terpri stream)
                                    (setf (%vm-format-context-column ctx) 0)))
                                 (t
                                  ;; ~_ — emit newline
                                  (dotimes (_ n) (declare (ignore _))
                   (cl:terpri stream)
                                    (setf (%vm-format-context-column ctx) 0))))))
                       (otherwise (error "Unsupported FORMAT directive: ~A" directive)))
                     (setf pos next)))
                  (progn
              (cl:write-char ch stream)
                    (if (char= ch #\Newline)
                        (setf (%vm-format-context-column ctx) 0)
                        (incf (%vm-format-context-column ctx)))
                    (incf pos))))
    (values ctx nil)))

(defun %vm-format-render-to-string (format-string arg-vals)
  (let ((ctx (%make-vm-format-context (coerce arg-vals 'vector))))
    (values (with-output-to-string (out)
              (%vm-format-render format-string ctx out))
            (%vm-format-context-index ctx))))

(defun %vm-format-native (format-string arg-vals &optional stream)
  "Render FORMAT-STRING with ARG-VALS using cl-cc's native VM FORMAT processor.
When STREAM is NIL, return the produced string.  When STREAM is non-NIL, write to
it via runtime stream functions and return NIL."
  (check-type format-string string)
  (if stream
      (progn
        (%vm-format-render format-string
                           (%make-vm-format-context (coerce arg-vals 'vector))
                           stream)
        nil)
      (with-output-to-string (out)
        (%vm-format-render format-string
                           (%make-vm-format-context (coerce arg-vals 'vector))
                           out))))
