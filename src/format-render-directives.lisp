(in-package :cl-cc/vm)

;;; Native FORMAT directive primitives: the %vm-format-context arg cursor,
;;; output/padding/param helpers, integer digit-grouping, English/Roman
;;; numeral spelling (~R, ~:R), and float/character/case-conversion
;;; rendering. The control-string parser and top-level driver that dispatch
;;; through these are in format-render.lisp (loads next).

(defstruct (%vm-format-context (:constructor %make-vm-format-context (args)))
  (args #() :type vector)
  (index 0 :type fixnum)
  (last-arg nil)
  (column 0 :type fixnum))

(defun %vm-format-remaining-count (ctx)
  (- (cl:length (%vm-format-context-args ctx))
     (%vm-format-context-index ctx)))

(defun %vm-format-next-arg (ctx)
  (let ((index (%vm-format-context-index ctx)))
    (when (>= index (cl:length (%vm-format-context-args ctx)))
      (error "FORMAT argument exhausted"))
    (prog1 (aref (%vm-format-context-args ctx) index)
      (setf (%vm-format-context-last-arg ctx)
            (aref (%vm-format-context-args ctx) index))
      (setf (%vm-format-context-index ctx) (1+ index)))))

(defun %vm-format-plural (ctx colonp atsignp stream)
  "Implement FORMAT ~P pluralization. ~:P reuses the previous arg; ~@P emits y/ies."
  (let ((value (if colonp
                   (%vm-format-context-last-arg ctx)
                   (%vm-format-next-arg ctx))))
    (%vm-format-write stream
                      (if atsignp
                          (if (eql value 1) "y" "ies")
                          (if (eql value 1) "" "s"))
                      ctx)))

(defun %vm-format-write-directive (ctx colonp atsignp stream)
  "Implement FORMAT ~W through WRITE, honoring the common pretty/readable flags."
  (let ((value (%vm-format-next-arg ctx)))
    (let ((*print-pretty* (or colonp *print-pretty*))
          (*print-circle* (or atsignp *print-circle*))
          (cl:*print-pretty* (or colonp *print-pretty*))
          (cl:*print-circle* (or atsignp *print-circle*)))
      (%vm-format-write stream
                        (vm-write-object-to-string value
                                                   :escape *print-escape*
                                                   :circle *print-circle*)
                        ctx))))

(defun %vm-format-peek-arg (ctx)
  (let ((index (%vm-format-context-index ctx)))
    (when (>= index (cl:length (%vm-format-context-args ctx)))
      (error "FORMAT argument exhausted"))
    (aref (%vm-format-context-args ctx) index)))

(defun %vm-format-note-output (ctx string)
  "Update CTX's best-effort output column after emitting STRING."
  (loop for ch across string
        do (if (char= ch #\Newline)
               (setf (%vm-format-context-column ctx) 0)
               (incf (%vm-format-context-column ctx)))))

(defun %vm-format-write (stream string &optional ctx)
    (cl:write-string string stream)
  (when ctx
    (%vm-format-note-output ctx string)))

(defun %vm-format-pad (stream string mincol padchar atsignp &optional ctx)
  (let* ((text (princ-to-string string))
         (needed (max 0 (- (or mincol 0) (cl:length text)))))
    (if atsignp
        (progn
          (%vm-format-write stream text ctx)
          (dotimes (_ needed) (declare (ignore _))
        (cl:write-char padchar stream)
            (when ctx (if (char= padchar #\Newline)
                          (setf (%vm-format-context-column ctx) 0)
                          (incf (%vm-format-context-column ctx))))))
        (progn
          (dotimes (_ needed) (declare (ignore _))
        (cl:write-char padchar stream)
            (when ctx (if (char= padchar #\Newline)
                          (setf (%vm-format-context-column ctx) 0)
                          (incf (%vm-format-context-column ctx)))))
          (%vm-format-write stream text ctx)))))

(defun %vm-format-param (params index &optional default)
  (let ((value (nth index params)))
    (if (null value) default value)))

(defun %vm-format-escape-terminate-p (params ctx)
  "ANSI ~^ (escape-upward) termination predicate.  With no parameters, terminate
when no arguments remain in CTX.  One parameter n: terminate when n is zero.
Two n,m: when n = m.  Three n,m,p: when n <= m <= p.  (The old test terminated
whenever PARAMS was empty — i.e. for EVERY plain ~^ — so ~{~a~^, ~} dropped all
separators and ~a~^~a stopped after the first arg.)"
  (let ((p0 (%vm-format-param params 0 nil))
        (p1 (%vm-format-param params 1 nil))
        (p2 (%vm-format-param params 2 nil)))
    (cond
      ((and p0 p1 p2) (and (<= p0 p1) (<= p1 p2)))
      ((and p0 p1)    (eql p0 p1))
      (p0             (eql p0 0))
      (t              (zerop (%vm-format-remaining-count ctx))))))

(defun %vm-format-group-digits (string comma-char comma-interval)
  (let* ((signp (and (> (cl:length string) 0) (cl:find (cl:char string 0) "+-")))
         (start (if signp 1 0))
         (digits (cl:subseq string start))
         (interval (or comma-interval 3))
         (comma (or comma-char #\,)))
    (with-output-to-string (out)
      (when signp (write-char (cl:char string 0) out))
      (loop for i from 0 below (cl:length digits)
            when (and (> i 0)
                      (zerop (mod (- (cl:length digits) i) interval)))
              do (write-char comma out)
            do (write-char (cl:char digits i) out)))))

(defun %vm-format-integer (value radix colonp atsignp params stream &optional ctx)
  (let* ((mincol (%vm-format-param params 0 nil))
         (padchar (%vm-format-param params 1 #\Space))
         (comma-char (%vm-format-param params 2 #\,))
         (comma-interval (%vm-format-param params 3 3))
         (text (let ((*print-base* radix) (*print-radix* nil))
                 (write-to-string value :base radix :radix nil)))
         (signed (if (and atsignp (numberp value) (not (minusp value)))
                     (concatenate 'string "+" text)
                     text))
         (grouped (if colonp
                      (%vm-format-group-digits signed comma-char comma-interval)
                      signed)))
    (%vm-format-pad stream grouped mincol padchar nil ctx)))

(defparameter +vm-format-small-cardinals+
  #("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"
    "ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen"
    "seventeen" "eighteen" "nineteen"))

(defparameter +vm-format-small-ordinals+
  #("zeroth" "first" "second" "third" "fourth" "fifth" "sixth" "seventh" "eighth" "ninth"
    "tenth" "eleventh" "twelfth" "thirteenth" "fourteenth" "fifteenth" "sixteenth"
    "seventeenth" "eighteenth" "nineteenth"))

(defparameter +vm-format-tens+
  #("" "" "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"))

(defun %vm-format-english-under-100 (n ordinalp)
  (cond
    ((< n 20) (aref (if ordinalp +vm-format-small-ordinals+ +vm-format-small-cardinals+) n))
    (t (let* ((tens (floor n 10))
              (ones (mod n 10))
              (base (aref +vm-format-tens+ tens)))
         (cond
           ((zerop ones) (if ordinalp
                              (concatenate 'string (cl:subseq base 0 (max 0 (- (cl:length base) 1))) "ieth")
                             base))
           (t (concatenate 'string base "-" (%vm-format-english-under-100 ones ordinalp))))))))

(defun %vm-format-english (n ordinalp)
  (cond
    ((minusp n) (concatenate 'string "minus " (%vm-format-english (- n) ordinalp)))
    ((< n 100) (%vm-format-english-under-100 n ordinalp))
    ((< n 1000)
     (let ((hundreds (floor n 100))
           (rest (mod n 100)))
       (if (zerop rest)
           (concatenate 'string (%vm-format-english-under-100 hundreds nil)
                        (if ordinalp " hundredth" " hundred"))
           (concatenate 'string (%vm-format-english-under-100 hundreds nil)
                        " hundred " (%vm-format-english-under-100 rest ordinalp)))))
    (t (princ-to-string n))))

(defun %vm-format-roman (n oldp)
  (declare (ignore oldp))
  (unless (and (integerp n) (< 0 n 4000))
    (return-from %vm-format-roman (princ-to-string n)))
  (let ((pairs '((1000 . "M") (900 . "CM") (500 . "D") (400 . "CD")
                 (100 . "C") (90 . "XC") (50 . "L") (40 . "XL")
                 (10 . "X") (9 . "IX") (5 . "V") (4 . "IV") (1 . "I"))))
    (with-output-to-string (out)
      (dolist (pair pairs)
        (loop while (>= n (car pair))
              do (write-string (cdr pair) out)
                 (decf n (car pair)))))))

(defun %vm-format-radix (value colonp atsignp params stream &optional ctx)
  (let ((radix (%vm-format-param params 0 nil)))
    (cond
      (radix (%vm-format-integer value radix colonp atsignp (cdr params) stream ctx))
      (atsignp (%vm-format-write stream (%vm-format-roman value colonp) ctx))
      (t (%vm-format-write stream (%vm-format-english value colonp) ctx)))))

(defun %vm-format-params-prefix (params)
  "Render PARAMS (already resolved to integers/characters/nil) as a directive
parameter prefix, e.g. (6 2) -> \"6,2\", (nil 2) -> \",2\", (6) -> \"6\". Trailing
nils are dropped; a nil between non-nils becomes an empty field (its default)."
  (let ((last (position-if #'identity params :from-end t)))
    (if (null last)
        ""
        (cl:with-output-to-string (s)
          (loop for i from 0 to last
                for p = (nth i params)
                do (when (plusp i) (cl:write-char #\, s))
                   (when p
                     (if (characterp p)
                         (progn (cl:write-char #\' s) (cl:write-char p s))
                         (princ p s))))))))

(defun %vm-format-float (directive value params stream &optional ctx)
  ;; With explicit parameters (~w,dF, ~,2$, ...), reconstruct the directive and
  ;; delegate to the host FORMAT, which implements the full ANSI width/decimals/
  ;; scale/pad semantics — the VM shortest/fixed printer ignored them entirely
  ;; (~,2f printed full precision). ~$ always delegates (its default is 2 decimals,
  ;; not the shortest representation). Parameter-free ~F/~E/~G keep the existing
  ;; VM printer for byte-identical output to before.
  (if (or (char= directive #\$)
          (find-if #'identity params))
      (%vm-format-write
       stream
       (cl:format nil
                  (concatenate 'string "~" (%vm-format-params-prefix params)
                               (string directive))
                  value)
       ctx)
      (let ((mode (case directive
                    (#\F :fixed)
                    (#\E :exponential)
                    (#\G :shortest)
                    (#\$ :fixed)
                    (otherwise :shortest))))
        (%vm-format-write stream
                          (if (fboundp 'vm-float-to-string)
                              (vm-float-to-string value :mode mode)
                              (princ-to-string value))
                          ctx))))

(defun %vm-format-character-name (ch)
  (or (char-name ch) (string ch)))

(defun %vm-format-character-readable (ch)
  (concatenate 'string "#\\" (%vm-format-character-name ch)))

(defun %vm-format-character (char colonp atsignp stream &optional ctx)
  (let ((ch (if (characterp char) char (code-char char))))
    (cond
      ((and colonp atsignp) (%vm-format-write stream (%vm-format-character-readable ch) ctx))
      (colonp (%vm-format-write stream (%vm-format-character-name ch) ctx))
      (atsignp (%vm-format-write stream (%vm-format-character-readable ch) ctx))
    (t (%vm-format-write stream (string ch) ctx)))))

(defun %vm-format-capitalize-first-word (string)
  (let ((result (string-downcase string))
        (converted nil))
    (loop for i below (cl:length result)
          for ch = (cl:char result i)
          when (and (not converted) (alphanumericp ch))
            do (setf (cl:char result i) (char-upcase ch)
                     converted t))
    result))

(defun %vm-format-convert-case (string colonp atsignp)
  "Apply ANSI FORMAT ~( case conversion modifiers to STRING."
  (cond
    ((and colonp atsignp) (string-upcase string))
    (colonp (string-capitalize string))
    (atsignp (%vm-format-capitalize-first-word string))
    (t (string-downcase string))))
