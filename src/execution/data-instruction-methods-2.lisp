(in-package #:cl-cc/vm/execution)

(defmethod execute-instruction ((inst vm-export-inst) state pc labels)
  (declare (ignore labels))
  (let* ((symbols (vm-reg-get state (vm-export-symbols inst)))
         (pkg (vm-reg-get state (vm-export-pkg inst)))
         (result (%vm-package-operation-result
                  pkg
                  (function cl-cc/runtime::rt-export)
                  (function export)
                  (list symbols pkg))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-gensym-inst) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (gensym))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-find-symbol) state pc labels)
  (declare (ignore labels))
  (let* ((name (string (%vm-host-string (vm-reg-get state (vm-src inst)))))
         (package-supplied-p (vm-find-symbol-pkg inst))
         (package-designator
           (when package-supplied-p
             (vm-reg-get state (vm-find-symbol-pkg inst)))))
    (multiple-value-bind (symbol status)
        (if package-supplied-p
            (cl-cc/vm/data:vm-find-symbol name package-designator)
            (cl-cc/vm/data:vm-find-symbol name))
      (vm-reg-set state (vm-dst inst) symbol)
      (setf (vm-values-list state) (list symbol status))
      (values (1+ pc) nil nil))))

(defmethod execute-instruction ((inst vm-find-package) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (runtime-package host-designator)
      (%vm-resolve-package-designator (vm-reg-get state (vm-src inst)))
    (let ((result
            (or runtime-package
                (and *vm-self-host-mode*
                     (not (packagep host-designator))
                     (error "Package self-hosting required: package ~S not found in runtime registry"
                            (vm-reg-get state (vm-src inst))))
                (find-package host-designator))))
      (vm-reg-set state (vm-dst inst) result)
      (values (1+ pc) nil nil))))

(defmethod execute-instruction ((inst vm-remove-package-local-nickname)
                                  state pc labels)
  (declare (ignore labels))
  (let* ((pkg-designator (vm-reg-get state (vm-local-nickname-pkg inst)))
         (nickname (vm-reg-get state (vm-local-nickname-nick inst)))
         (fn (%vm-host-package-local-nickname-function
              "REMOVE-PACKAGE-LOCAL-NICKNAME")))
    (multiple-value-bind (runtime-package host-package)
        (%vm-resolve-package-designator pkg-designator)
      (declare (ignore runtime-package))
      (let ((result (funcall fn nickname host-package)))
        (vm-reg-set state (vm-dst inst) result)
        (values (1+ pc) nil nil)))))

(defmethod execute-instruction ((inst vm-add-package-local-nickname) state pc labels)
  (declare (ignore labels))
  (let* ((pkg-designator (vm-reg-get state (vm-local-nickname-pkg inst)))
         (nickname (vm-reg-get state (vm-local-nickname-nick inst)))
         (target (vm-reg-get state (vm-local-nickname-target inst)))
         (fn (%vm-host-package-local-nickname-function
              "ADD-PACKAGE-LOCAL-NICKNAME")))
    (multiple-value-bind (runtime-target host-target)
        (%vm-resolve-package-designator target)
      (declare (ignore runtime-target))
      (multiple-value-bind (runtime-package host-package)
          (%vm-resolve-package-designator pkg-designator)
        (declare (ignore runtime-package))
        (let ((result (funcall fn nickname host-target host-package)))
          (vm-reg-set state (vm-dst inst) result)
          (values (1+ pc) nil nil))))))

(defmethod execute-instruction ((inst vm-intern-symbol) state pc labels)
  (declare (ignore labels))
  (let* ((name (string (%vm-host-string (vm-reg-get state (vm-src inst)))))
         (package-supplied-p (vm-intern-pkg inst))
         (package-designator
           (when package-supplied-p
             (vm-reg-get state (vm-intern-pkg inst))))
         (result
           (cond
             ((not package-supplied-p)
              (cond
                (cl-cc/bootstrap:*runtime-intern-fn*
                 (funcall cl-cc/bootstrap:*runtime-intern-fn* name))
                (*vm-self-host-mode*
                 (error "Package self-hosting required: cannot intern ~S without runtime package registry"
                        name))
                (t
                 (intern name))))
             (t
              (multiple-value-bind (runtime-package host-designator)
                  (%vm-resolve-package-designator package-designator)
                (cond
                  (runtime-package
                   (cond
                     (cl-cc/bootstrap:*runtime-intern-fn*
                      (funcall cl-cc/bootstrap:*runtime-intern-fn*
                               name runtime-package))
                     (*vm-self-host-mode*
                      (error "Package self-hosting required: cannot intern ~S without runtime package registry"
                             name))
                     (t
                      (cl-cc/runtime:rt-intern name runtime-package))))
                  (*vm-self-host-mode*
                   (error "Package self-hosting required: cannot intern ~S without runtime package registry"
                          name))
                  (t
                   (intern name (find-package host-designator)))))))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-symbol-name) state pc labels)
  (declare (ignore labels))
  (let ((result (%vm-maybe-sso-string (symbol-name (vm-reg-get state (vm-src inst))))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

;;; Fast path: immediate symbols don't need the general symbol-name path.
(defmethod execute-instruction :around ((inst vm-symbol-name) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (if (vm-immediate-symbol-p value)
        (progn
          (vm-reg-set state (vm-dst inst)
                      (rt-string-intern (symbol-name (vm-decode-symbol value))))
          (values (1+ pc) nil nil))
        (call-next-method))))

(defmethod execute-instruction :around ((inst vm-keywordp) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (if (vm-immediate-symbol-p value)
        (progn
          (vm-reg-set state (vm-dst inst)
                      (if (keywordp (vm-decode-symbol value)) 1 0))
          (values (1+ pc) nil nil))
        (call-next-method))))

;;; Immediate-symbol fast paths: bypass the general execute-instruction method
;;; when the source value is a packed immediate symbol (vm-immediate-symbol-p).

(defmethod execute-instruction :around ((inst vm-intern-symbol) state pc labels)
  (multiple-value-bind (next-pc value-1 value-2) (call-next-method)
    (when (vm-immediate-intern-enabled-p)
      (let ((value (vm-reg-get state (vm-dst inst))))
        (when (symbolp value)
          (vm-reg-set state (vm-dst inst) (vm-encode-common-symbol value)))))
    (values next-pc value-1 value-2)))

(defmethod execute-instruction ((inst vm-string-set) state pc labels)
  (declare (ignore labels))
  (let ((v (vm-reg-get state (vm-val-reg inst))))
    (vm-string-set-char (vm-reg-get state (vm-str-reg inst))
                        (vm-reg-get state (vm-idx inst)) v)
    (vm-reg-set state (vm-dst inst) v)
    (values (1+ pc) nil nil)))

;;; Instruction Execution - String Search

(defmethod execute-instruction ((inst vm-search-string) state pc labels)
  (declare (ignore labels))
  (let* ((pattern (%vm-host-string (vm-reg-get state (vm-pattern inst))))
         (string (%vm-host-string (vm-reg-get state (vm-string-reg inst))))
         (start (vm-reg-get state (vm-start inst)))
         (result (or (search pattern string :start2 start) -1)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-concatenate) state pc labels)
  (declare (ignore labels))
  (let* ((parts (or (vm-parts inst)
                     (list (vm-str1 inst) (vm-str2 inst))))
          (result (%vm-maybe-sso-string
                   (apply #'concatenate 'string
                          (mapcar (lambda (reg)
                                    (%vm-host-string (vm-reg-get state reg)))
                                  parts)))))
     (vm-reg-set state (vm-dst inst) result)
     (values (1+ pc) nil nil)))

;;; Instruction Execution - String Manipulation

(defmethod execute-instruction ((inst vm-subseq) state pc labels)
  (declare (ignore labels))
  ;; SUBSEQ is a sequence operation: it must work on lists and vectors, not only
  ;; strings.  The previous version coerced the input to a string, so (subseq
  ;; '(1 2 3) 0 2) and (subseq #(1 2 3) 0 2) — and butlast/last, which expand to
  ;; subseq — raised a type error.
  (let* ((seq   (vm-reg-get state (vm-string-reg inst)))
         (start (vm-reg-get state (vm-start inst)))
         ;; nil end-slot means no upper bound (= end of sequence)
         (end   (if (vm-end inst) (vm-reg-get state (vm-end inst)) nil))
         (result
           (cond
             ((listp seq)
              (subseq seq start (or end (length seq))))
             ((or (stringp seq) (%vm-sso-string-p seq))
              (%vm-maybe-sso-string
               (vm-string-materialize (vm-string-subseq (%vm-host-string seq) start end))))
             (t
              (let ((v (%vm-cow-vector-materialize seq)))
                (subseq v start (or end (length v))))))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-char) state pc labels)
  (declare (ignore labels))
  (let* ((string (vm-reg-get state (vm-char-string inst)))
         (index (vm-reg-get state (vm-char-index inst)))
         (result (%vm-string-char string index)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

;;; Instruction Execution - String Access and Query

(defmethod execute-instruction ((inst vm-string-length) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (%vm-string-length (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-make-string) state pc labels)
  (declare (ignore labels))
  (let* ((size      (vm-reg-get state (vm-src inst)))
         (char-reg  (vm-char inst))
         (init-char (if char-reg (vm-reg-get state char-reg) #\Space)))
    (vm-reg-set state (vm-dst inst) (make-string size :initial-element init-char))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction :around ((inst vm-rplacd) state pc labels)
  (declare (ignore labels))
  (let ((cons-val (vm-reg-get state (vm-cons-reg inst))))
    (if (%vm-managed-cons-pointer-p cons-val)
        (progn
          (%vm-managed-cons-set-slot state cons-val 2
                                     (vm-reg-get state (vm-val-reg inst)))
          (vm-reg-set state (vm-cons-reg inst) cons-val)
          (values (1+ pc) nil nil))
        (call-next-method))))

(defmethod execute-instruction :around ((inst vm-rplaca) state pc labels)
  (declare (ignore labels))
  (let ((cons-val (vm-reg-get state (vm-cons-reg inst))))
    (if (%vm-managed-cons-pointer-p cons-val)
        (progn
          (%vm-managed-cons-set-slot state cons-val 1
                                     (vm-reg-get state (vm-val-reg inst)))
          (vm-reg-set state (vm-cons-reg inst) cons-val)
          (values (1+ pc) nil nil))
        (call-next-method))))

(defmethod execute-instruction :around ((inst vm-cdr) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (if (%vm-managed-cons-pointer-p value)
        (progn
          (vm-reg-set state (vm-dst inst) (%vm-managed-cons-slot state value 2))
          (values (1+ pc) nil nil))
        (call-next-method))))

(defmethod execute-instruction :around ((inst vm-car) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (if (%vm-managed-cons-pointer-p value)
        (progn
          (vm-reg-set state (vm-dst inst) (%vm-managed-cons-slot state value 1))
          (values (1+ pc) nil nil))
        (call-next-method))))

(defmethod execute-instruction :around ((inst vm-cons) state pc labels)
  (declare (ignore labels))
  (if *vm-managed-cons-allocation-enabled*
      (let ((car-val (vm-reg-get state (vm-car-reg inst)))
            (cdr-val (vm-reg-get state (vm-cdr-reg inst))))
        (vm-reg-set state (vm-dst inst)
                    (%vm-managed-cons-alloc state (vm-dst inst) car-val cdr-val))
        (values (1+ pc) nil nil))
      (let ((car-val (vm-reg-get state (vm-car-reg inst)))
            (cdr-val (vm-reg-get state (vm-cdr-reg inst))))
        (vm-reg-set state (vm-dst inst) (cons car-val cdr-val))
        (values (1+ pc) nil nil))))

(defmethod execute-instruction ((inst vm-atom) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst)
                (if (consp (%vm-list-structure-materialize state value)) 0 1))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-listp) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst)
                (if (listp (%vm-list-structure-materialize state value))
                    1
                    0))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-subst) state pc labels)
  (declare (ignore labels))
  (let ((new-val (vm-reg-get state (vm-new-val inst)))
        (old-val (vm-reg-get state (vm-old-val inst)))
        (tree (vm-reg-get state (vm-tree inst))))
    (vm-reg-set state (vm-dst inst)
                (subst new-val old-val
                       (%vm-list-structure-materialize state tree)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-copy-tree) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst)
                (copy-tree (%vm-list-structure-materialize state value)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-copy-list) state pc labels)
  (declare (ignore labels))
  (let ((list-val (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst)
                (copy-list (%vm-list-structure-materialize state list-val)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-nconc) state pc labels)
  (declare (ignore labels))
  (let* ((lhs (vm-reg-get state (vm-lhs inst)))
         (rhs (vm-reg-get state (vm-rhs inst)))
         (lhs-writable (%vm-cow-list-ensure-writable lhs))
         (rhs-value (%vm-cow-list-materialize rhs)))
    (vm-reg-set state (vm-dst inst) (nconc lhs-writable rhs-value))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-equal) state pc labels)
  (declare (ignore labels))
  (let* ((lhs (vm-reg-get state (vm-lhs inst)))
         (rhs (vm-reg-get state (vm-rhs inst)))
         (lhs-value (%vm-list-structure-materialize state lhs))
         (rhs-value (%vm-list-structure-materialize state rhs)))
    (vm-reg-set state (vm-dst inst) (if (equal lhs-value rhs-value) 1 0))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-acons) state pc labels)
  (declare (ignore labels))
  (let* ((key (vm-reg-get state (vm-acons-key inst)))
         (value (vm-reg-get state (vm-acons-value inst)))
         (alist (vm-reg-get state (vm-acons-alist inst))))
    (vm-reg-set state (vm-dst inst) (acons key value alist))
    (values (1+ pc) nil nil)))

;; Execute instructions
(defmethod execute-instruction ((inst vm-assoc) state pc labels)
  (declare (ignore labels))
  (let* ((key (vm-reg-get state (vm-assoc-key inst)))
         (alist (vm-reg-get state (vm-assoc-alist inst)))
         (alist-value (%vm-list-structure-materialize state alist))
         (result (assoc key alist-value)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-pop) state pc labels)
  (declare (ignore labels))
  (let ((list (vm-reg-get state (vm-list-reg inst))))
    (vm-reg-set state (vm-dst inst)
                (if (%vm-managed-cons-pointer-p list)
                    (%vm-managed-cons-car state list)
                    (car list)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-push) state pc labels)
  (declare (ignore labels))
  (let ((item (vm-reg-get state (vm-item-reg inst)))
        (list (vm-reg-get state (vm-list-reg inst))))
    (vm-reg-set state (vm-dst inst) (cons item list))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-null) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst)
                (if (%vm-managed-nil-p value) 1 0))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-endp) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst)
                (if (%vm-managed-nil-p value) 1 0))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-list-length) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst)
                (if (%vm-managed-cons-pointer-p value)
                    (%vm-managed-list-length state value)
                    (list-length value)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-rplacd) state pc labels)
  (declare (ignore labels))
  (let ((cons-val (vm-reg-get state (vm-cons-reg inst)))
        (new-val (vm-reg-get state (vm-val-reg inst))))
    (rplacd (%vm-cow-list-ensure-writable cons-val) new-val)
    (vm-reg-set state (vm-cons-reg inst) cons-val)
    (values (1+ pc) nil nil)))
