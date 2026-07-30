(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — Generic Function Dispatch
;;;
;;; Contains: %gethash-multi-key (shared key-lookup helper),
;;; vm-classify-arg, EQL specializer helpers (%eql-specializer-p,
;;; %eql-specializer-matches-p, %vm-extract-eql-specializer-keys,
;;; %vm-gf-eql-methods), vm-get-all-applicable-methods,
;;; %lookup-qualified-methods, %collect-combo-methods,
;;; *method-combination-operators*, %resolve-combination-operator.
;;;
;;; Multi-dispatch resolution (%vm-gf-uses-composite-keys-p, etc.) lives in
;;; vm-dispatch-gf-multi.lisp.
;;; Call dispatch (%vm-dispatch-custom-combination, vm-dispatch-generic-call,
;;; %vm-dispatch-call) lives in vm-dispatch-gf-call.lisp.
;;;
;;; Load order: after vm-dispatch.lisp, before vm-dispatch-gf-multi.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── Generic dispatch helpers ──────────────────────────────────────────────

(defun %gethash-multi-key (ht keys)
  "Try each key form in KEYS against HT; collect all matches into a flat list.
Values stored as lists are spliced in; scalar values are wrapped in a list."
  (loop for key in keys
        for val = (gethash key ht)
        when val nconc (if (listp val) (copy-list val) (list val))))

(defun vm-classify-arg (arg state)
  "Determine the class name of an argument for generic dispatch."
  (let ((class-ht (cond
                    ((hash-table-p arg) (gethash :__class__ arg))
                    ((and (vectorp arg)
                          (plusp (length arg))
                          (hash-table-p (aref arg 0)))
                     (aref arg 0)))))
    (if class-ht
        (let ((metaclass (and state
                              (fboundp '%vm-class-effective-metaclass)
                              (funcall #'%vm-class-effective-metaclass class-ht state))))
          (if (and (fboundp '%vm-standard-metaclass-p)
                   (not (funcall #'%vm-standard-metaclass-p metaclass))
                   (hash-table-p metaclass))
              (gethash :__name__ metaclass)
              (gethash :__name__ class-ht)))
        ;; A *class object* has no :__CLASS__ of its own and so used to classify
        ;; as T. The MOP hands the class itself as the first argument to
        ;; SLOT-VALUE-USING-CLASS and friends, and a method specialising that
        ;; parameter on a custom metaclass must match — otherwise it is registered
        ;; under (meta-b t t) while dispatch computes (t t symbol) and the hook
        ;; never fires. Only a custom metaclass is named here; a standard class
        ;; keeps answering T, which is what everything downstream expects.
        (let ((metaclass (and (hash-table-p arg)
                              (nth-value 1 (gethash :__name__ arg))
                              state
                              (fboundp '%vm-class-effective-metaclass)
                              (funcall #'%vm-class-effective-metaclass arg state))))
          (if (and (hash-table-p metaclass)
                   (fboundp '%vm-standard-metaclass-p)
                   (not (funcall #'%vm-standard-metaclass-p metaclass)))
              (gethash :__name__ metaclass)
              (typecase arg
                ;; Order matters: the most specific ANSI class first, because
                ;; TYPECASE takes the first clause that matches and NULL is a
                ;; SYMBOL, a STRING is a VECTOR, and so on.
                (null 'null)
                (cons 'cons)
                (integer 'integer)
                (ratio 'ratio)
                (float 'float)
                (complex 'complex)
                (character 'character)
                (string 'string)
                (symbol 'symbol)
                ;; HASH-TABLE is deliberately absent. A class object is a hash
                ;; table carrying :__NAME__, and the branch above answers T for
                ;; one with a standard metaclass on purpose -- naming it here
                ;; would classify every class object as HASH-TABLE and break the
                ;; MOP dispatch that relies on the catch-all.
                (function 'function)
                (vector 'vector)
                (array 'array)
                (t t)))))))

;;; ── Built-in class precedence ───────────────────────────────────────────────
;;;
;;; A method specialised on NUMBER has to apply to an integer. Dispatch used to
;;; compare the specializer against VM-CLASSIFY-ARG's answer with EQ, so only an
;;; exact name matched and (defmethod f ((x number))) was reported as having no
;;; applicable method for 42. What was missing is not the comparison but the
;;; ancestry: these are the ANSI class precedence lists for the classes
;;; VM-CLASSIFY-ARG can name, T excluded because the callers test it separately.

(defparameter *vm-builtin-class-precedence*
  '((null      symbol list sequence)
    (cons      list sequence)
    (integer   rational real number)
    (ratio     rational real number)
    (float     real number)
    (complex   number)
    (character)
    (string    vector array sequence)
    (symbol)
    (function)
    (vector    array sequence)
    (array))
  "Alist of (CLASS . PROPER-ANCESTORS) for the built-in classes.

LIST rather than SEQUENCE alone for NULL and CONS, and VECTOR before ARRAY for
STRING, so the order is the ANSI precedence order and not merely the set: a
method on LIST is more specific than one on SEQUENCE and has to win.")

(defun vm-builtin-class-precedence-list (class-name)
  "Return CLASS-NAME followed by its proper ancestors, most specific first."
  (let ((entry (assoc class-name *vm-builtin-class-precedence*)))
    (if entry
        (cons class-name (cdr entry))
        (list class-name))))

(defun vm-specializer-matches-class-p (spec class-name)
  "Return T when a method specialised on SPEC applies to an argument of CLASS-NAME."
  (or (eq spec t)
      (eq spec class-name)
      (and (member spec (vm-builtin-class-precedence-list class-name)) t)))

(defun %eql-specializer-p (key)
  "Return T if KEY is an eql specializer form (eql value)."
  (and (consp key) (eq (car key) 'eql)))

(defun %eql-specializer-matches-p (spec-key arg)
  "Test if an eql specializer key matches ARG.
SPEC-KEY is (eql value) — matches if (eql arg value)."
  (and (%eql-specializer-p spec-key)
       (eql arg (second spec-key))))

(defun %vm-extract-eql-specializer-keys (specializer)
  "Return the eql specializer values embedded in SPECIALIZER.
Only single-argument eql specializers are indexed for fast lookup."
  (cond
    ((%eql-specializer-p specializer)
     (list (second specializer)))
    ((and (consp specializer)
          (= (length specializer) 1)
          (%eql-specializer-p (car specializer)))
     (list (second (car specializer))))
    (t nil)))

(defun %vm-gf-eql-methods (gf-ht first-arg)
  "Return fast-path eql-specializer methods for FIRST-ARG, if indexed."
  (let ((eql-index (and (hash-table-p gf-ht) (gethash :__eql-index__ gf-ht))))
    (when eql-index
      (let ((m (gethash first-arg eql-index)))
        (cond
          ((null m) nil)
          ((listp m) m)
          (t (list m)))))))

(defun %vm-key-has-eql-specializer-p (key)
  "Return T when KEY includes at least one eql specializer." 
  (cond
    ((%eql-specializer-p key) t)
    ((listp key) (some #'%eql-specializer-p key))
    (t nil)))

(defun %vm-specializer-key-matches-args-p (key all-args state)
  "Return T when dispatch KEY applies to ALL-ARGS under STATE classification." 
  (cond
    ((%eql-specializer-p key)
     (and (= (length all-args) 1)
          (%eql-specializer-matches-p key (car all-args))))
    ((listp key)
     (and (= (length key) (length all-args))
          (every (lambda (spec arg)
                   (or (eq spec t)
                       (%eql-specializer-matches-p spec arg)
                       (vm-specializer-matches-class-p spec (vm-classify-arg arg state))))
                 key all-args)))
    (t
     (and (= (length all-args) 1)
          (let ((class-name (vm-classify-arg (car all-args) state)))
            (vm-specializer-matches-class-p key class-name))))))

(defun %vm-arg-cpls (state all-args)
  "Return class-precedence lists for ALL-ARGS, each extended with T." 
  (mapcar (lambda (arg)
            (let* ((class-name (vm-classify-arg arg state))
                   (class-ht (gethash class-name (vm-class-registry state)))
                   (cpl (if class-ht
                            (gethash :__cpl__ class-ht)
                            (list class-name))))
              (if (member t cpl) cpl (append cpl (list t)))))
          all-args))

(defun %vm-dispatch-key-collect (remaining prefix)
  "Generate all dispatch-key tuples from REMAINING CPL lists, prepending PREFIX."
  (if (null remaining)
      (list (reverse prefix))
      (loop for class in (car remaining)
            nconc (%vm-dispatch-key-collect (cdr remaining) (cons class prefix)))))

(defun %vm-dispatch-key-combinations (cpls)
  "Return dispatch-key combinations from CPLs, most-specific first."
  (%vm-dispatch-key-collect cpls nil))

(defun %vm-canonical-dispatch-key (combo)
  "Return the canonical dispatch-table key for COMBO." 
  (if (= (length combo) 1)
      (first combo)
      combo))

(defun %vm-method-value->list (value)
  "Normalize dispatch-table VALUE to a flat method list." 
  (cond
    ((null value) nil)
    ((listp value) (copy-list value))
    ((hash-table-p value) (list value))
    ((functionp value) (list value))
    (t (list value))))

(defun %vm-method-function (method)
  "Extract the callable closure from METHOD (which may be a descriptor hash-table)."
  (if (hash-table-p method)
      (or (gethash :function method)
          (error "Method descriptor missing :function"))
      method))

(defun %vm-single-required-rest-lambda-list-p (lambda-list)
    "Return T for a lambda list with one required parameter followed by &REST."
    (let ((required-count 0)
          (rest-p nil))
      (dolist (entry lambda-list)
        (if (and (symbolp entry)
                 (char= (char (symbol-name entry) 0) #\&))
            (when (string-equal (symbol-name entry) "&REST")
              (setf rest-p t))
            (unless rest-p
              (incf required-count))))
      (and (= required-count 1) rest-p)))

  (defun %vm-collect-applicable-methods
      (methods-ht state all-args &optional scalar-rest-p)
    "Collect applicable methods from METHODS-HT in most-specific-first order."
    (let ((result nil)
          (seen nil))
      (labels ((collect-methods (value)
                 (dolist (method (%vm-method-value->list value))
                   (unless (member method seen)
                     (push method seen)
                     (setf result (append result (list method)))))))
        ;; EQL-specialized methods first.
        (maphash (lambda (key value)
                   (when (and (%vm-key-has-eql-specializer-p key)
                              (%vm-specializer-key-matches-args-p
                               key all-args state))
                     (collect-methods value)))
                 methods-ht)
        ;; Then walk class-precedence combinations in canonical key form.
        (let ((cpls (%vm-arg-cpls state all-args)))
          (dolist (combo (%vm-dispatch-key-combinations cpls))
            (collect-methods
             (gethash (%vm-canonical-dispatch-key combo) methods-ht)))
          (when (and scalar-rest-p (cdr all-args))
            (dolist (class (car cpls))
              (collect-methods (gethash class methods-ht)))))
        result)))

(defun vm-get-all-applicable-methods (gf-ht state all-args)
  "Return applicable primary methods for GF-HT and ALL-ARGS, most-specific first."
  (%vm-collect-applicable-methods
    (when (and (hash-table-p gf-ht)
               (nth-value 1 (gethash :__methods__ gf-ht)))
      (gethash :__methods__ gf-ht))
    state all-args
    (%vm-single-required-rest-lambda-list-p
      (gethash :__lambda-list__ gf-ht))))

(defun %lookup-qualified-methods (gf-ht qual-key state all-arg-values)
  "Look up applicable qualified methods for QUAL-KEY, most-specific first."
  (let ((qual-ht (gethash qual-key gf-ht)))
    (when qual-ht
      (%vm-collect-applicable-methods
        qual-ht state all-arg-values
        (%vm-single-required-rest-lambda-list-p
          (gethash :__lambda-list__ gf-ht))))))

(defun %collect-combo-methods (gf-ht qual-key state all-arg-values)
  "Collect all applicable methods for custom combination by walking the CPL.
Returns methods most-specific-first using the canonical specializer key shape."
  (let ((qual-ht (gethash qual-key gf-ht)))
    (when qual-ht
      (%vm-collect-applicable-methods qual-ht state all-arg-values))))

(defparameter *method-combination-operators*
  `((+      . ,(lambda (&rest results) (apply #'+ (%combination-results results))))
    (*      . ,(lambda (&rest results) (apply #'* (%combination-results results))))
    (list   . ,(lambda (&rest results) (%combination-results results)))
    (append . ,(lambda (&rest results) (apply #'append (%combination-results results))))
    (nconc  . ,(lambda (&rest results) (apply #'nconc (%combination-results results))))
    (max    . ,(lambda (&rest results) (apply #'max (%combination-results results))))
    (min    . ,(lambda (&rest results) (apply #'min (%combination-results results))))
    (and    . ,(lambda (&rest results) (every #'identity (%combination-results results))))
    (or     . ,(lambda (&rest results) (some  #'identity (%combination-results results))))
    (progn  . ,(lambda (&rest results) (car (last (%combination-results results))))))
  "Alist mapping method combination names to fold functions (results→result).")

(defun %combination-results (results)
  "Normalize method-combination arguments for direct and list-based callers."
  (if (and (= (length results) 1) (listp (first results)))
      (first results)
      results))

(defun %resolve-combination-operator (combination)
  "Return the operator function for COMBINATION, or signal an error."
  (cdr (%resolve-combination-operator-entry combination)))

(defun %resolve-combination-operator-entry (combination)
  "Return (TAG . FUNCTION) for COMBINATION in *method-combination-operators*.
TAG is :long-form or NIL (short-form). Signals error if not found."
  (let ((entry (assoc combination *method-combination-operators*)))
    (if entry
        (let ((val (cdr entry)))
          (if (and (consp val) (eq (car val) :long-form))
              val     ;; (:long-form . function)
              (cons nil val)))  ;; short-form: (nil . function)
        (error "Unknown method combination operator: ~S" combination))))
