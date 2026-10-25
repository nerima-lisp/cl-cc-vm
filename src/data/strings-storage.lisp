(in-package :cl-cc/vm)

;;; VM string storage: copy-on-write string backing, taint tracking
;;; (taint-mark/tainted-p/untaint), and small-string-optimization (SSO)
;;; interning/decoding. The VM instruction macros built on this storage are
;;; in strings.lisp (loads next).

(defvar *rt-string-dedup-table* nil
  "Weak runtime string interning table mapping string contents to canonical strings.")

(defparameter *taint-mode* nil
  "When true, warn when tainted strings reach SQL, shell, or pathname sinks.")

(defvar *vm-taint-table* (make-hash-table :test #'equal)
  "Fallback taint bitmap for host strings that do not carry VM headers.")

(defstruct (vm-cow-string (:constructor %make-vm-cow-string))
  "VM string wrapper carrying COW, displacement, freeze, and taint header bits."
  (header (list :type-tag :string :cow-flag t :taint-bit nil) :type list)
  (backing "" :type string)
  (start 0 :type fixnum)
  (end 0 :type fixnum)
  (refcount 1 :type integer)
  (frozen-p nil :type boolean))

(defun %vm-cow-string-length (string)
  (- (vm-cow-string-end string) (vm-cow-string-start string)))

(defun vm-string-materialize (value)
  "Return a host string for host, SSO, or VM COW strings."
  (cond
    ((vm-cow-string-p value)
     (subseq (vm-cow-string-backing value)
             (vm-cow-string-start value)
             (vm-cow-string-end value)))
    ((%vm-sso-string-p value) (%vm-decode-sso-string value))
    (t value)))

(defun %vm-string-header-flag (value flag)
  (and (vm-cow-string-p value) (getf (vm-cow-string-header value) flag)))

(defun (setf %vm-string-header-flag) (new-value value flag)
  (when (vm-cow-string-p value)
    (setf (getf (vm-cow-string-header value) flag) new-value))
  new-value)

(defun taint-mark (value &optional (source :external-input))
  "Mark VALUE as tainted and return it."
  (declare (ignore source))
  (cond
    ((vm-cow-string-p value) (setf (%vm-string-header-flag value :taint-bit) t) value)
    ((stringp value) (setf (gethash value *vm-taint-table*) t) value)
    (t value)))

(defun tainted-p (value)
  "Return true when VALUE carries taint."
  (cond
    ((vm-cow-string-p value) (%vm-string-header-flag value :taint-bit))
    ((stringp value) (gethash value *vm-taint-table*))
    (t nil)))

(defun untaint (value)
  "Clear taint from VALUE and return it."
  (cond
    ((vm-cow-string-p value) (setf (%vm-string-header-flag value :taint-bit) nil) value)
    ((stringp value) (remhash value *vm-taint-table*) value)
    (t value)))

(defun vm-check-tainted-usage (value sink)
  "Warn when tainted VALUE flows into SINK while *TAINT-MODE* is enabled."
  (when (and *taint-mode* (tainted-p value)
             (member sink '(:sql :shell :path) :test #'eq))
    (warn "Tainted data used in ~A sink: ~S" sink (vm-string-materialize value)))
  value)

(defun vm-string-copy (string)
  "Return a shallow copy-on-write string wrapper sharing STRING's storage."
  (let ((source (if (vm-cow-string-p string)
                    string
                    (%make-vm-cow-string :backing string :start 0 :end (length string)))))
    (incf (vm-cow-string-refcount source))
    (%make-vm-cow-string :header (copy-list (vm-cow-string-header source))
                         :backing (vm-cow-string-backing source)
                         :start (vm-cow-string-start source)
                         :end (vm-cow-string-end source)
                         :refcount (vm-cow-string-refcount source)
                         :frozen-p (vm-cow-string-frozen-p source))))

(defun vm-string-subseq (string start &optional end)
  "Return a displaced COW substring without copying backing storage."
  (let* ((source (if (vm-cow-string-p string) string (vm-string-copy string)))
         (real-end (or end (%vm-cow-string-length source))))
    (incf (vm-cow-string-refcount source))
    (%make-vm-cow-string :header (copy-list (vm-cow-string-header source))
                         :backing (vm-cow-string-backing source)
                         :start (+ (vm-cow-string-start source) start)
                         :end (+ (vm-cow-string-start source) real-end)
                         :refcount (vm-cow-string-refcount source)
                         :frozen-p (vm-cow-string-frozen-p source))))

(defun %vm-cow-string-ensure-writable (string)
  (when (vm-cow-string-frozen-p string)
    (error "String is frozen: ~S" (vm-string-materialize string)))
  (when (or (> (vm-cow-string-refcount string) 1)
            (/= (vm-cow-string-start string) 0)
            (/= (vm-cow-string-end string) (length (vm-cow-string-backing string))))
    (let ((copy (vm-string-materialize string)))
      (setf (vm-cow-string-backing string) copy
            (vm-cow-string-start string) 0
            (vm-cow-string-end string) (length copy)
            (vm-cow-string-refcount string) 1)))
  string)

(defun vm-string-set-char (string index char)
  "Set CHAR in STRING, resolving COW wrappers before mutation."
  (if (vm-cow-string-p string)
      (let ((writable (%vm-cow-string-ensure-writable string)))
        (vm-check-index (vm-cow-string-backing writable) index 'char)
        (setf (char (vm-cow-string-backing writable) index) char)
        char)
      (progn
        (vm-check-index string index 'char)
        (setf (char string index) char))))

(defun string-freeze (string)
  "Freeze STRING and return a COW string wrapper."
  (let ((result (if (vm-cow-string-p string) string (vm-string-copy string))))
    (setf (vm-cow-string-frozen-p result) t)
    result))

(defun string-unfreeze (string)
  "Unfreeze STRING and return a writable COW string wrapper."
  (let ((result (if (vm-cow-string-p string) string (vm-string-copy string))))
    (setf (vm-cow-string-frozen-p result) nil)
    result))

(defun %rt-ensure-string-dedup-table ()
  "Return the runtime string interning table, creating it lazily."
  (or *rt-string-dedup-table*
      (let ((table (make-hash-table :test 'equal
                                    :weakness :key-and-value)))
        (setf *rt-string-dedup-table* table))))

(defun rt-string-dedup (string)
  "Return the canonical VM string with the same contents as STRING."
  (check-type string string)
  (let ((table (%rt-ensure-string-dedup-table)))
    (multiple-value-bind (canonical found-p) (gethash string table)
      (if (and found-p canonical (string= canonical string))
          canonical
          (setf (gethash string table) string)))))

(defun rt-string-intern (string)
  "Return the canonical weakly interned VM string for STRING's contents."
  (rt-string-dedup string))

(defun %vm-sso-function (name)
  "Return runtime SSO function NAME when the runtime package is loaded."
  (%vm-runtime-function name))

(defun %vm-sso-string-p (value)
  "True when VALUE is a runtime small-string immediate."
  (and (integerp value)
       (typep value '(unsigned-byte 64))
       (let ((predicate (%vm-sso-function "VAL-SSO-STRING-P")))
         (and predicate (funcall predicate value)))))

(defun %vm-decode-sso-string (value)
  "Decode runtime SSO VALUE to a host string."
  (funcall (%vm-sso-function "DECODE-SSO-STRING") value))

(defun %vm-host-string (value)
  "Return VALUE as a host string, decoding SSO immediates as needed."
  (vm-string-materialize value))

(defun %vm-maybe-sso-string (string)
  "In the VM interpreter, return the host string directly.
SSO encoding is reserved for native codegen backends; the interpreter
stores host CL values in registers so tests receive proper strings."
  string)

(defun %vm-string-length (value)
  "Return the length of host or SSO string VALUE."
  (if (%vm-sso-string-p value)
      (logand value #x7)
      (length (vm-string-materialize value))))

(defun %vm-string-char (value index)
  "Return character INDEX from host or SSO string VALUE."
  (if (%vm-sso-string-p value)
      (code-char (ldb (byte 8 (+ 3 (* 8 index))) value))
      (let ((string (vm-string-materialize value)))
        (vm-check-index string index 'char)
        (char string index))))
