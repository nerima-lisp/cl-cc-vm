(in-package #:cl-cc/vm/interop)

(defvar *vm-current-state* nil
  "Dynamically bound to the VM state while invoking host bridge callables.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((package (or (find-package :cffi)
                     (make-package :cffi :use '(:cl)))))
    (dolist (name '("FOREIGN-FUNCALL" "DEFINE-FOREIGN-LIBRARY" "USE-FOREIGN-LIBRARY"))
      (export (intern name package) package))))

(defun %vm-bridge-symbol-aliases (sym)
  "Return same-name symbols that may have been imported through facade packages."
  (let ((name (and (symbolp sym) (symbol-name sym)))
        (result nil))
    (when name
      (dolist (package-name (list *vm-guest-function-package* :cl-cc/vm :cl))
        (let ((package (find-package package-name)))
          (when package
            (multiple-value-bind (candidate status)
                (find-symbol name package)
              (declare (ignore status))
              (when candidate
                (pushnew candidate result :test #'eq)))))))
    (nreverse result)))

(defun vm-bridge-callable (sym)
  "Return the callable registered for SYM, or NIL if none is available."
  (labels ((lookup (candidate)
             (let ((entry (gethash candidate *vm-host-bridge-functions*)))
               (and (functionp entry) entry))))
    (or (lookup sym)
        (dolist (candidate (%vm-bridge-symbol-aliases sym) nil)
          (let ((callable (lookup candidate)))
            (when callable
              (return callable)))))))

(defun %vm-runtime-callable (runtime-symbol-name)
  "Resolve a callable from the VM runtime-callable registry by symbol name."
  (or (gethash runtime-symbol-name *vm-runtime-callables*)
      (error "CL-CC/RUNTIME:~A is unavailable" runtime-symbol-name)))

(defun hash-table-values (table)
  "Return TABLE values as a list preserving host iteration order."
  (let ((result nil))
    (maphash (lambda (k v)
                (declare (ignore k))
                (push v result))
              table)
    (nreverse result)))

(defun %make-hash-table-with-test (test-designator)
  "Construct a native CL hash table honoring TEST-DESIGNATOR when supported.
Accepts symbols/functions for EQ/EQL/EQUAL/EQUALP and falls back to EQL." 
  (let* ((sym (cond ((symbolp test-designator) test-designator)
                    ((functionp test-designator)
                     (or (hash-test-symbol test-designator) 'eql))
                    (t 'eql)))
         (test (resolve-hash-test sym)))
    (make-hash-table :test test)))

(defun %class-slot-initargs-for-slot (class slot-name)
  "Return all initargs in CLASS that initialize SLOT-NAME."
  (let ((result nil)
        (imap (and (hash-table-p class) (gethash :__initargs__ class))))
    (dolist (entry imap)
      (when (eq (cdr entry) slot-name)
        (push (car entry) result)))
    (nreverse result)))

(defun %class-slot-location (class slot-name)
  "Return SLOT-NAME's zero-based effective slot location in CLASS, or NIL."
  (cdr (assoc slot-name
              (and (hash-table-p class)
                   (gethash :__slot-locations__ class))
              :test #'eq)))

(defun %class-slot-metadata (class slot-name)
  "Build a lightweight slot-definition object for SLOT-NAME in CLASS."
  (let ((slot (make-hash-table :test #'eq))
        (initforms (and (hash-table-p class) (gethash :__initforms__ class)))
        (slot-types (and (hash-table-p class) (gethash :__slot-types__ class)))
        (class-slots (and (hash-table-p class) (gethash :__class-slots__ class))))
    (setf (gethash :name slot) slot-name)
    (let ((entry (assoc slot-name initforms)))
      (when entry
        (setf (gethash :initform slot) (cdr entry))))
    (setf (gethash :initargs slot) (%class-slot-initargs-for-slot class slot-name))
    (setf (gethash :type slot) (or (cdr (assoc slot-name slot-types)) t))
    (setf (gethash :allocation slot)
          (if (member slot-name class-slots :test #'eq) :class :instance))
    (let ((location (%class-slot-location class slot-name)))
      (when location
        (setf (gethash :location slot) location)))
    slot))

(defun %class-slot-definitions-from-key (class key)
  "Return public slot-definition objects for slot-name list stored at KEY."
  (let ((slots (and (hash-table-p class) (gethash key class))))
    (when slots
      (mapcar (lambda (slot-name)
                 (%class-slot-metadata class slot-name))
              slots))))

(defun %class-slot-definitions (class &optional (key :__slots__))
  "Return public slot-definition objects for CLASS using metadata list KEY."
  (%class-slot-definitions-from-key class key))

(defun slot-definition-location (slot)
  "Return the zero-based effective slot location for SLOT when available."
  (if (hash-table-p slot)
      (gethash :location slot)
      nil))

(defun slot-definition-initform (slot)
  "Return the initform metadata for SLOT when available."
  (if (hash-table-p slot)
      (gethash :initform slot)
      nil))

(defun slot-definition-initargs (slot)
  "Return the initarg metadata for SLOT when available."
  (if (hash-table-p slot)
      (gethash :initargs slot)
      nil))

(defun slot-definition-type (slot)
  "Return the declared slot type for SLOT, defaulting to T."
  (if (hash-table-p slot)
      (or (gethash :type slot) t)
      t))

(defun slot-definition-allocation (slot)
  "Return the allocation mode for SLOT, defaulting to :INSTANCE."
  (if (hash-table-p slot)
      (or (gethash :allocation slot) :instance)
      :instance))

(defun slot-definition-initfunction (slot)
  "Return a zero-argument function yielding SLOT's initform value."
  (let ((initform (slot-definition-initform slot)))
    (lambda () initform)))

(defun class-metaclass (class)
  "Return CLASS metaclass metadata, defaulting to STANDARD-CLASS."
  (if (hash-table-p class)
      (or (gethash :__metaclass__ class) 'standard-class)
      'standard-class))

(defun compute-effective-slot-definition (class slot-name &optional direct-slots)
  "Return a lightweight effective slot-definition for SLOT-NAME in CLASS."
  (or (and (hash-table-p class)
           (member slot-name (gethash :__slots__ class) :test #'eq)
           (%class-slot-metadata class slot-name))
      (find slot-name direct-slots :key #'slot-definition-name :test #'eq)))

(defun %make-instances-obsolete (class)
  "Mark CLASS metadata obsolete. Existing VM instances migrate when possible."
  (let ((class-ht (if (symbolp class)
                      (and *vm-current-state*
                           (gethash class
                                    (vm-class-registry *vm-current-state*)))
                      class)))
    (when (hash-table-p class-ht)
      (setf (gethash :__obsolete__ class-ht) t))
    class-ht))

(defun %normalize-text-encoding (encoding external-format)
  "Normalize ENCODING / EXTERNAL-FORMAT into the host designator we support."
  (case (or encoding external-format :utf-8)
    (:utf8 :utf-8)
    ((:utf-16 :utf16) :utf-16le)
    (otherwise (or encoding external-format :utf-8))))

(defun string-to-octets (string &key encoding external-format)
  "Encode STRING to octets using the requested host encoding."
  (let ((format-designator (%normalize-text-encoding encoding external-format)))
    (sb-ext:string-to-octets string :external-format format-designator)))

(defun %coerce-octets-vector (octets)
  "Return OCTETS as an (UNSIGNED-BYTE 8) vector."
  (cond
    ((typep octets '(array (unsigned-byte 8) (*))) octets)
    ((vectorp octets)
     (let ((result (make-array (length octets)
                               :element-type '(unsigned-byte 8))))
       (dotimes (i (length octets) result)
         (setf (aref result i) (aref octets i)))))
    ((listp octets)
     (make-array (length octets)
                 :element-type '(unsigned-byte 8)
                 :initial-contents octets))
    (t
     (error "OCTETS-TO-STRING expects a list or vector of octets: ~S" octets))))

(defun octets-to-string (octets &key encoding external-format)
  "Decode OCTETS to a string using the requested host encoding."
  (let ((format-designator (%normalize-text-encoding encoding external-format))
        (octet-vector (%coerce-octets-vector octets)))
    (sb-ext:octets-to-string octet-vector :external-format format-designator)))

(defun %utf-8-byte-length (string)
  "Return the UTF-8 byte length of STRING."
  (length (string-to-octets string :encoding :utf-8)))

(defun %vm-file-string-length (stream object)
  "Return the file string length for OBJECT.
For VM integer handles we ignore STREAM and compute the UTF-8 byte width."
  (declare (ignore stream))
  (typecase object
    (character (%utf-8-byte-length (string object)))
    (string (%utf-8-byte-length object))
    (t
     (error "FILE-STRING-LENGTH currently supports character/string objects only: ~S"
             object))))

(defun %foreign-alien-type-form (type return-type-p)
  "Map a minimal CFFI type keyword to an SBCL alien type form."
  (case type
    (:void (if return-type-p 'sb-alien:void
               (error ":VOID is only valid as a foreign return type")))
    (:string 'sb-alien:c-string)
    (:int 'sb-alien:int)
    (:unsigned-int 'sb-alien:unsigned-int)
    (:short 'sb-alien:short)
    (:unsigned-short 'sb-alien:unsigned-short)
    (:long 'sb-alien:long)
    (:unsigned-long 'sb-alien:unsigned-long)
    (:char 'sb-alien:char)
    (:unsigned-char 'sb-alien:unsigned-char)
    (:float 'sb-alien:float)
    (:double 'sb-alien:double)
    (otherwise (error "Unsupported foreign type: ~S" type))))

(defun %parse-foreign-funcall-arguments (arguments)
  "Split CFFI-style type/value pairs and final return type."
  (let ((items arguments)
        (arg-types nil)
        (arg-values nil))
    (loop
      (cond
        ((null items)
         (return (values (nreverse arg-types) (nreverse arg-values) :void)))
        ((null (cdr items))
         (return (values (nreverse arg-types) (nreverse arg-values) (car items))))
        (t
         (push (car items) arg-types)
         (push (cadr items) arg-values)
         (setf items (cddr items)))))))

(defmethod vm-lookup-bridge-callable ((symbol t)) (vm-bridge-callable symbol))

;;; Instruction Execution - Hash Table Operations
;;; are in hash-execute.lisp (loaded after this file).

(defun resolve-hash-test (test-symbol)
  "Convert TEST-SYMBOL to a canonical hash-test designator."
  (or (and (null test-symbol) 'eql)
      (let ((entry (assoc test-symbol *hash-test-function-alist* :test #'eq)))
        (and entry (cdr entry)))
      (error "Unknown hash table test: ~S" test-symbol)))

(defmethod vm-resolve-hash-test ((test-symbol t)) (resolve-hash-test test-symbol))
