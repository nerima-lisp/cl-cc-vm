(in-package :cl-cc/vm)

;;; FR-136: ASCII character classification: the bitmask lookup table and the
;;; inlined alpha/digit/upper/lower/alphanumeric/graphic/standard/both-case
;;; predicate functions built on it. The VM instructions that dispatch
;;; through these are in strings.lisp (loads next).

(defconstant +char-class-alpha+ #x01)

(defconstant +char-class-digit+ #x02)

(defconstant +char-class-upper+ #x04)

(defconstant +char-class-lower+ #x08)

(defconstant +char-class-alphanumeric+ #x10)

(defconstant +char-class-graphic+ #x20)

(defconstant +char-class-whitespace+ #x40)

(defconstant +char-class-standard+ #x80)

(defun %make-char-class-table ()
  "Build the immutable-by-convention ASCII character class table."
  (let ((table (make-array 256 :element-type '(unsigned-byte 8) :initial-element 0)))
    (labels ((add-flag (code flag)
               (setf (aref table code) (logior (aref table code) flag)))
             (add-range (start end flag)
               (loop for code from start to end do (add-flag code flag))))
      ;; Standard graphic ASCII characters are space through tilde. Newline is
      ;; the only non-graphic standard character.
      (add-range 32 126 +char-class-graphic+)
      (add-range 32 126 +char-class-standard+)
      (add-flag (char-code #\Newline) +char-class-standard+)
      (dolist (code (mapcar #'char-code '(#\Space #\Tab #\Newline #\Return #\Page)))
        (add-flag code +char-class-whitespace+))
      (add-range (char-code #\0) (char-code #\9)
                 (logior +char-class-digit+ +char-class-alphanumeric+))
      (add-range (char-code #\A) (char-code #\Z)
                 (logior +char-class-alpha+ +char-class-upper+ +char-class-alphanumeric+))
      (add-range (char-code #\a) (char-code #\z)
                 (logior +char-class-alpha+ +char-class-lower+ +char-class-alphanumeric+)))
    table))

(defparameter *char-class-table* (%make-char-class-table)
  "256-byte ASCII character class table for O(1) character predicates.
Each byte encodes +CHAR-CLASS-* flags. Treat as read-only after load.")

(declaim (inline %char-code<=255-p %ascii-char-class-logtest
                 vm-alpha-char-p-value vm-digit-char-p-value
                 vm-upper-case-p-value vm-lower-case-p-value
                 vm-alphanumericp-value vm-graphic-char-p-value
                 vm-standard-char-p-value vm-both-case-p-value))

(defun %char-code<=255-p (code)
  (and (integerp code) (<= 0 code 255)))

(defun %ascii-char-class-logtest (ch flag)
  (let ((code (char-code ch)))
    (and (%char-code<=255-p code)
         (logtest (aref *char-class-table* code) flag))))

(defun vm-alpha-char-p-value (ch)
  (let ((code (char-code ch)))
    (if (%char-code<=255-p code)
        (logtest (aref *char-class-table* code) +char-class-alpha+)
        (alpha-char-p ch))))

(defun vm-digit-char-p-value (ch)
  (let ((code (char-code ch)))
    (if (%char-code<=255-p code)
        (and (logtest (aref *char-class-table* code) +char-class-digit+)
             (- code (char-code #\0)))
        (digit-char-p ch))))

(defun vm-upper-case-p-value (ch)
  (let ((code (char-code ch)))
    (if (%char-code<=255-p code)
        (logtest (aref *char-class-table* code) +char-class-upper+)
        (upper-case-p ch))))

(defun vm-lower-case-p-value (ch)
  (let ((code (char-code ch)))
    (if (%char-code<=255-p code)
        (logtest (aref *char-class-table* code) +char-class-lower+)
        (lower-case-p ch))))

(defun vm-alphanumericp-value (ch)
  (let ((code (char-code ch)))
    (if (%char-code<=255-p code)
        (logtest (aref *char-class-table* code) +char-class-alphanumeric+)
        (alphanumericp ch))))

(defun vm-graphic-char-p-value (ch)
  (let ((code (char-code ch)))
    (if (%char-code<=255-p code)
        (logtest (aref *char-class-table* code) +char-class-graphic+)
        (graphic-char-p ch))))

(defun vm-standard-char-p-value (ch)
  (let ((code (char-code ch)))
    (if (%char-code<=255-p code)
        (logtest (aref *char-class-table* code) +char-class-standard+)
        (standard-char-p ch))))

(defun vm-both-case-p-value (ch)
  (let ((code (char-code ch)))
    (if (%char-code<=255-p code)
        (logtest (aref *char-class-table* code) +char-class-alpha+)
        (both-case-p ch))))
