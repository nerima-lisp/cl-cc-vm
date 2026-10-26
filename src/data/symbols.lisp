(in-package #:cl-cc/vm/data)

;;; VM Symbol and Character Instructions
;;;
;;; The host package/symbol operations these instructions dispatch through
;;; are in symbols-packages.lisp (loads before this file).
;;;
;;; This file provides VM instructions for symbol manipulation
;;; (symbol-name, make-symbol, intern, gensym, keywordp) and
;;; character predicates (digit-char-p, alpha-char-p, upper/lower-case-p,
;;; char-upcase/downcase, stringp, characterp, parse-integer, alphanumericp).
;;;

;;; ─── Symbol Manipulation Instructions ───────────────────────────────────────

(define-vm-instruction vm-symbol-name (vm-instruction)
  "Get the name string of a symbol."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :symbol-name)
  (:sexp-slots dst src))

(define-vm-instruction vm-make-symbol (vm-instruction)
  "Create an uninterned symbol from a string."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :make-symbol)
  (:sexp-slots dst src))

(define-vm-instruction vm-intern-symbol (vm-instruction)
  "Intern a string as a symbol. Optional package designator."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (pkg nil :reader vm-intern-pkg)
  (:sexp-tag :intern)
  (:sexp-slots dst src pkg))

(define-vm-instruction vm-add-package-local-nickname (vm-instruction)
  "Add a package-local nickname."
  (dst nil :reader vm-dst)
  (pkg nil :reader vm-local-nickname-pkg)
  (nick nil :reader vm-local-nickname-nick)
  (target nil :reader vm-local-nickname-target)
  (:sexp-tag :add-package-local-nickname)
  (:sexp-slots dst pkg nick target))

(define-vm-instruction vm-remove-package-local-nickname (vm-instruction)
  "Remove a package-local nickname."
  (dst nil :reader vm-dst)
  (pkg nil :reader vm-local-nickname-pkg)
  (nick nil :reader vm-local-nickname-nick)
  (target nil :reader vm-local-nickname-target)
  (:sexp-tag :remove-package-local-nickname)
  (:sexp-slots dst pkg nick target))

(define-vm-instruction vm-find-package (vm-instruction)
  "Find a package by designator through the runtime package registry."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :find-package)
  (:sexp-slots dst src))

(define-vm-instruction vm-find-symbol (vm-instruction)
  "Find a symbol by name in a package designator."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (pkg nil :reader vm-find-symbol-pkg)
  (:sexp-tag :find-symbol)
  (:sexp-slots dst src pkg))

(define-vm-instruction vm-gensym-inst (vm-instruction)
  "Generate a unique uninterned symbol."
  (dst nil :reader vm-dst)
  (:sexp-tag :gensym)
  (:sexp-slots dst))

(define-vm-instruction vm-keywordp (vm-instruction)
  "Test if value is a keyword symbol. Returns 1 if true, 0 otherwise."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :keywordp)
  (:sexp-slots dst src))

(define-simple-instruction vm-symbol-name :unary symbol-name)

(define-simple-instruction vm-make-symbol :unary make-symbol)

(define-simple-instruction vm-keywordp :pred1 keywordp)

;;; ─── Extended Package Operation Instructions (ANSI CL Ch.11) ────────────────

(define-vm-instruction vm-export-inst (vm-instruction)
  "Export symbols from a package."
  (dst nil :reader vm-dst)
  (symbols nil :reader vm-export-symbols)
  (pkg nil :reader vm-export-pkg)
  (:sexp-tag :export)
  (:sexp-slots dst symbols pkg))

(define-vm-instruction vm-import-inst (vm-instruction)
  "Import symbols into a package."
  (dst nil :reader vm-dst)
  (symbols nil :reader vm-import-symbols)
  (pkg nil :reader vm-import-pkg)
  (:sexp-tag :import)
  (:sexp-slots dst symbols pkg))

(define-vm-instruction vm-use-package-inst (vm-instruction)
  "Make packages accessible to another package."
  (dst nil :reader vm-dst)
  (packages-to-use nil :reader vm-use-packages)
  (pkg nil :reader vm-use-pkg)
  (:sexp-tag :use-package)
  (:sexp-slots dst packages-to-use pkg))

(define-vm-instruction vm-unuse-package-inst (vm-instruction)
  "Remove packages from the use-list of another package."
  (dst nil :reader vm-dst)
  (packages-to-unuse nil :reader vm-unuse-packages)
  (pkg nil :reader vm-unuse-pkg)
  (:sexp-tag :unuse-package)
  (:sexp-slots dst packages-to-unuse pkg))

(define-vm-instruction vm-shadow-inst (vm-instruction)
  "Create shadowing symbols in a package."
  (dst nil :reader vm-dst)
  (symbol-names nil :reader vm-shadow-names)
  (pkg nil :reader vm-shadow-pkg)
  (:sexp-tag :shadow)
  (:sexp-slots dst symbol-names pkg))

(define-vm-instruction vm-unintern-inst (vm-instruction)
  "Remove a symbol from its home package."
  (dst nil :reader vm-dst)
  (symbol nil :reader vm-unintern-symbol)
  (pkg nil :reader vm-unintern-pkg)
  (:sexp-tag :unintern)
  (:sexp-slots dst symbol pkg))

(define-vm-instruction vm-package-name-inst (vm-instruction)
  "Get the name of a package."
  (dst nil :reader vm-dst)
  (pkg nil :reader vm-package-name-pkg)
  (:sexp-tag :package-name)
  (:sexp-slots dst pkg))

(define-vm-instruction vm-list-all-packages-inst (vm-instruction)
  "List all registered packages."
  (dst nil :reader vm-dst)
  (:sexp-tag :list-all-packages)
  (:sexp-slots dst))

;;; ─── Character Predicate Instructions ───────────────────────────────────────

;; define-vm-unary-instruction is defined in vm.lisp (available to all vm/ files).

(define-vm-unary-instruction vm-digit-char-p  :digit-char-p  "Test if character is a digit. Returns weight or nil.")
(define-vm-unary-instruction vm-alpha-char-p  :alpha-char-p  "Test if character is alphabetic. Returns 1/0.")
(define-vm-unary-instruction vm-upper-case-p  :upper-case-p  "Test if character is upper case. Returns 1/0.")
(define-vm-unary-instruction vm-lower-case-p  :lower-case-p  "Test if character is lower case. Returns 1/0.")
(define-vm-unary-instruction vm-char-upcase   :char-upcase   "Upcase a character. DST = uppercase of SRC.")
(define-vm-unary-instruction vm-char-downcase :char-downcase "Downcase a character. DST = lowercase of SRC.")
(define-vm-unary-instruction vm-stringp       :stringp       "Test if value is a string. Returns 1/0.")
(define-vm-unary-instruction vm-characterp    :characterp    "Test if value is a character. Returns 1/0.")
(progn
  (define-vm-unary-instruction vm-parse-integer :parse-integer "Parse an integer from a string. DST = integer value.")
  (define-vm-instruction vm-parse-float (vm-instruction)
    "Parse a C floating-point prefix. DST is a double float; values are (float position)."
    (dst nil :reader vm-dst)
    (src nil :reader vm-src)
    (start nil :reader vm-parse-float-start)
    (end nil :reader vm-parse-float-end)
    (:sexp-tag :parse-float)
    (:sexp-slots dst src start end)))
(define-vm-unary-instruction vm-alphanumericp :alphanumericp "Test if character is alphanumeric. Returns 1/0.")

(define-simple-instruction vm-alpha-char-p :pred1 vm-alpha-char-p-value)
(define-simple-instruction vm-upper-case-p :pred1 vm-upper-case-p-value)
(define-simple-instruction vm-lower-case-p :pred1 vm-lower-case-p-value)
(define-simple-instruction vm-char-upcase :unary char-upcase)
(define-simple-instruction vm-char-downcase :unary char-downcase)
(define-simple-instruction vm-stringp :pred1 stringp)
(define-simple-instruction vm-characterp :pred1 characterp)
(progn
  (defmethod execute-instruction ((inst vm-parse-integer) state pc labels)
    (declare (ignore labels))
    (multiple-value-bind (value position)
        (cl:parse-integer (vm-reg-get state (vm-src inst)))
      (setf (vm-values-list state) (list value position))
      (vm-reg-set state (vm-dst inst) value)
      (values (1+ pc) nil nil)))

  (defun %vm-parse-float-ascii-space-p (character)
    (let ((code (char-code character)))
      (or (= code 32) (<= 9 code 13))))
  (defun %vm-parse-float-ascii-upcase-code (character)
    (let ((code (char-code character)))
      (if (<= 97 code 122) (- code 32) code)))

  (defun %vm-parse-float-prefix (string &key (start 0) end)
  (check-type string string)
  (check-type start (integer 0 *))
  (let ((end (or end (length string))))
    (check-type end (integer 0 *))
    (unless (<= start end (length string))
      (error "parse-float bounds are outside the string"))
    (labels ((digit-value (character radix)
               (let ((code (%vm-parse-float-ascii-upcase-code character)))
                 (cond ((<= 48 code 57) (let ((digit (- code 48)))
                                          (and (< digit radix) digit)))
                       ((<= 65 code 70) (let ((digit (+ 10 (- code 65))))
                                          (and (< digit radix) digit))))))
             (scan-digits (position radix)
               (let ((value 0)
                     (count 0)
                     (significant-count 0))
                 (loop while (< position end)
                       for digit = (digit-value (char string position) radix)
                       while digit
                       do (setf value (+ (* value radix) digit))
                          (incf count)
                          (when (or (plusp significant-count) (plusp digit))
                            (incf significant-count))
                          (incf position))
                 (values value position count significant-count)))
             (read-exponent (position marker)
               (if (and (< position end)
                        (= (%vm-parse-float-ascii-upcase-code (char string position)) (%vm-parse-float-ascii-upcase-code marker)))
                   (let ((marker-position position)
                         (cursor (1+ position))
                         (sign 1))
                     (when (and (< cursor end)
                                (or (char= (char string cursor) #\+)
                                    (char= (char string cursor) #\-)))
                       (when (char= (char string cursor) #\-)
                         (setf sign -1))
                       (incf cursor))
                     (multiple-value-bind (value next count) (scan-digits cursor 10)
                       (if (plusp count)
                           (values (* sign value) next t)
                           (values 0 marker-position nil))))
                   (values 0 position nil)))
             (as-double-float (significand power radix negative-p)
               (if (zerop significand)
                   (if negative-p (- 0.0d0) 0.0d0)
                   (let* ((order (if (= radix 10)
                                     (+ (length (write-to-string significand :base 10 :radix nil)) power)
                                     (+ (integer-length significand) power -1)))
                          (overflow-p (> order (if (= radix 10) 309 1023)))
                          (underflow-p (< order (if (= radix 10) -323 -1075))))
                     (cond
                       (overflow-p
                        (if negative-p
                            sb-ext:double-float-negative-infinity
                            sb-ext:double-float-positive-infinity))
                       (underflow-p (if negative-p (- 0.0d0) 0.0d0))
                       (t
                        (let* ((magnitude
                                 (if (= radix 10)
                                     (if (minusp power)
                                         (/ significand (expt 10 (- power)))
                                         (* significand (expt 10 power)))
                                     (if (minusp power)
                                         (/ significand (expt 2 (- power)))
                                         (* significand (expt 2 power)))))
                               (signed (if negative-p (- magnitude) magnitude)))
                          (handler-case
                              (coerce signed 'double-float)
                            (floating-point-overflow ()
                              (if negative-p
                                  sb-ext:double-float-negative-infinity
                                  sb-ext:double-float-positive-infinity))
                            (floating-point-underflow ()
                              (if negative-p (- 0.0d0) 0.0d0)))))))))
             (special-candidate (position negative-p)
  (cond
    ((or (word-at-p position "infinity")
         (word-at-p position "inf"))
     (values (if negative-p
                 sb-ext:double-float-negative-infinity
                 sb-ext:double-float-positive-infinity)
             (+ position (if (word-at-p position "infinity") 8 3))
             t))
    ((word-at-p position "nan")
     (let* ((nan-end (+ position 3))
            (payload-end
              (when (and (< nan-end end)
                         (char= (char string nan-end) #\())
                (cl:position #\) string :start (1+ nan-end) :end end)))
            (nan (sb-kernel:make-double-float #x7FF80000 0)))
       (values (if negative-p (- nan) nan)
               (if payload-end (1+ payload-end) nan-end)
               t)))
    (t (values nil position nil))))
             (word-at-p (position word)
               (and (<= (+ position (length word)) end)
                    (loop for offset below (length word)
                          always (= (%vm-parse-float-ascii-upcase-code (char string (+ position offset))) (%vm-parse-float-ascii-upcase-code (char word offset))))))
             (decimal-candidate (position negative-p)
               (multiple-value-bind (whole cursor whole-count whole-significant)
                   (scan-digits position 10)
                 (let ((fraction 0)
                       (fraction-count 0)
                       (fraction-significant 0))
                   (when (and (< cursor end) (char= (char string cursor) #\.))
                     (multiple-value-setq (fraction cursor fraction-count fraction-significant)
                       (scan-digits (1+ cursor) 10)))
                   (unless (plusp (+ whole-count fraction-count))
                     (error "parse-float found no floating-point prefix"))
                   (let ((exponent 0))
                     (multiple-value-bind (parsed-exponent next consumed-p)
                         (read-exponent cursor #\e)
                       (when consumed-p
                         (setf exponent parsed-exponent
                               cursor next)))
                     (let* ((significand (+ (* whole (expt 10 fraction-count)) fraction))
                            (power (- exponent fraction-count))
                            (significant-count (if (plusp whole-significant) (+ whole-significant fraction-count) fraction-significant))
                            (order (+ significant-count power))
                            (value
                              (cond
                                ((zerop significant-count)
                                 (if negative-p (- 0.0d0) 0.0d0))
                                ((> order 309)
                                 (if negative-p
                                     sb-ext:double-float-negative-infinity
                                     sb-ext:double-float-positive-infinity))
                                ((< order -323)
                                 (if negative-p (- 0.0d0) 0.0d0))
                                (t (as-double-float significand power 10 negative-p)))))
                       (values value cursor t))))))
             (hex-candidate (position negative-p)
  (if (and (<= (+ position 2) end)
           (char= (char string position) #\0)
           (= (%vm-parse-float-ascii-upcase-code (char string (1+ position)))
              (%vm-parse-float-ascii-upcase-code #\x)))
      (multiple-value-bind (whole cursor whole-count)
          (scan-digits (+ position 2) 16)
        (let ((fraction 0)
              (fraction-count 0))
          (when (and (< cursor end) (char= (char string cursor) #\.))
            (multiple-value-setq (fraction cursor fraction-count)
              (scan-digits (1+ cursor) 16)))
          (if (plusp (+ whole-count fraction-count))
              (let ((significand (+ (* whole (expt 16 fraction-count)) fraction))
                    (power (* -4 fraction-count)))
                (multiple-value-bind (exponent next consumed-p)
                    (read-exponent cursor #\p)
                  (when consumed-p
                    (incf power exponent)
                    (setf cursor next)))
                (values (as-double-float significand power 16 negative-p)
                        cursor
                        t))
              (values nil position nil))))
      (values nil position nil)))) (let ((cursor start)
            (negative-p nil))
        (loop while (and (< cursor end)
                         (%vm-parse-float-ascii-space-p (char string cursor)))
              do (incf cursor))
        (when (and (< cursor end)
                   (or (char= (char string cursor) #\+)
                       (char= (char string cursor) #\-)))
          (setf negative-p (char= (char string cursor) #\-))
          (incf cursor))
        (multiple-value-bind (value position success-p)
            (special-candidate cursor negative-p)
          (when success-p
            (return-from %vm-parse-float-prefix (values value position)))
          (multiple-value-bind (value position success-p)
              (hex-candidate cursor negative-p)
            (when success-p
              (return-from %vm-parse-float-prefix (values value position)))
            (multiple-value-bind (value position success-p)
                (decimal-candidate cursor negative-p)
              (if success-p
                  (values value position)
                  (error "parse-float found no floating-point prefix")))))))))

  (defmethod execute-instruction ((inst vm-parse-float) state pc labels)
    (declare (ignore labels))
    (multiple-value-bind (value position)
        (%vm-parse-float-prefix
         (vm-reg-get state (vm-src inst))
         :start (or (vm-parse-float-start inst) 0)
         :end (vm-parse-float-end inst))
      (setf (vm-values-list state) (list value position))
      (vm-reg-set state (vm-dst inst) value)
      (values (1+ pc) nil nil))))
(define-simple-instruction vm-alphanumericp :pred1 vm-alphanumericp-value)

