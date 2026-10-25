(in-package :cl-cc/vm)

;;; VM string/character instruction macros (comparisons, case conversion,
;;; trim, search, access) and their execute-instruction methods, built on
;;; strings-storage.lisp's COW/taint/SSO layer and
;;; strings-char-classes.lisp's classification predicates (both load
;;; before this file). Character comparison/predicate instructions specific
;;; to characters (as opposed to strings) are in strings-char-instrs.lisp
;;; (loads after).

(eval-when (:load-toplevel :execute)
  (when (fboundp 'vm-register-host-bridge)
    (vm-register-host-bridge 'getenv #'vm-getenv)
    (vm-register-host-bridge 'taint-mark #'taint-mark)
    (vm-register-host-bridge 'tainted-p #'tainted-p)
    (vm-register-host-bridge 'untaint #'untaint)
    (vm-register-host-bridge 'string-freeze #'string-freeze)
    (vm-register-host-bridge 'string-unfreeze #'string-unfreeze)))

(defmacro define-vm-sso-string-comparison-executor (vm-class cl-fn)
  `(defmethod execute-instruction ((inst ,vm-class) state pc labels)
     (declare (ignore labels))
     (vm-reg-set state (vm-dst inst)
                 (,cl-fn (%vm-host-string (vm-reg-get state (vm-str1 inst)))
                         (%vm-host-string (vm-reg-get state (vm-str2 inst)))))
     (values (1+ pc) nil nil)))

(defmacro define-vm-sso-string-unary-executor (vm-class cl-fn)
  `(defmethod execute-instruction ((inst ,vm-class) state pc labels)
     (declare (ignore labels))
     (vm-reg-set state (vm-dst inst)
                 (%vm-maybe-sso-string
                  (,cl-fn (%vm-host-string (vm-reg-get state (vm-src inst))))))
     (values (1+ pc) nil nil)))

(defmacro define-vm-sso-string-destructive-unary-executor (vm-class cl-fn)
  `(defmethod execute-instruction ((inst ,vm-class) state pc labels)
     (declare (ignore labels))
     (let ((string (%vm-host-string (vm-reg-get state (vm-src inst)))))
       (vm-reg-set state (vm-dst inst)
                   (if (stringp string)
                       (,cl-fn string)
                       (,cl-fn (copy-seq string)))))
     (values (1+ pc) nil nil)))

;; All binary string comparisons share the same (dst str1 str2) slot structure.
(defmacro define-vm-string-comparison (name tag docstring)
  `(define-vm-instruction ,name (vm-instruction)
     ,docstring
     (dst nil :reader vm-dst)
     (str1 nil :reader vm-str1)
     (str2 nil :reader vm-str2)
     (:sexp-tag ,tag)
     (:sexp-slots dst str1 str2)))

(define-vm-string-comparison vm-string=           :string=          "Case-sensitive string equality. Returns 1 if equal, 0 otherwise.")
(define-vm-string-comparison vm-string<           :string<          "Case-sensitive string less than. Returns 1 if STR1 < STR2, 0 otherwise.")
(define-vm-string-comparison vm-string>           :string>          "Case-sensitive string greater than. Returns 1 if STR1 > STR2, 0 otherwise.")
(define-vm-string-comparison vm-string<=          :string<=         "Case-sensitive string <=. Returns 1 if STR1 <= STR2, 0 otherwise.")
(define-vm-string-comparison vm-string>=          :string>=         "Case-sensitive string >=. Returns 1 if STR1 >= STR2, 0 otherwise.")
(define-vm-string-comparison vm-string-equal      :string-equal     "Case-insensitive string equality. Returns 1 if equal (ignoring case), 0 otherwise.")
(define-vm-string-comparison vm-string-lessp      :string-lessp     "Case-insensitive string <. Returns 1 if STR1 < STR2 (ignoring case), 0 otherwise.")
(define-vm-string-comparison vm-string-greaterp   :string-greaterp  "Case-insensitive string >. Returns 1 if STR1 > STR2 (ignoring case), 0 otherwise.")
(define-vm-string-comparison vm-string-not-equal  :string-not-equal "Case-sensitive string inequality. Returns 1 if STR1 /= STR2, 0 otherwise.")
(define-vm-string-comparison vm-string-not-greaterp :string-not-greaterp "Case-insensitive string <=. Returns 1 if STR1 <= STR2 (ignoring case), 0 otherwise.")
(define-vm-string-comparison vm-string-not-lessp  :string-not-lessp "Case-insensitive string >=. Returns 1 if STR1 >= STR2 (ignoring case), 0 otherwise.")

(define-vm-unary-instruction vm-string-length :string-length "String length. DST = length of SRC string.")

(define-vm-instruction vm-char (vm-instruction)
  "Character at position. DST = STRING[INDEX]."
  (dst nil :reader vm-dst)
  (string nil :reader vm-string-reg)
  (index nil :reader vm-index)
  (:sexp-tag :char)
  (:sexp-slots dst string index))

(define-vm-unary-instruction vm-char-code :char-code "Character code. DST = ASCII/Unicode code of SRC character.")
(define-vm-unary-instruction vm-code-char :code-char "Character from code. DST = character with code SRC.")

(define-vm-char-comparison vm-char= :char= "Character equality. Returns 1 if CHAR1 equals CHAR2, 0 otherwise.")
(define-vm-char-comparison vm-char< :char< "Character less than. Returns 1 if CHAR1 < CHAR2, 0 otherwise.")

;;; String Manipulation Instructions

(define-vm-instruction vm-subseq (vm-instruction)
  "Substring extraction. DST = STRING[START:END]."
  (dst nil :reader vm-dst)
  (string nil :reader vm-string-reg)
  (start nil :reader vm-start)
  (end nil :reader vm-end)
  (:sexp-tag :subseq)
  (:sexp-slots dst string start end))

(define-vm-instruction vm-concatenate (vm-instruction)
  "String concatenation. DST = STR1 + STR2."
  (dst nil :reader vm-dst)
  (str1 nil :reader vm-str1)
  (str2 nil :reader vm-str2)
  (parts nil :reader vm-parts)
  (:sexp-tag :concatenate)
  (:sexp-slots dst str1 str2 parts))

(define-vm-unary-instruction vm-string-upcase    :string-upcase    "Uppercase conversion. DST = uppercase of SRC.")
(define-vm-unary-instruction vm-string-downcase  :string-downcase  "Lowercase conversion. DST = lowercase of SRC.")
(define-vm-unary-instruction vm-string-capitalize :string-capitalize "Capitalize string. DST = capitalized form of SRC.")
;;; FR-475: Destructive string case operations
(define-vm-unary-instruction vm-nstring-upcase    :nstring-upcase    "Destructive uppercase. Modifies and returns SRC.")
(define-vm-unary-instruction vm-nstring-downcase  :nstring-downcase  "Destructive lowercase. Modifies and returns SRC.")
(define-vm-unary-instruction vm-nstring-capitalize :nstring-capitalize "Destructive capitalize. Modifies and returns SRC.")

(defmacro define-vm-string-trim-instruction (name tag docstring)
  `(define-vm-instruction ,name (vm-instruction)
     ,docstring
     (dst nil :reader vm-dst)
     (char-bag nil :reader vm-char-bag)
     (string nil :reader vm-string-reg)
     (:sexp-tag ,tag)
     (:sexp-slots dst char-bag string)))

(define-vm-string-trim-instruction vm-string-trim       :string-trim       "Trim characters from both ends. DST = STRING with CHAR-BAG chars trimmed from both ends.")
(define-vm-string-trim-instruction vm-string-left-trim  :string-left-trim  "Trim characters from left. DST = STRING with CHAR-BAG chars trimmed from left.")
(define-vm-string-trim-instruction vm-string-right-trim :string-right-trim "Trim characters from right. DST = STRING with CHAR-BAG chars trimmed from right.")

;;; String Search Instructions

(define-vm-instruction vm-search-string (vm-instruction)
  "Search for pattern in string. DST = index of PATTERN in STRING from START, or -1 if not found."
  (dst nil :reader vm-dst)
  (pattern nil :reader vm-pattern)
  (string nil :reader vm-string-reg)
  (start nil :reader vm-start)
  (:sexp-tag :search-string)
  (:sexp-slots dst pattern string start))

;;; Instruction Execution - String Comparisons
;;; Use :binary (pass-through) so ANSI return values are preserved:
;;;   string=, string-equal     → T or NIL
;;;   string<, string>, etc.    → mismatch index (integer) or NIL

(define-vm-sso-string-comparison-executor vm-string= string=)
(define-vm-sso-string-comparison-executor vm-string< string<)
(define-vm-sso-string-comparison-executor vm-string> string>)
(define-vm-sso-string-comparison-executor vm-string<= string<=)
(define-vm-sso-string-comparison-executor vm-string>= string>=)
(define-vm-sso-string-comparison-executor vm-string-equal string-equal)
(define-vm-sso-string-comparison-executor vm-string-lessp string-lessp)
(define-vm-sso-string-comparison-executor vm-string-greaterp string-greaterp)
(define-vm-sso-string-comparison-executor vm-string-not-equal string-not-equal)
(define-vm-sso-string-comparison-executor vm-string-not-greaterp string-not-greaterp)
(define-vm-sso-string-comparison-executor vm-string-not-lessp string-not-lessp)

;;; Instruction Execution - String Access and Query

(defmethod execute-instruction ((inst vm-string-length) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (%vm-string-length (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-char) state pc labels)
  (declare (ignore labels))
  (let* ((string (vm-reg-get state (vm-string-reg inst)))
          (index (vm-reg-get state (vm-index inst)))
          (result (%vm-string-char string index)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(define-simple-instruction vm-char-code :unary char-code)
(define-simple-instruction vm-code-char :unary code-char)

(define-simple-instruction vm-char= :pred2 char= :lhs vm-char1 :rhs vm-char2)
(define-simple-instruction vm-char< :pred2 char< :lhs vm-char1 :rhs vm-char2)

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

(define-vm-sso-string-unary-executor vm-string-upcase string-upcase)
(define-vm-sso-string-unary-executor vm-string-downcase string-downcase)
(define-vm-sso-string-unary-executor vm-string-capitalize string-capitalize)
(define-vm-sso-string-destructive-unary-executor vm-nstring-upcase nstring-upcase)
(define-vm-sso-string-destructive-unary-executor vm-nstring-downcase nstring-downcase)
(define-vm-sso-string-destructive-unary-executor vm-nstring-capitalize nstring-capitalize)

(defmacro define-vm-string-trim-executor (vm-class cl-fn)
  `(defmethod execute-instruction ((inst ,vm-class) state pc labels)
     (declare (ignore labels))
     (vm-reg-set state (vm-dst inst)
                 (%vm-maybe-sso-string
                  (,cl-fn (%vm-host-string (vm-reg-get state (vm-char-bag inst)))
                          (%vm-host-string (vm-reg-get state (vm-string-reg inst))))))
     (values (1+ pc) nil nil)))

(define-vm-string-trim-executor vm-string-trim       string-trim)
(define-vm-string-trim-executor vm-string-left-trim  string-left-trim)
(define-vm-string-trim-executor vm-string-right-trim string-right-trim)

;;; Instruction Execution - String Search

(defmethod execute-instruction ((inst vm-search-string) state pc labels)
  (declare (ignore labels))
  (let* ((pattern (%vm-host-string (vm-reg-get state (vm-pattern inst))))
         (string (%vm-host-string (vm-reg-get state (vm-string-reg inst))))
         (start (vm-reg-get state (vm-start inst)))
         (result (or (search pattern string :start2 start) -1)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

;;; String character mutation (FR-614) — (setf (char s i) v) → (rt-string-set s i v)

(define-vm-instruction vm-string-set (vm-instruction)
  "Set character in string at index. Returns the new character."
  (dst nil :reader vm-dst)
  (str nil :reader vm-str-reg)
  (idx nil :reader vm-idx)
  (val nil :reader vm-val-reg)
  (:sexp-tag :string-set)
  (:sexp-slots dst str idx val))

(defmethod execute-instruction ((inst vm-string-set) state pc labels)
  (declare (ignore labels))
  (let ((v (vm-reg-get state (vm-val-reg inst))))
    (vm-string-set-char (vm-reg-get state (vm-str-reg inst))
                        (vm-reg-get state (vm-idx inst)) v)
    (vm-reg-set state (vm-dst inst) v)
    (values (1+ pc) nil nil)))
