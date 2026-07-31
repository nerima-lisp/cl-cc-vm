;;;; src/regex.lisp — guest-visible regex stdlib, backed by cl-regex-kit
;;;;
;;;; Compiles and matches directly through cl-regex-kit's RE2/Rust-compatible
;;;; Thompson-NFA/Pike-VM engine (linear time in the input, regardless of
;;;; pattern), rather than a hand-maintained parser/matcher. cl-regex-kit's
;;;; compiled REGEX objects, MATCH-RESULTs and MATCH-CAPTURES are used
;;;; exactly as it returns them, with no intermediate wrapper type; only the
;;;; guest stdlib's existing return shapes -- (values start end
;;;; groups-vector), (start . end) conses, plain strings -- are
;;;; reconstructed at this boundary, matching what callers of REGEX-SCAN et
;;;; al already expect.

(in-package :cl-cc/vm)

(defun regex-scan (pattern string &key (start 0) end)
  (let ((result (cl-regex-kit:scan (cl-regex-kit:compile-regex pattern) string
                                    :start start :end end)))
    (when result
      (values (cl-regex-kit:match-start result)
              (cl-regex-kit:match-end result)
              (cl-regex-kit:match-captures result string)))))

(defun regex-all-matches (pattern string &key (start 0) end)
  (let ((regex (cl-regex-kit:compile-regex pattern)))
    (mapcar (lambda (result)
              (cons (cl-regex-kit:match-start result) (cl-regex-kit:match-end result)))
            (cl-regex-kit:all-matches regex string :start start :end end))))

(defun regex-replace (pattern string replacement)
  (cl-regex-kit:replace-first (cl-regex-kit:compile-regex pattern) string replacement))

(defun regex-replace-all (pattern string replacement)
  (cl-regex-kit:replace-all (cl-regex-kit:compile-regex pattern) string replacement))

(defun regex-split (pattern string)
  (cl-regex-kit:split (cl-regex-kit:compile-regex pattern) string))
