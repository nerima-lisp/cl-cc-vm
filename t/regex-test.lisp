;;;; t/regex-test.lisp — guest-visible regex stdlib, backed by cl-regex-kit
;;;;
;;;; src/regex.lisp stopped hand-rolling a matcher in favor of calling
;;;; cl-regex-kit directly; these pin down the boundary shapes callers of
;;;; REGEX-SCAN et al already depend on (multiple values, cons cells, plain
;;;; strings) now that a different engine produces them.

(in-package :cl-cc-vm/test)

(describe-sequential "guest-visible regex stdlib"
  (it "returns the match bounds and a whole-match-first capture vector"
    (multiple-value-bind (start end groups)
        (cl-cc/vm:regex-scan "(\\d+)-(\\w+)" "order 42-widget shipped")
      (expect (equal start 6) :to-be-truthy)
      (expect (equal end 15) :to-be-truthy)
      (expect (equal (aref groups 0) "42-widget") :to-be-truthy)
      (expect (equal (aref groups 1) "42") :to-be-truthy)
      (expect (equal (aref groups 2) "widget") :to-be-truthy)))

  (it "returns nil when nothing matches"
    (expect (cl-cc/vm:regex-scan "zzz" "abc") :to-be nil))

  (it "finds every non-overlapping match left to right"
    (let ((ranges (cl-cc/vm:regex-all-matches "\\d+" "a1 b22 c333")))
      (expect (equal (length ranges) 3) :to-be-truthy)
      (expect (equal (mapcar (lambda (r) (subseq "a1 b22 c333" (car r) (cdr r))) ranges)
                     '("1" "22" "333"))
              :to-be-truthy)))

  (it "replaces only the first match"
    (expect (equal (cl-cc/vm:regex-replace "\\d+" "id 42 here, id 43 there" "N")
                   "id N here, id 43 there")
            :to-be-truthy))

  (it "replaces every match"
    (expect (equal (cl-cc/vm:regex-replace-all "\\d+" "a1 b22 c333" "#") "a# b# c#")
            :to-be-truthy))

  (it "splits on every match of the pattern, keeping empty fields"
    (expect (equal (cl-cc/vm:regex-split "," "a,b,,c") '("a" "b" "" "c")) :to-be-truthy))

  (it "honors the inline case-insensitive flag"
    (multiple-value-bind (start end) (cl-cc/vm:regex-scan "(?i)hello" "say HELLO now")
      (expect (equal start 4) :to-be-truthy)
      (expect (equal end 9) :to-be-truthy)))

  (it "matches Unicode letter properties"
    (multiple-value-bind (start end) (cl-cc/vm:regex-scan "\\p{L}+" "  abc123  ")
      (expect (equal start 2) :to-be-truthy)
      (expect (equal end 5) :to-be-truthy))))
