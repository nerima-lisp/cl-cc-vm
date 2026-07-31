;;;; t/vm-format-render-test.lisp — native FORMAT directive processor
;;;;
;;;; src/format-render.lisp's %vm-format-render/%vm-format-native had no
;;;; direct test coverage anywhere in this suite. These pin the directives
;;;; actually exercised across the VM's stdlib before %vm-format-render is
;;;; split into per-directive local functions for readability.

(in-package :cl-cc-vm/test)

(defun %render (control &rest args)
  (cl-cc/vm::%vm-format-native control args))

(describe-sequential "native FORMAT directive processor"
  (it "renders ~A as princ and ~S as write"
    (expect (%render "~A" "hi") :to-be-truthy)
    (expect (string= (%render "~A" "hi") "hi") :to-be-truthy)
    (expect (string= (%render "~S" "hi") "\"hi\"") :to-be-truthy))

  (it "renders ~% as newline and ~~ as a literal tilde"
    (expect (string= (%render "a~%b") (format nil "a~%b")) :to-be-truthy)
    (expect (string= (%render "~~") "~") :to-be-truthy))

  (it "renders ~D/~B/~O/~X in the requested radix"
    (expect (string= (%render "~D" 42) "42") :to-be-truthy)
    (expect (string= (%render "~B" 5) "101") :to-be-truthy)
    (expect (string= (%render "~O" 8) "10") :to-be-truthy)
    (expect (string= (%render "~X" 255) "FF") :to-be-truthy))

  (it "renders ~D with comma grouping under :"
    (expect (string= (%render "~:D" 1234567) "1,234,567") :to-be-truthy))

  (it "renders ~C as the character itself"
    (expect (string= (%render "~C" #\z) "z") :to-be-truthy))

  (it "renders ~:P as a pluralizing suffix reusing the previous argument"
    (expect (string= (%render "~D file~:P" 1) "1 file") :to-be-truthy)
    (expect (string= (%render "~D file~:P" 2) "2 files") :to-be-truthy))

  (it "renders ~[...~] as a selector-indexed conditional"
    (expect (string= (%render "~[zero~;one~;two~]" 0) "zero") :to-be-truthy)
    (expect (string= (%render "~[zero~;one~;two~]" 1) "one") :to-be-truthy)
    (expect (string= (%render "~[zero~;one~;two~]" 2) "two") :to-be-truthy))

  (it "renders ~{...~} by iterating a list argument"
    (expect (string= (%render "~{~A,~}" '(1 2 3)) "1,2,3,") :to-be-truthy))

  (it "renders ~<...~> as column-padded right justification"
    (expect (string= (%render "~10<hi~>") "        hi") :to-be-truthy))

  (it "renders ~(...~) as lowercase conversion"
    (expect (string= (%render "~(~A~)" "HI") "hi") :to-be-truthy))

  (it "renders ~^ as an argument-exhaustion escape inside ~{~}"
    (expect (string= (%render "~{~A~^,~}" '(1 2 3)) "1,2,3") :to-be-truthy))

  (it "renders ~T as column-relative tabulation"
    (expect (string= (%render "ab~5,1T|") "ab   |") :to-be-truthy))

  (it "renders ~* as argument-index skipping"
    (expect (string= (%render "~*~A" 1 2) "2") :to-be-truthy)))
