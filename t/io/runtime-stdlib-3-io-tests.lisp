;;;; runtime-stdlib-3-io-tests.lisp — FR-959/965/1051/1088/1100/1042

(in-package #:cl-cc/vm/io/test)

(defun register-runtime-stdlib-3-io-tests ()
  (it-sequential "stdlib3-external-format-utf8-roundtrip-and-bom"
  (let* ((text "héλ")
         (bytes (cl-cc/vm:vm-encode-string text :utf-8))
         (bom-bytes (concatenate '(vector (unsigned-byte 8)) #(239 187 191) bytes)))
    (expect (typep bytes '(vector (unsigned-byte 8))) :to-be-truthy)
    (expect (cl-cc/vm:vm-decode-bytes bytes :utf-8) :to-equal text)
    (expect (cl-cc/vm:vm-decode-bytes bom-bytes :utf-8) :to-equal text))) (it-sequential "stdlib3-external-format-ascii-error-modes"
  (expect (equalp #(65 63) (cl-cc/vm:vm-encode-string "Aé" :ascii :error-mode :replace)) :to-be-truthy)
  (expect (cl-cc/vm:vm-decode-bytes #(65 255) :ascii :error-mode :ignore) :to-equal "A")) (it-sequential "stdlib3-stream-external-format-side-table"
  (let ((stream (make-string-output-stream)))
    (expect (cl-cc/vm:vm-stream-external-format stream) :to-equal :utf-8)
    (expect (cl-cc/vm:vm-set-stream-external-format stream :utf-16) :to-equal :utf-16le)
    (expect (cl-cc/vm:vm-stream-external-format stream) :to-equal :utf-16le))) (it-sequential "stdlib3-format-p-and-w-directives"
  (expect (cl-cc/vm::%vm-format-native "~D file~:P" '(1)) :to-equal "1 file")
  (expect (cl-cc/vm::%vm-format-native "~D file~:P" '(2)) :to-equal "2 files")
  (expect (cl-cc/vm::%vm-format-native "~D stor~:@P" '(1)) :to-equal "1 story")
  (expect (cl-cc/vm::%vm-format-native "~D stor~:@P" '(2)) :to-equal "2 stories")
  (expect (cl-cc/vm::%vm-format-native "~W" '((:a 1))) :to-equal "(:A 1)")) (it-sequential "stdlib3-print-control-variables"
  (let ((cl-cc/vm:*print-case* :downcase)
        (cl-cc/vm:*print-base* 16)
        (cl-cc/vm:*print-radix* t)
        (cl-cc/vm:*print-array* nil))
    (expect (cl-cc/vm::vm-write-object-to-string 'foo :escape t) :to-equal "foo")
    (expect (string-downcase (cl-cc/vm::vm-write-object-to-string 16 :escape t)) :to-equal "#x10")
    (expect (search "#<" (cl-cc/vm::vm-write-object-to-string #(1 2) :escape t)) :to-be-truthy))))

