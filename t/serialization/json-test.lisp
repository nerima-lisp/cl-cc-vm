(in-package #:cl-cc/vm/serialization/test)

(defun register-json-compat-api-tests ()
  nil
  (describe-sequential "JSON compatibility API"
                       (it
                        "parses one value from a string with the established object and array shapes"
                        (let* ((value
                                (cl-cc/vm:json-parse
                                 "{\"value\":1,\"value\":2,\"items\":[null,false]} trailing"))
                               (items (gethash "items" value)))
                          (expect (hash-table-p value) :to-be-truthy)
                          (expect (equal (gethash "value" value) 2)
                                  :to-be-truthy)
                          (expect (vectorp items) :to-be-truthy)
                          (expect (eq (aref items 0) cl-cc/vm:*json-null*)
                                  :to-be-truthy)
                          (expect (null (aref items 1)) :to-be-truthy)))
                       (it
                        "reads one stream value and leaves following input available"
                        (with-input-from-string
                            (stream " {\"value\":true} false")
                          (let ((value (cl-cc/vm:json-parse-stream stream)))
                            (expect (gethash "value" value) :to-be-truthy)
                            (expect (char= (peek-char nil stream) #\Space)
                                    :to-be-truthy)
                            (expect (null (cl-cc/vm:json-parse-stream stream))
                                    :to-be-truthy)))
                        (with-input-from-string (stream "{}{}")
                          (let ((first (cl-cc/vm:json-parse-stream stream))
                                (second (cl-cc/vm:json-parse-stream stream)))
                            (expect (hash-table-p first) :to-be-truthy)
                            (expect (hash-table-p second) :to-be-truthy)
                            (expect (eq (peek-char nil stream nil :eof) :eof)
                                    :to-be-truthy))))
                       (it
                        "stringifies nulls, coerced object keys, and unsupported values"
                        (let ((cl-cc/vm:*json-null* :missing)
                              (object (make-hash-table :test (function equal))))
                          (setf (gethash :answer object) 42)
                          (setf (gethash "1" object) (vector nil
                                                             :missing
                                                             :unsupported))
                          (let* ((parsed
                                  (cl-cc/vm:json-parse
                                   (cl-cc/vm:json-stringify object)))
                                 (items (gethash "1" parsed)))
                            (expect (equal (gethash "ANSWER" parsed) 42)
                                    :to-be-truthy)
                            (expect (eq (aref items 0) :missing) :to-be-truthy)
                            (expect (eq (aref items 1) :missing) :to-be-truthy)
                            (progn
                              (expect (equal (aref items 2) ":UNSUPPORTED")
                                      :to-be-truthy)
                              (expect
                               (equal (cl-cc/vm:json-stringify (list 1 2 3))
                                      "[1,2,3]")
                               :to-be-truthy)))))
                       (it "uses the JSON package's dynamic null sentinel"
                           (let ((json:*json-null* :empty))
                             (expect (eq (json:parse "null") :empty)
                                     :to-be-truthy)
                             (expect (equal (json:stringify :empty) "null")
                                     :to-be-truthy))
                           (let ((cl-cc/vm:*json-null* t))
                             (expect (equal (cl-cc/vm:json-stringify t) "true")
                                     :to-be-truthy))
                           (let ((json:*json-null* t))
                             (expect (equal (json:stringify t) "true")
                                     :to-be-truthy)))
                       (it "writes through the stream API"
                           (let ((encoded
                                  (with-output-to-string (stream)
                                    (cl-cc/vm:json-stringify-stream #(1 nil)
                                                                    stream))))
                             (let ((parsed (cl-cc/vm:json-parse encoded)))
                               (expect (vectorp parsed) :to-be-truthy)
                               (expect (equal (aref parsed 0) 1) :to-be-truthy)
                               (expect (eq (aref parsed 1) cl-cc/vm:*json-null*)
                                       :to-be-truthy))))
                       (it "rejects circular object graphs while stringifying"
                           (let ((value (make-array 1)))
                             (setf (aref value 0) value)
                             (expect
                              (handler-case (progn
                                              (cl-cc/vm:json-stringify value)
                                              nil)
                                (error (condition)
                                  (search "Circular object graphs"
                                          (princ-to-string condition))))
                              :to-be-truthy)))
                       (it
                        "rejects a stream beyond the default JSON input limit"
                        (with-input-from-string
                            (stream
                             (make-string 16777217
                                          :element-type
                                          'cl:base-char
                                          :initial-element
                                          #\Space))
                          (let ((condition
                                 (handler-case (progn
                                                 (cl-cc/vm:json-parse-stream
                                                  stream)
                                                 nil)
                                   (json-kit:json-parse-error (condition)
                                     condition))))
                            (expect (typep condition 'json-kit:json-parse-error)
                                    :to-be-truthy)
                            (expect
                             (and condition
                                  (=
                                   (json-kit:json-parse-error-position
                                    condition)
                                   16777216))
                             :to-be-truthy)
                            (expect
                             (and condition
                                  (string=
                                   (json-kit:json-parse-error-expected
                                    condition)
                                   "input within MAX-INPUT-LENGTH"))
                             :to-be-truthy)
                            (expect (char= (peek-char nil stream) #\Space)
                                    :to-be-truthy))))))
