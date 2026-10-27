(in-package #:cl-cc/vm/data/test)



(defun register-string-builder-tests ()
(it-sequential "string-builder-append-and-finish"
  :timeout
  5
  (let ((builder (cl-cc/vm:make-string-builder :capacity 1)))
    (expect (cl-cc/vm:string-builder-p builder) :to-be-truthy)
    (cl-cc/vm:string-builder-append! builder "ab")
    (cl-cc/vm:string-builder-append! builder #\c)
    (cl-cc/vm:string-builder-append! builder 123)
    (expect (cl-cc/vm:string-builder-finish builder) :to-equal "abc123")))
(it-sequential "with-string-builder-returns-final-string"
  :timeout
  5
  (expect (cl-cc/vm:with-string-builder (out)
                  (cl-cc/vm:string-builder-append! out "hello")
                  (cl-cc/vm:string-builder-append! out #\Space)
                  (cl-cc/vm:string-builder-append! out 'world)) :to-equal "hello world"))
(it-sequential "rope-concat-split-and-flatten"
  :timeout
  5
  (let ((rope (cl-cc/vm:rope-concat "hello" (cl-cc/vm:rope-concat " " "world"))))
    (expect (cl-cc/vm:rope-length rope) :to-equal 11)
    (expect (cl-cc/vm:rope-to-string rope) :to-equal "hello world")
    (multiple-value-bind (left right) (cl-cc/vm:rope-split rope 6)
      (expect (cl-cc/vm:rope-to-string left) :to-equal "hello ")
      (expect (cl-cc/vm:rope-to-string right) :to-equal "world"))))
(it-sequential "rope-short-strings-stay-inline"
  :timeout
  5
  (let ((rope (cl-cc/vm:rope-concat "hello" " world")))
    (expect (stringp (cl-cc/vm/data::rope-root rope)) :to-be-truthy)
    (expect (cl-cc/vm:rope-to-string rope) :to-equal "hello world")))
(it-sequential "rope-long-concat-is-root-node-without-copying-children"
  :timeout
  5
  (let* ((left-text (make-string cl-cc/vm/data::+rope-inline-threshold+ :initial-element #\a))
         (right-text (make-string cl-cc/vm/data::+rope-inline-threshold+ :initial-element #\b))
         (left (cl-cc/vm:rope left-text))
         (right (cl-cc/vm:rope right-text))
         (joined (cl-cc/vm:rope-concat left right))
         (root (cl-cc/vm/data::rope-root joined)))
    (expect (cl-cc/vm/data::rope-node-p root) :to-be-truthy)
    (expect (cl-cc/vm/data::rope-node-left root) :to-be left-text)
    (expect (cl-cc/vm/data::rope-node-right root) :to-be right-text)
    (expect (cl-cc/vm:rope-length joined) :to-equal (* 2 cl-cc/vm/data::+rope-inline-threshold+))))
(it-sequential "string-builder-performance-metrics-are-linear"
  :timeout
  5
  (flet ((test-string-builder-performance (&key iterations chunk)
         (let ((builder (cl-cc/vm:make-string-builder :capacity 1)))
           (loop repeat iterations
                 do (cl-cc/vm:string-builder-append! builder chunk))
           (list :length (cl-cc/vm/data::string-builder-length builder)
                 :capacity-per-character
                 (/ (cl-cc/vm/data::sb-capacity builder) iterations)))))
    (let ((metrics (test-string-builder-performance :iterations 4096 :chunk "x")))
      (expect (getf metrics :length) :to-equal 4096)
      (expect (<= (getf metrics :capacity-per-character) 2) :to-be-truthy))))
)

