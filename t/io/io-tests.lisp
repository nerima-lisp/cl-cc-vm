;;;; tests/unit/vm/io-tests.lisp — VM I/O Operations Unit Tests
;;;;
;;;; Tests for I/O instructions: make-string-output-stream,
;;;; get-output-stream-string, stream-write-string, read-from-string,
;;;; stream predicates, file-handle helpers, and read/write line behavior.

(in-package #:cl-cc/vm/io/test)

(defun register-io-tests ()
  (it-sequential "io-print-circle-circular-list"
  (let ((x (list 'a)))
    (setf (cdr x) x)
    (let ((printed (cl-cc/vm/io::vm-write-object-to-string x :circle t)))
      (expect (search "#0=" printed) :to-be-truthy)
      (expect (search "#0#" printed) :to-be-truthy)))) (it-sequential "io-print-circle-shared-vector"
  (let* ((shared (list 1 2))
         (vec (vector shared shared))
         (printed (cl-cc/vm/io::vm-write-object-to-string vec :circle t)))
    (expect (search "#0=" printed) :to-be-truthy)
    (expect (search "#0#" printed) :to-be-truthy))) (defun io-vm (&optional (out (make-string-output-stream)))
  "Create a vm-io-state with a string output stream for capture."
  (cl-cc/vm/execution:make-vm-state :output-stream out)) (defun io-vm-full (&optional (out (make-string-output-stream)))
  "Create a vm-io-state (with file handle management) for IO tests."
  (cl-cc/vm/execution:make-vm-state :output-stream out)) (defun io-exec (inst state)
  "Execute a single instruction against STATE."
  (cl-cc/vm/protocols:execute-instruction inst state 0 (make-hash-table :test #'equal))) (it-sequential "io-string-output-stream-roundtrip"
  (let ((s (io-vm)))
    ;; Create string output stream
    (io-exec (cl-cc/vm:make-vm-make-string-output-stream-inst :dst :R0) s)
    (let ((stream (cl-cc/vm/protocols:vm-reg-get s :R0)))
      (expect (streamp stream) :to-be-truthy)
      ;; Write string to it
      (cl-cc/vm/protocols:vm-reg-set s :R1 stream)
      (cl-cc/vm/protocols:vm-reg-set s :R2 "hello")
      (io-exec (cl-cc/vm:make-vm-stream-write-string-inst :stream-reg :R1 :src :R2) s)
      ;; Get accumulated string
      (io-exec (cl-cc/vm:make-vm-get-output-stream-string-inst :dst :R3 :src :R1) s)
      (expect (cl-cc/vm/protocols:vm-reg-get s :R3) :to-equal "hello")))) (it-sequential "io-string-output-stream-multiple-writes"
  (let ((s (io-vm)))
    (io-exec (cl-cc/vm:make-vm-make-string-output-stream-inst :dst :R0) s)
    (let ((stream (cl-cc/vm/protocols:vm-reg-get s :R0)))
      (cl-cc/vm/protocols:vm-reg-set s :R1 stream)
      (cl-cc/vm/protocols:vm-reg-set s :R2 "hello")
      (io-exec (cl-cc/vm:make-vm-stream-write-string-inst :stream-reg :R1 :src :R2) s)
      (cl-cc/vm/protocols:vm-reg-set s :R2 " world")
      (io-exec (cl-cc/vm:make-vm-stream-write-string-inst :stream-reg :R1 :src :R2) s)
      (io-exec (cl-cc/vm:make-vm-get-output-stream-string-inst :dst :R3 :src :R1) s)
      (expect (cl-cc/vm/protocols:vm-reg-get s :R3) :to-equal "hello world")))) (defun %io-read-str (src)
  "Execute vm-read-from-string on SRC in a fresh vm-state and return the result."
  (let ((vm (io-vm)))
    (cl-cc/vm/protocols:vm-reg-set vm :R1 src)
    (io-exec (cl-cc/vm:make-vm-read-from-string-inst :dst :R0 :src :R1) vm)
    (cl-cc/vm/protocols:vm-reg-get vm :R0))) (it-sequential "io-read-from-string-integer"
  (expect (%io-read-str "42") :to-equal 42)) (it-sequential "io-read-from-string-symbol"
  (expect (symbol-name (%io-read-str "hello")) :to-equal "HELLO")) (it-sequential "io-read-from-string-list"
  (let ((result (%io-read-str "(1 2 3)")))
    (expect (listp result) :to-be-truthy)
    (expect (cl-cc/vm/data:length result) :to-equal 3))) (it-sequential "io-read-from-string-empty"
  (expect (%io-read-str "") :to-be-null)) (it-sequential "io-allocate-handle-sequence"
  (let ((s (io-vm-full)))
    (let ((h1 (cl-cc/vm/io:vm-allocate-file-handle s))
          (h2 (cl-cc/vm/io:vm-allocate-file-handle s))
          (h3 (cl-cc/vm/io:vm-allocate-file-handle s)))
      (expect h1 :to-equal 2)
      (expect h2 :to-equal 3)
      (expect h3 :to-equal 4)))) (it-sequential "io-get-stream-std-handles stdin"
  (destructuring-bind (accessor handle) (list #'cl-cc/vm/io:vm-standard-input cl-cc/vm/io:+stdin-handle+)
    (let ((s (io-vm-full)))
    (expect (cl-cc/vm/io:vm-get-stream s handle) :to-equal (funcall accessor s))))) (it-sequential "io-get-stream-std-handles stdout"
  (destructuring-bind (accessor handle) (list #'cl-cc/vm/io:vm-standard-output cl-cc/vm/io:+stdout-handle+)
    (let ((s (io-vm-full)))
    (expect (cl-cc/vm/io:vm-get-stream s handle) :to-equal (funcall accessor s))))) (it-sequential "io-get-stream-cl-stream-passthrough"
  (let ((s (io-vm-full))
        (stream (make-string-output-stream)))
    (expect (cl-cc/vm/io:vm-get-stream s stream) :to-equal stream))) (it-sequential "io-get-stream-handle-map-lookup-cases open-files"
  (destructuring-bind (register-fn) (list (lambda (s h stream) (setf (gethash h (cl-cc/vm/io:vm-open-files s)) stream)))
    (let* ((s      (io-vm-full))
         (stream (make-string-output-stream))
         (handle (cl-cc/vm/io:vm-allocate-file-handle s)))
    (funcall register-fn s handle stream)
    (expect (cl-cc/vm/io:vm-get-stream s handle) :to-equal stream)))) (it-sequential "io-get-stream-handle-map-lookup-cases string-streams"
  (destructuring-bind (register-fn) (list (lambda (s h stream) (setf (gethash h (cl-cc/vm/io:vm-string-streams s)) stream)))
    (let* ((s      (io-vm-full))
         (stream (make-string-output-stream))
         (handle (cl-cc/vm/io:vm-allocate-file-handle s)))
    (funcall register-fn s handle stream)
    (expect (cl-cc/vm/io:vm-get-stream s handle) :to-equal stream)))) (it-sequential "io-get-stream-invalid-handle-error"
  (let ((s (io-vm-full)))
    (signals error (cl-cc/vm/io:vm-get-stream s 999)))) (it-sequential "io-stream-open-p-standard-handles stdin"
  (destructuring-bind (handle) (list cl-cc/vm/io:+stdin-handle+)
    (let ((s (io-vm-full)))
    (expect (cl-cc/vm/io:vm-stream-open-p s handle) :to-be-truthy)))) (it-sequential "io-stream-open-p-standard-handles stdout"
  (destructuring-bind (handle) (list cl-cc/vm/io:+stdout-handle+)
    (let ((s (io-vm-full)))
    (expect (cl-cc/vm/io:vm-stream-open-p s handle) :to-be-truthy)))) (it-sequential "io-stream-open-p-unknown-handle-returns-nil"
  (let ((s (io-vm-full)))
    (expect (cl-cc/vm/io:vm-stream-open-p s 999) :to-equal nil))) (it-sequential "io-stream-open-p-direct-cl-stream-returns-truthy"
  (let ((s (io-vm-full)))
    (expect (cl-cc/vm/io:vm-stream-open-p s (make-string-output-stream)) :to-be-truthy))) (it-sequential "io-streamp cl-stream"
  (destructuring-bind (value expected) (list (make-string-output-stream) t)
    (let ((s (io-vm-full)))
    (cl-cc/vm/protocols:vm-reg-set s :R1 value)
    (io-exec (cl-cc/vm:make-vm-streamp :dst :R0 :src :R1) s)
    (expect (cl-cc/vm/protocols:vm-reg-get s :R0) :to-equal expected)))) (it-sequential "io-streamp non-stream"
  (destructuring-bind (value expected) (list 42 nil)
    (let ((s (io-vm-full)))
    (cl-cc/vm/protocols:vm-reg-set s :R1 value)
    (io-exec (cl-cc/vm:make-vm-streamp :dst :R0 :src :R1) s)
    (expect (cl-cc/vm/protocols:vm-reg-get s :R0) :to-equal expected)))) (it-sequential "io-streamp-handle-resolved"
  (let ((s (io-vm-full)))
    (cl-cc/vm/protocols:vm-reg-set s :R1 cl-cc/vm/io:+stdin-handle+)
    (io-exec (cl-cc/vm:make-vm-streamp :dst :R0 :src :R1) s)
    (expect (cl-cc/vm/protocols:vm-reg-get s :R0) :to-equal t))) (it-sequential "io-input-stream-p input-stream"
  (destructuring-bind (stream-val expected) (list (make-string-input-stream "hello") t)
    (let ((s (io-vm-full)))
    (cl-cc/vm/protocols:vm-reg-set s :R1 stream-val)
    (io-exec (cl-cc/vm:make-vm-input-stream-p :dst :R0 :src :R1) s)
    (expect (cl-cc/vm/protocols:vm-reg-get s :R0) :to-equal expected)))) (it-sequential "io-input-stream-p output-stream"
  (destructuring-bind (stream-val expected) (list (make-string-output-stream) nil)
    (let ((s (io-vm-full)))
    (cl-cc/vm/protocols:vm-reg-set s :R1 stream-val)
    (io-exec (cl-cc/vm:make-vm-input-stream-p :dst :R0 :src :R1) s)
    (expect (cl-cc/vm/protocols:vm-reg-get s :R0) :to-equal expected)))) (it-sequential "io-output-stream-p output-stream"
  (destructuring-bind (stream-val expected) (list (make-string-output-stream) t)
    (let ((s (io-vm-full)))
    (cl-cc/vm/protocols:vm-reg-set s :R1 stream-val)
    (io-exec (cl-cc/vm:make-vm-output-stream-p :dst :R0 :src :R1) s)
    (expect (cl-cc/vm/protocols:vm-reg-get s :R0) :to-equal expected)))) (it-sequential "io-output-stream-p input-stream"
  (destructuring-bind (stream-val expected) (list (make-string-input-stream "x") nil)
    (let ((s (io-vm-full)))
    (cl-cc/vm/protocols:vm-reg-set s :R1 stream-val)
    (io-exec (cl-cc/vm:make-vm-output-stream-p :dst :R0 :src :R1) s)
    (expect (cl-cc/vm/protocols:vm-reg-get s :R0) :to-equal expected)))) (it-sequential "io-open-stream-p-true"
  (let ((s (io-vm-full)))
    (cl-cc/vm/protocols:vm-reg-set s :R1 (make-string-output-stream))
    (io-exec (cl-cc/vm:make-vm-open-stream-p :dst :R0 :src :R1) s)
    (expect (cl-cc/vm/protocols:vm-reg-get s :R0) :to-equal t))) (it-sequential "io-make-string-stream-output"
  (let ((s (io-vm-full)))
    (io-exec (cl-cc/vm:make-vm-make-string-stream :dst :R0 :direction :output) s)
    (let ((handle (cl-cc/vm/protocols:vm-reg-get s :R0)))
      (expect (integerp handle) :to-be-truthy)
      (expect (>= handle 2) :to-be-truthy)))) (it-sequential "io-make-string-stream-input"
  (let ((s (io-vm-full)))
    (cl-cc/vm/protocols:vm-reg-set s :R1 "hello")
    (io-exec (cl-cc/vm:make-vm-make-string-stream :dst :R0 :direction :input
                                                 :initial-string :R1) s)
    (let ((handle (cl-cc/vm/protocols:vm-reg-get s :R0)))
      (expect (integerp handle) :to-be-truthy)
      ;; Read a char from it to verify content
      (cl-cc/vm/protocols:vm-reg-set s :R2 handle)
      (io-exec (cl-cc/vm:make-vm-read-char :dst :R3 :handle :R2) s)
      (expect (cl-cc/vm/protocols:vm-reg-get s :R3) :to-equal #\h)))) (it-sequential "io-make-string-stream-get-string"
  (let ((s (io-vm-full)))
    ;; Create output string stream (handle-based)
    (io-exec (cl-cc/vm:make-vm-make-string-stream :dst :R0 :direction :output) s)
    (let ((handle (cl-cc/vm/protocols:vm-reg-get s :R0)))
      ;; Write to it via handle
      (cl-cc/vm/protocols:vm-reg-set s :R1 handle)
      (cl-cc/vm/protocols:vm-reg-set s :R2 "test output")
      (io-exec (cl-cc/vm:make-vm-write-string :handle :R1 :str :R2) s)
      ;; Get string from handle
      (io-exec (cl-cc/vm:make-vm-get-string-from-stream :dst :R3 :handle :R1) s)
      (expect (cl-cc/vm/protocols:vm-reg-get s :R3) :to-equal "test output")))) (it-sequential "io-eof-p eof"
  (destructuring-bind (value expected) (list :eof 1)
    (let ((s (io-vm-full)))
    (cl-cc/vm/protocols:vm-reg-set s :R1 value)
    (io-exec (cl-cc/vm:make-vm-eof-p :dst :R0 :value :R1) s)
    (expect (cl-cc/vm/protocols:vm-reg-get s :R0) :to-equal expected)))) (it-sequential "io-eof-p non-eof"
  (destructuring-bind (value expected) (list #\a 0)
    (let ((s (io-vm-full)))
    (cl-cc/vm/protocols:vm-reg-set s :R1 value)
    (io-exec (cl-cc/vm:make-vm-eof-p :dst :R0 :value :R1) s)
    (expect (cl-cc/vm/protocols:vm-reg-get s :R0) :to-equal expected)))) (it-sequential "io-read-line-from-string-stream"
  (let ((s (io-vm-full)))
    (cl-cc/vm/protocols:vm-reg-set s :R1 "first line")
    (io-exec (cl-cc/vm:make-vm-make-string-stream :dst :R0 :direction :input
                                                 :initial-string :R1) s)
    (let ((handle (cl-cc/vm/protocols:vm-reg-get s :R0)))
      (cl-cc/vm/protocols:vm-reg-set s :R2 handle)
      (io-exec (cl-cc/vm:make-vm-read-line :dst :R3 :handle :R2) s)
      (expect (cl-cc/vm/protocols:vm-reg-get s :R3) :to-equal "first line")))) (it-sequential "resolve-integer-stream-handle-stdin"
  (let ((s   (io-vm-full))
        (h   cl-cc/vm/io:+stdin-handle+))
    (expect (cl-cc/vm/io::%resolve-integer-stream-handle h s) :to-be (cl-cc/vm/io:vm-standard-input s)))) (it-sequential "resolve-integer-stream-handle-stdout"
  (let ((s (io-vm-full))
        (h cl-cc/vm/io:+stdout-handle+))
    (expect (cl-cc/vm/io::%resolve-integer-stream-handle h s) :to-be (cl-cc/vm/io:vm-standard-output s)))) (it-sequential "resolve-integer-stream-handle-unknown-returns-nil"
  (let ((s (io-vm-full)))
    (expect (cl-cc/vm/io::%resolve-integer-stream-handle 9999 s) :to-be-null))) (it-sequential "copy-ht-into-copies-all-entries"
  (let ((src (make-hash-table :test #'eq))
        (dst (make-hash-table :test #'eq)))
    (setf (gethash :a src) 1
          (gethash :b src) 2
          (gethash :old dst) 99)
    (cl-cc/vm/io::%copy-ht-into src dst)
    (expect (= 1 (gethash :a dst)) :to-be-truthy)
    (expect (= 2 (gethash :b dst)) :to-be-truthy)
    (expect (gethash :old dst) :to-be-null)
    (expect (= 2 (hash-table-count dst)) :to-be-truthy))) (it-sequential "copy-ht-into-empty-src-clears-dst"
  (let ((src (make-hash-table :test #'eq))
        (dst (make-hash-table :test #'eq)))
    (setf (gethash :x dst) 42)
    (cl-cc/vm/io::%copy-ht-into src dst)
    (expect (= 0 (hash-table-count dst)) :to-be-truthy))) (it-sequential "clone-vm-state-copies-global-vars"
  (let ((source (cl-cc/vm/execution:make-vm-state)))
    (setf (gethash 'x (cl-cc/vm/execution:vm-global-vars source)) 42)
    (let ((clone (cl-cc/vm/io:clone-vm-state source)))
      (expect (gethash 'x (cl-cc/vm/execution:vm-global-vars clone)) :to-equal 42)
      (setf (gethash 'x (cl-cc/vm/execution:vm-global-vars clone)) 99)
      (expect (gethash 'x (cl-cc/vm/execution:vm-global-vars source)) :to-equal 42)))) (it-sequential "io-file-position-set-returns-boolean"
  (let ((s (io-vm-full)))
    (cl-cc/vm/protocols:vm-reg-set s :contents "abcdef")
    (io-exec (cl-cc/vm:make-vm-make-string-stream :dst :stream :direction :input
                                                :initial-string :contents) s)
    (flet ((seek (position)
             (cl-cc/vm/protocols:vm-reg-set s :new-position position)
             (io-exec (cl-cc/vm:make-vm-file-position :dst :ok :handle :stream
                                                    :position :new-position) s)
             (cl-cc/vm/protocols:vm-reg-get s :ok))
           (stream-position ()
             (io-exec (cl-cc/vm:make-vm-file-position :dst :position :handle :stream) s)
             (cl-cc/vm/protocols:vm-reg-get s :position)))
      (expect (stream-position) :to-equal 0)
      (expect (seek 3) :to-equal t)
      (io-exec (cl-cc/vm:make-vm-read-char :dst :character :handle :stream) s)
      (expect (cl-cc/vm/protocols:vm-reg-get s :character) :to-equal #\d)
      (expect (seek :current) :to-equal t)
      (expect (stream-position) :to-equal 4)
      (expect (seek :start) :to-equal t)
      (io-exec (cl-cc/vm:make-vm-read-char :dst :character :handle :stream) s)
      (expect (cl-cc/vm/protocols:vm-reg-get s :character) :to-equal #\a)
      (expect (seek :end) :to-equal t)
      (expect (stream-position) :to-equal 6)))) (it-sequential "io-file-length-binary-stream"
  (let ((path (merge-pathnames (format nil "clcc-io-file-length-~A.bin" (gensym))
                               (uiop:temporary-directory)))
        (large-position (+ (ash 1 31) 17))
        (s (io-vm-full)))
    (unwind-protect
         (with-open-file (stream path :direction :io :element-type '(unsigned-byte 8)
                                :if-exists :supersede :if-does-not-exist :create)
           (write-byte 1 stream)
           (write-byte 2 stream)
           (write-byte 3 stream)
           (finish-output stream)
           (cl-cc/vm/protocols:vm-reg-set s :stream stream)
           (flet ((seek (position)
                    (cl-cc/vm/protocols:vm-reg-set s :new-position position)
                    (io-exec (cl-cc/vm:make-vm-file-position :dst :ok :handle :stream
                                                           :position :new-position) s)
                    (cl-cc/vm/protocols:vm-reg-get s :ok))
                  (stream-position ()
                    (io-exec (cl-cc/vm:make-vm-file-position :dst :position
                                                          :handle :stream) s)
                    (cl-cc/vm/protocols:vm-reg-get s :position)))
             (expect (seek :start) :to-equal t)
             (expect (stream-position) :to-equal 0)
             (expect (seek :end) :to-equal t)
             (expect (stream-position) :to-equal 3)
             (expect (seek :current) :to-equal t)
             (expect (stream-position) :to-equal 3)
             (expect (seek large-position) :to-equal t)
             (write-byte 255 stream)
             (finish-output stream)
             (expect (stream-position) :to-equal (1+ large-position))
             (io-exec (cl-cc/vm:make-vm-file-length :dst :length :handle :stream) s)
             (expect (cl-cc/vm/protocols:vm-reg-get s :length) :to-equal (1+ large-position))
             (expect (seek large-position) :to-equal t)
             (expect (read-byte stream) :to-equal 255)))
      (when (probe-file path) (delete-file path))))) (it-sequential "io-make-buffered-stream-validates-options-and-flushes"
  (let* ((raw (make-string-output-stream))
         (stream (cl-cc/vm/io::make-buffered-stream raw :buffer-size 16 :strategy :line)))
    (expect stream :to-be raw)
    (write-string "abc" stream)
    (force-output stream)
    (finish-output stream)
    (expect (get-output-stream-string stream) :to-equal "abc"))) (it-sequential "io-stream-query-instructions-on-handle"
  (let ((s (io-vm-full)))
    (io-exec (cl-cc/vm:make-vm-make-string-stream :dst :handle :direction :input
                                                :initial-string nil) s)
    (io-exec (cl-cc/vm:make-vm-stream-element-type-inst :dst :type :src :handle) s)
    (io-exec (cl-cc/vm:make-vm-open-stream-p :dst :open :src :handle) s)
    (io-exec (cl-cc/vm:make-vm-interactive-stream-p :dst :interactive :src :handle) s)
    (expect (cl-cc/vm/protocols:vm-reg-get s :type) :to-equal 'character)
    (expect (cl-cc/vm/protocols:vm-reg-get s :open) :to-equal t)
    (expect (cl-cc/vm/protocols:vm-reg-get s :interactive) :to-equal nil))) (it-sequential "io-special-string-stream-bridge"
  (let ((stream (make-string-output-stream)))
    (write-string "hello" stream)
    (expect (cl-cc/vm/io::%vm-bridge-get-output-stream-string stream) :to-equal "hello"))) (it-sequential "io-special-composite-streams"
  (let* ((out-a (make-string-output-stream))
         (out-b (make-string-output-stream))
         (broadcast (cl-cc/vm/io::%vm-bridge-make-broadcast-stream out-a out-b))
         (two-way (cl-cc/vm/io::%vm-bridge-make-two-way-stream
                   (make-string-input-stream "x") broadcast))
         (echo (cl-cc/vm/io::%vm-bridge-make-echo-stream
                (make-string-input-stream "y") broadcast))
         (concat (cl-cc/vm/io::%vm-bridge-make-concatenated-stream
                  (make-string-input-stream "ab")
                  (make-string-input-stream "cd")))
         (*standard-output* broadcast)
         (synonym (cl-cc/vm/io::%vm-bridge-make-synonym-stream '*standard-output*)))
    (write-char #\A two-way)
    (expect (read-char echo) :to-equal #\y)
    (write-char #\B synonym)
    (expect (loop repeat 4 collect (read-char concat) into chars
                                finally (return (coerce chars 'string))) :to-equal "abcd")
    (finish-output broadcast)
    (expect (get-output-stream-string out-a) :to-equal "AyB")
    (expect (get-output-stream-string out-b) :to-equal "AyB"))) (it-sequential "io-pathname-operations-host-backed"
  (let* ((root (uiop:ensure-directory-pathname
                (merge-pathnames (format nil "clcc-path-fr927-~A/" (gensym))
                                 (uiop:temporary-directory))))
         (file (merge-pathnames "alpha.txt" root))
         (wild (merge-pathnames "*.txt" root)))
    (unwind-protect
         (progn
           (ensure-directories-exist file)
           (with-open-file (out file :direction :output :if-exists :supersede :if-does-not-exist :create)
             (write-string "x" out))
           (let* ((pn (make-pathname :directory (pathname-directory root)
                                     :name "alpha" :type "txt"))
                  (merged (merge-pathnames "alpha.txt" root))
                  (parsed (parse-namestring (namestring file))))
             (expect (pathname-directory pn) :to-equal (pathname-directory root))
             (expect (pathname-name pn) :to-equal "alpha")
             (expect (pathname-type pn) :to-equal "txt")
             (expect (namestring merged) :to-equal (namestring file))
             (expect (file-namestring parsed) :to-equal "alpha.txt")
             (expect (plusp (cl-cc/vm/data:length (directory-namestring parsed))) :to-be-truthy)
             (expect (enough-namestring file root) :to-equal "alpha.txt")
             (expect (wild-pathname-p wild) :to-be-truthy)
             (expect (pathname-match-p file wild) :to-be-truthy)
             (expect (= 1 (cl-cc/vm/data:length (directory wild))) :to-be-truthy)))
      (when (probe-file file) (delete-file file))
      (when (probe-file root) (uiop:delete-directory-tree root :validate t))))) (it-sequential "vm-opcodes-defs-make-vm-state-returns-vm-io-state"
                 (let ((s (cl-cc/vm/execution:make-vm-state)))
                   (expect (typep s 'cl-cc/vm/io:vm-io-state) :to-be-truthy))) (it-sequential "vm-opcodes-defs-make-vm-state-defaults-to-standard-output"
                 (let ((s (cl-cc/vm/execution:make-vm-state)))
                   (expect (cl-cc/vm/io:vm-standard-output s) :to-be *standard-output*))) (it-sequential "vm-opcodes-defs-make-vm-state-accepts-custom-output-stream"
                 (let* ((str (make-string-output-stream))
                        (s   (cl-cc/vm/execution:make-vm-state :output-stream str)))
                   (expect (cl-cc/vm/io:vm-standard-output s) :to-be str))) (it-sequential "vm-opcodes-defs-vm-standard-output-reflects-custom-stream"
                 (let* ((str (make-string-output-stream))
                        (s   (cl-cc/vm/execution:make-vm-state :output-stream str)))
                   (expect (cl-cc/vm/io:vm-standard-output s) :to-be str))) (it-sequential "vm-execute-print-writes-to-stream"
  (let* ((str (make-string-output-stream))
         (s   (make-instance 'cl-cc/vm/io:vm-io-state :output-stream str)))
    (cl-cc/vm:vm-reg-set s :R0 42)
    (cl-cc/vm/protocols:execute-instruction
     (cl-cc/vm:make-vm-print :reg :R0) s 0 (make-hash-table :test #'equal))
    (expect (get-output-stream-string str) :to-equal (format nil "42~%")))))
;;; ─── Helpers ──────────────────────────────────────────────────────────────


;;; ─── make-string-output-stream / get-output-stream-string ─────────────────


;;; ─── read-from-string ─────────────────────────────────────────────────────


;;; ─── vm-allocate-file-handle ────────────────────────────────────────────────


;;; ─── vm-get-stream ──────────────────────────────────────────────────────────


;;; ─── vm-stream-open-p ──────────────────────────────────────────────────────


;;; ─── stream predicate instructions ─────────────────────────────────────────


;;; ─── handle-based string streams (vm-make-string-stream) ───────────────────


;;; ─── eof-p ──────────────────────────────────────────────────────────────────


;;; ─── read-char / read-line via handle ──────────────────────────────────────


;;; ─── %resolve-integer-stream-handle (extracted helper) ──────────────────


;;; ─── %copy-ht-into ────────────────────────────────────────────────────────


;;; ─── clone-vm-state ───────────────────────────────────────────────────────


;;; ─── FR-868: file-position / file-length / with-binary-file ───────────────


;;; ─── FR-923: buffered I/O and stream predicates ───────────────────────────


;;; ─── FR-924: special streams ───────────────────────────────────────────────


;;; ─── FR-927: pathname operations ───────────────────────────────────────────


;;; ─── FR-851/852/853/869: networking, DNS, TLS, mmap ───────────────────────

