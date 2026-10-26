(in-package #:cl-cc/vm/io/test)

(defun register-io-network-tests ()
  (it-sequential "io-fr869-mmap-file-shared-sync"
  (let ((path (merge-pathnames (format nil "clcc-mmap-~A.bin" (gensym))
                               (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)
                                :if-exists :supersede :if-does-not-exist :create)
             (write-sequence #(65 66 67) out))
           (cl-cc/vm:with-mmap (region path :protection :read-write :flags :shared)
             (let ((array (cl-cc/vm:mmap-array region)))
               (multiple-value-bind (base offset) (array-displacement array)
                 (expect base :to-be-truthy)
                 (expect (= 0 offset) :to-be-truthy))
               (expect (= 65 (aref array 0)) :to-be-truthy)
               (setf (aref array 1) 90)
               (cl-cc/vm:mmap-sync region)))
           (with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
             (let ((bytes (make-array 3 :element-type '(unsigned-byte 8))))
               (read-sequence bytes in)
               (expect (equalp #(65 90 67) bytes) :to-be-truthy))))
      (when (probe-file path) (delete-file path))))) (it-sequential "io-fr853-tls-context-and-unsupported-condition"
  (let ((context (cl-cc/vm:make-tls-context :verify-peer t)))
    (expect (cl-cc/vm::tls-context-p context) :to-be-truthy)
    (unless (find-package :cl+ssl)
      (let ((socket (cl-cc/vm:make-tcp-socket)))
        (unwind-protect
             (signals cl-cc/vm:tls-unsupported (cl-cc/vm:tls-wrap-socket socket context :hostname "localhost"))
          (cl-cc/vm:socket-close socket)))))) (it-sequential "io-fr852-dns-localhost-cache-and-async"
  (sb-ext:with-timeout 5
    (let ((addresses (cl-cc/vm:dns-resolve "localhost" :ttl 60)))
      (expect (member "127.0.0.1" addresses :test #'string=) :to-be-truthy)
      (expect (plusp (length (cl-cc/vm:getaddrinfo "localhost" :service 80))) :to-be-truthy)
      (let* ((timeout 5)
             (join-timeout (gensym "JOIN-TIMEOUT"))
             (async (cl-cc/vm:dns-resolve-async "localhost"))
             (thread (cl-cc/vm::dns-async-result-thread async)))
        (unwind-protect
             (progn
               (expect (eq (sb-thread:join-thread thread :timeout timeout :default join-timeout)
                           join-timeout)
                       :to-be-falsy)
               (expect (cl-cc/vm:dns-async-result-done-p async) :to-be-truthy)
               (expect (cl-cc/vm:dns-async-result-error async) :to-equal nil)
               (expect (member "127.0.0.1" (cl-cc/vm:dns-async-result-result async)
                               :test #'string=)
                       :to-be-truthy))
          (when (sb-thread:thread-alive-p thread)
            (sb-thread:terminate-thread thread))
          (sb-thread:join-thread thread :timeout timeout :default join-timeout)))))) (it-sequential "io-fr851-udp-localhost-roundtrip"
  (sb-ext:with-timeout 5
    (let ((receiver nil) (sender nil))
      (unwind-protect
           (progn
             (setf receiver (cl-cc/vm:make-udp-socket :reuse-address t))
             (cl-cc/vm:socket-bind receiver "127.0.0.1" 0)
             (multiple-value-bind (_host port) (cl-cc/vm:socket-local-address receiver)
               (declare (ignore _host))
               (setf sender (cl-cc/vm:make-udp-socket))
               (cl-cc/vm:socket-connect sender "127.0.0.1" port)
               (expect (= 3 (cl-cc/vm:socket-send sender #(117 100 112))) :to-be-truthy)
               (multiple-value-bind (payload count) (cl-cc/vm:socket-receive receiver :size 3 :stringp t)
                 (expect (= 3 count) :to-be-truthy)
                 (expect payload :to-equal "udp"))))
        (when sender (cl-cc/vm:socket-close sender))
        (when receiver (cl-cc/vm:socket-close receiver)))))) (it-sequential "io-fr851-tcp-localhost-roundtrip"
  (sb-ext:with-timeout 5
    (let ((listener nil) (client nil) (accepted nil))
      (unwind-protect
           (progn
             (setf listener (cl-cc/vm:make-tcp-socket :reuse-address t :tcp-nodelay t :keepalive t))
             (cl-cc/vm:socket-bind listener "127.0.0.1" 0)
             (cl-cc/vm:socket-listen listener 1)
             (multiple-value-bind (host port) (cl-cc/vm:socket-local-address listener)
               (expect host :to-equal "127.0.0.1")
               (setf client (cl-cc/vm:make-tcp-socket :tcp-nodelay t))
               (cl-cc/vm:socket-connect client "127.0.0.1" port)
               (setf accepted (cl-cc/vm:socket-accept listener))
               (expect (= 4 (cl-cc/vm:socket-send client #(112 105 110 103))) :to-be-truthy)
               (multiple-value-bind (payload count) (cl-cc/vm:socket-receive accepted :size 4 :stringp t)
                 (expect (= 4 count) :to-be-truthy)
                 (expect payload :to-equal "ping"))))
        (when accepted (cl-cc/vm:socket-close accepted))
        (when client (cl-cc/vm:socket-close client))
        (when listener (cl-cc/vm:socket-close listener)))))))
