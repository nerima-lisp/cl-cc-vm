(in-package #:cl-cc/vm/io)

;;; ── FR-959: External formats ──────────────────────────────────────────────

(defparameter *default-external-format* :utf-8
  "Default external format used by the VM OPEN wrapper.")

(defvar *vm-stream-external-formats* (make-hash-table :test #'eq)
  "Weakly maps streams opened by VM OPEN to their requested external format.")

(defun %normalize-external-format (external-format)
  (or external-format *default-external-format*))

(defun open (filespec &rest args &key external-format &allow-other-keys)
  "Open FILESPEC with VM default :EXTERNAL-FORMAT handling.
The host CL OPEN does the concrete I/O; this wrapper records the requested format
for STREAM-EXTERNAL-FORMAT and supplies *DEFAULT-EXTERNAL-FORMAT* when omitted."
  (let* ((format (%normalize-external-format external-format))
         (stream (apply #'cl:open filespec :external-format format args)))
    (when stream
      (setf (gethash stream *vm-stream-external-formats*) format))
    stream))

(defun stream-external-format (stream)
  "Return STREAM's external format when known, otherwise NIL."
  (or (gethash stream *vm-stream-external-formats*)
      (ignore-errors (sb-impl::fd-stream-external-format stream))))

(defun encode-external-format (string &key (external-format *default-external-format*))
  "Encode STRING to a vector of octets using EXTERNAL-FORMAT."
  (declare (ignore external-format))
  (string-to-utf8-bytes string))

(defun decode-external-format (octets &key (external-format *default-external-format*))
  "Decode OCTETS to a string using EXTERNAL-FORMAT."
  (declare (ignore external-format))
  (utf8-bytes-to-string octets))

(eval-when (:load-toplevel :execute) (vm-register-host-bridge 'open #'open) (vm-register-host-bridge 'stream-external-format #'stream-external-format) (vm-register-host-bridge 'encode-external-format #'encode-external-format) (vm-register-host-bridge 'decode-external-format #'decode-external-format))
