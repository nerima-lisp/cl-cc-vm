(in-package :cl-cc/vm)

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — Print Variables and Stream External-Format Encoding
;;;
;;; Contains:
;;;   - *print-* and *default-external-format* parameters
;;;   - *vm-stream-external-formats* side table
;;;   - External format normalization and encoding/decoding utilities:
;;;       %vm-normalize-external-format, %vm-host-external-format,
;;;       vm-encode-string, vm-decode-bytes, vm-stream-external-format,
;;;       vm-set-stream-external-format
;;;
;;; vm-io-state, vm-open, and the stream handle/bridge machinery built on
;;; these encoding utilities are in io.lisp (loads next).
;;;
;;; Load order: after vm-run.lisp, before io.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(declaim (special *print-circle*))

(declaim (special *print-pprint-dispatch* *readtable*))

(defparameter *print-level* nil
  "Maximum depth to print, or NIL for unlimited depth.")

(defparameter *print-length* nil
  "Maximum number of elements to print at each level, or NIL for unlimited length.")

(defparameter *print-case* :upcase
  "Case conversion used when printing symbols: :UPCASE, :DOWNCASE, or :CAPITALIZE.")

(defparameter *print-escape* t
  "When true, print escape characters needed for READ to recover the object.")

(defparameter *print-gensym* t
  "When true, print uninterned symbols with the #: prefix.")

(defparameter *print-array* t
  "When true, print array contents when possible.")

(defparameter *print-lines* nil
  "Maximum pretty-printer lines to emit, or NIL for unlimited lines.")

(defparameter *print-right-margin* nil
  "Pretty-printer target line width, or NIL for the implementation default.")

(defparameter *default-external-format* :utf-8
  "Default external format for VM character streams.")

(defvar *vm-stream-external-formats* (make-hash-table :test #'eq)
  "Side table mapping host stream objects to VM external-format designators.")

(defparameter *print-readably* nil
  "When true, signal if an object cannot be printed readably.")

(defparameter *print-base* 10
  "Radix used when printing integers.")

(defparameter *print-radix* nil
  "When true, print radix markers for numbers.")

(defparameter *print-pretty* nil
  "When true, use the pretty printer for structured output.")

(defun %vm-normalize-external-format (format)
  "Normalize VM external format designators to canonical keywords."
  (case (or format *default-external-format*)
    ((:utf8 :utf-8) :utf-8)
    ((:utf16 :utf-16 :utf-16le) :utf-16le)
    (:utf-16be :utf-16be)
    ((:utf32 :utf-32 :utf-32le) :utf-32le)
    (:utf-32be :utf-32be)
    ((:latin1 :latin-1 :iso-8859-1) :latin-1)
    (:ascii :ascii)
    (otherwise (error "Unsupported external format: ~S" format))))

(defun %vm-host-external-format (format)
  "Return the closest host external-format designator for FORMAT."
  (case (%vm-normalize-external-format format)
    (:latin-1 :latin-1)
    (otherwise (%vm-normalize-external-format format))))

(defun %vm-signal-or-substitute-encoding-error (mode replacement)
  (ecase mode
    (:error (error "Character cannot be represented in requested external format"))
    (:replace replacement)
    (:ignore nil)))

(defun %vm-replacement-character ()
  (or (code-char #xfffd) #\?))

(defun %vm-encode-latin/ascii (string limit error-mode)
  (let ((bytes (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for ch across string
          for code = (char-code ch)
          do (cond
               ((<= code limit) (vector-push-extend code bytes))
               (t (let ((sub (%vm-signal-or-substitute-encoding-error error-mode 63)))
                    (when sub (vector-push-extend sub bytes))))))
    bytes))

(defun vm-encode-string (string format &key (error-mode :error))
  "Encode STRING into an (UNSIGNED-BYTE 8) vector using FORMAT.
ERROR-MODE is :ERROR, :REPLACE, or :IGNORE."
  (check-type string string)
  (let ((format (%vm-normalize-external-format format)))
    (case format
      (:ascii (%vm-encode-latin/ascii string 127 error-mode))
      (:latin-1 (%vm-encode-latin/ascii string 255 error-mode))
      (otherwise
       (handler-case
           (sb-ext:string-to-octets string :external-format (%vm-host-external-format format))
         (error (e)
           (ecase error-mode
             (:error (error e))
             ((:replace :ignore)
              ;; Re-encode after replacing non-Latin BMP surrogates defensively.
              (sb-ext:string-to-octets
               (with-output-to-string (out)
                 (loop for ch across string
                       do (if (code-char (char-code ch))
                              (write-char ch out)
                              (unless (eq error-mode :ignore) (write-char (%vm-replacement-character) out)))))
               :external-format (%vm-host-external-format format))))))))))

(defun %vm-coerce-octets (bytes)
  (cond
    ((typep bytes '(array (unsigned-byte 8) (*))) bytes)
    ((vectorp bytes)
     (let ((out (make-array (length bytes) :element-type '(unsigned-byte 8))))
       (dotimes (i (length bytes) out) (setf (aref out i) (aref bytes i)))))
    ((listp bytes)
     (make-array (length bytes) :element-type '(unsigned-byte 8) :initial-contents bytes))
    (t (error "Expected octet vector/list: ~S" bytes))))

(defun %vm-detect-bom (bytes requested-format)
  "Return canonical format and starting offset after recognizing a Unicode BOM."
  (let ((n (length bytes))
        (fmt (%vm-normalize-external-format requested-format)))
    (cond
      ((and (>= n 4) (= (aref bytes 0) #xFF) (= (aref bytes 1) #xFE)
            (= (aref bytes 2) 0) (= (aref bytes 3) 0))
       (values :utf-32le 4))
      ((and (>= n 4) (= (aref bytes 0) 0) (= (aref bytes 1) 0)
            (= (aref bytes 2) #xFE) (= (aref bytes 3) #xFF))
       (values :utf-32be 4))
      ((and (>= n 3) (= (aref bytes 0) #xEF) (= (aref bytes 1) #xBB) (= (aref bytes 2) #xBF))
       (values :utf-8 3))
      ((and (>= n 2) (= (aref bytes 0) #xFF) (= (aref bytes 1) #xFE))
       (values :utf-16le 2))
      ((and (>= n 2) (= (aref bytes 0) #xFE) (= (aref bytes 1) #xFF))
       (values :utf-16be 2))
      (t (values fmt 0)))))

(defun %vm-decode-latin/ascii (bytes limit error-mode)
  (with-output-to-string (out)
    (loop for byte across bytes
          do (cond
               ((<= byte limit) (write-char (code-char byte) out))
               (t (ecase error-mode
                    (:error (error "Invalid byte ~D for requested external format" byte))
                    (:replace (write-char (%vm-replacement-character) out))
                    (:ignore nil)))))))

(defun vm-decode-bytes (bytes format &key (error-mode :error))
  "Decode BYTES into a string using FORMAT, auto-removing UTF BOMs."
  (let ((octets (%vm-coerce-octets bytes)))
    (multiple-value-bind (format start) (%vm-detect-bom octets format)
      (let ((payload (if (zerop start) octets (subseq octets start))))
        (case format
          (:ascii (%vm-decode-latin/ascii payload 127 error-mode))
          (:latin-1 (%vm-decode-latin/ascii payload 255 error-mode))
          (otherwise
           (handler-case
               (sb-ext:octets-to-string payload :external-format (%vm-host-external-format format))
             (error (e)
               (ecase error-mode
                 (:error (error e))
                 (:replace (string (%vm-replacement-character)))
                 (:ignore ""))))))))))

(defun vm-stream-external-format (stream)
  "Return STREAM's VM external format, defaulting to *DEFAULT-EXTERNAL-FORMAT*."
  (gethash stream *vm-stream-external-formats* *default-external-format*))

(defun vm-set-stream-external-format (stream fmt)
  "Associate STREAM with external format FMT and return FMT's canonical form."
  (setf (gethash stream *vm-stream-external-formats*)
        (%vm-normalize-external-format fmt)))
