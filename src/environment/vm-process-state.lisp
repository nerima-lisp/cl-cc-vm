(in-package #:cl-cc/vm/environment)

;;; ── FR-1006: Environment variables and process exit helpers ───────────────

(defparameter *command-line-args*
  (copy-list (cdr sb-ext:*posix-argv*))
  "Command-line arguments visible to VM stdlib code.")

(defparameter *command-line-arguments* *command-line-args*
  "Stable script-visible command-line arguments after the script name.")

(defun getenv (name)
  "Return environment variable NAME, preserving vm-getenv taint marking."
  (vm-getenv name))

(defun setenv (name value &key overwrite)
  "Set environment variable NAME to VALUE.  Returns VALUE on success."
  (declare (ignore overwrite))
  (setf (host-kit:getenv (string name)) (string value))
  value)

(defun exit (&optional (code 0))
  "Exit the hosting process with CODE."
  (host-kit:quit code))

;;; ── FR-1066: SBCL-compatible implementation surface ───────────────────────

(defparameter *features*
  (remove-duplicates (append '(:cl-cc :cl-cc-vm :ansi-cl :common-lisp)
                             cl:*features*)
                     :test #'eq)
  "VM-visible feature list with CL-CC identifiers and host features.")

;;; LISP-IMPLEMENTATION-TYPE/VERSION are implemented in vm-environment.lisp.

;;; ── FR-1054/1082: Reader/load state variables ─────────────────────────────

(defparameter *read-base* 10 "Default radix used by VM reader helpers.")

(defparameter *read-eval* t "Whether #. reader evaluation is enabled.")

(defparameter *read-suppress* nil "When true, reader suppresses object construction where supported.")

(defparameter *read-default-float-format* 'single-float "Default float format for the reader.")

(defparameter *load-pathname* nil "Pathname currently being loaded by VM stdlib code.")

(defparameter *load-truename* nil "Truename currently being loaded by VM stdlib code.")

(defstruct source-location
  "Source location object for parser/load annotations."
  pathname file line column form)

(eval-when (:load-toplevel :execute) (vm-register-host-bridge 'getenv #'getenv) (vm-register-host-bridge 'setenv #'setenv))

(defmethod vm-load-pathname () *load-pathname*)
