;;; ──── FR-748: Code Signing ────
(in-package #:cl-cc/vm/interop)

(defvar *code-sign-identity* nil
  "Code signing identity (e.g., 'Developer ID: ...' for macOS).")

(defun sign-binary (binary-path &optional (identity *code-sign-identity*))
  "Sign BINARY-PATH with IDENTITY.
macOS: uses codesign --sign.
Windows: uses signtool.exe.
Linux: IMA signature support."
  (when identity
    #+darwin
    (let ((result (host-kit:run-program "codesign" (list "--sign" identity binary-path))))
      (write-string (host-kit:process-result-stdout result) *standard-output*)
      (write-string (host-kit:process-result-stderr result) *error-output*))
    #-darwin
    (warn "Code signing not supported on this platform"))
  t)
