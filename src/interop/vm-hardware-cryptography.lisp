(in-package #:cl-cc/vm/interop)

;; FR-849: AES-NI / SHA-NI Hardware Cryptography
(defun aes-encrypt (block key) (declare (ignore block key)) block)

(defun sha256-rounds (state msg) (declare (ignore state msg)) state)
