;;;; t/vm-terminal-test.lisp — terminal control backed by cl-tty-kit
;;;;
;;;; src/vm-terminal.lisp stopped shelling out to stty(1) in favor of
;;;; cl-tty-kit's termios/ioctl calls; these pin the boundary shapes
;;;; callers of VM-TERMINAL-SIZE/VM-ANSI-COLOR/VM-ANSI-RESET already expect.

(in-package :cl-cc-vm/test)

(describe-sequential "terminal control"
  (it "returns two positive integers for terminal size, terminal or not"
    (multiple-value-bind (columns rows) (cl-cc/vm:vm-terminal-size)
      (expect (integerp columns) :to-be-truthy)
      (expect (integerp rows) :to-be-truthy)
      (expect (plusp columns) :to-be-truthy)
      (expect (plusp rows) :to-be-truthy)))

  (it "writes an SGR color escape sequence"
    (expect (string= (with-output-to-string (s) (cl-cc/vm:vm-ansi-color s :red))
                     (format nil "~C[31m" #\Escape))
            :to-be-truthy))

  (it "writes the SGR reset escape sequence"
    (expect (string= (with-output-to-string (s) (cl-cc/vm:vm-ansi-reset s))
                     (format nil "~C[m" #\Escape))
            :to-be-truthy)))
