;;; vm-terminal.lisp — FR-1100: Terminal control
;;;
;;; ANSI terminal escape sequences, raw mode, and readline. Terminal sizing
;;; and raw-mode entry/exit are cl-tty-kit's TERMINAL-SIZE and WITH-RAW-MODE,
;;; used directly: both talk to the terminal through termios/ioctl rather
;;; than shelling out to stty, which is what this file did before adopting
;;; cl-tty-kit.

(in-package :cl-cc/vm)

(defun vm-isatty (stream)
  "Return T if STREAM is connected to a terminal."
  (or (interactive-stream-p stream)
      (ignore-errors (sb-unix:unix-isatty (sb-sys:fd-stream-fd stream)))))

(defun vm-terminal-size (&optional (stream *standard-output*))
  "Return (values columns rows) for the terminal attached to STREAM, or the
80x24 default when STREAM is not a terminal or its size is unavailable."
  (declare (ignore stream))
  (multiple-value-bind (columns rows) (cl-tty-kit:terminal-size)
    (if (and columns rows)
        (values columns rows)
        (values 80 24))))

(defun vm-ansi-color (stream color)
  "Emit ANSI escape sequence for COLOR on STREAM.
   Supported: :black, :red, :green, :yellow, :blue, :magenta, :cyan, :white
   and their bright variants :bright-red etc."
  (let ((code (ecase color
                (:black 30) (:red 31) (:green 32) (:yellow 33)
                (:blue 34) (:magenta 35) (:cyan 36) (:white 37)
                (:bright-black 90) (:bright-red 91) (:bright-green 92)
                (:bright-yellow 93) (:bright-blue 94) (:bright-magenta 95)
                (:bright-cyan 96) (:bright-white 97))))
    (write-string (cl-tty-kit:ansi-sgr code) stream)))

(defun vm-ansi-reset (stream)
  "Reset all ANSI attributes on STREAM."
  (write-string (cl-tty-kit:ansi-sgr) stream))

(defun vm-with-raw-terminal (thunk)
  "Execute THUNK with terminal in raw mode (no line buffering, no echo).
   Restores original terminal settings on exit."
  (cl-tty-kit:with-raw-mode () (funcall thunk)))

(defun vm-read-key (&optional (stream *standard-input*))
  "Read a single keypress from STREAM without waiting for newline.
   Returns the character (or keyword for special keys)."
  (vm-with-raw-terminal (lambda () (read-char stream nil +eof-value+))))

(defun vm-readline (prompt &key (history nil))
  "Read a line with PROMPT, supporting basic editing and HISTORY.
   Delegates to host CL readline or provides line-editor stub."
  (declare (ignore history))
  (format *query-io* "~A" prompt)
  (force-output *query-io*)
  (read-line *query-io* nil nil))

(export '(vm-isatty
          vm-terminal-size
          vm-ansi-color
          vm-ansi-reset
          vm-with-raw-terminal
          vm-read-key
          vm-readline))
