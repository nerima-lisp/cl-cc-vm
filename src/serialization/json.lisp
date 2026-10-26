(in-package #:cl-cc/vm/serialization)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage :json
    (:use :cl)
    (:export
     #:*json-null*
     #:parse
     #:parse-stream
     #:stringify
     #:stringify-stream)))

(defvar *json-null* :null)
(defconstant +json-max-input-length+ 16777216)
(defun json-parse (string)
  (multiple-value-bind (value end-index)
      (json-kit:parse-prefix string
                             :object-type :hash-table
                             :array-type :vector
                             :duplicate-key-policy :last
                             :null-value *json-null*
                             :false-value nil
                             :max-input-length +json-max-input-length+)
    (declare (ignore end-index))
    value))
(defun json-parse-stream (stream)
  ;; Keep adjacent values readable by framing one value before parse-prefix.
  (let ((buffer (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        (eof (gensym "EOF"))
        (line 1)
        (column 1)
        (previous-cr nil))
    (labels ((peek ()
               (peek-char nil stream nil eof))
             (take ()
               (when (>= (length buffer) +json-max-input-length+)
                 (error 'json-kit:json-parse-error
                        :position (length buffer)
                        :line line
                        :column column
                        :path nil
                        :expected "input within MAX-INPUT-LENGTH"
                        :context "stream"))
               (let ((character (read-char stream nil eof)))
                 (unless (eq character eof)
                   (vector-push-extend
                     character
                     buffer
                     (max 1
                          (min (max 4096 (array-total-size buffer))
                               (- +json-max-input-length+ (array-total-size buffer)))))
                   (cond
                     ((char= character #\Return)
                      (incf line)
                      (setf column 1 previous-cr t))
                     ((char= character #\Newline)
                      (unless previous-cr
                       (incf line)
                       (setf column 1))
                      (setf previous-cr nil))
                     (t
                      (incf column)
                      (setf previous-cr nil))))
                 character))
             (whitespace-character-p (character)
               (and (characterp character)
                    (member character '(#\Space #\Tab #\Newline #\Return))))
             (ascii-digit-p (character)
               (and (characterp character)
                    (char<= #\0 character #\9)))
             (take-count (count)
               (loop repeat count
                     while (characterp (peek))
                     do (take)))
             (take-string ()
               (take)
               (let ((escaped nil))
                 (loop for character = (peek)
                       until (eq character eof)
                       do (take)
                          (cond
                            (escaped (setf escaped nil))
                            ((char= character #\\) (setf escaped t))
                            ((char= character #\") (return))))))
             (take-container ()
               (let ((closers (list (if (char= (peek) #\{) #\} #\])))
                     (in-string nil)
                     (escaped nil))
                 (take)
                 (loop for character = (peek)
                       while closers
                       until (eq character eof)
                       do (take)
                          (cond
                            (in-string
                             (cond
                               (escaped (setf escaped nil))
                               ((char= character #\\) (setf escaped t))
                               ((char= character #\") (setf in-string nil))))
                            ((char= character #\") (setf in-string t))
                            ((char= character #\{) (push #\} closers))
                            ((char= character #\[) (push #\] closers))
                            ((member character '(#\} #\]))
                             (if (char= character (first closers))
                                 (pop closers)
                                 (return)))))))
             (take-number ()
               (when (and (characterp (peek)) (char= (peek) #\-))
                 (take))
               (let ((character (peek)))
                 (cond
                   ((and (characterp character) (char= character #\0))
                    (take))
                   ((and (characterp character) (char<= #\1 character #\9))
                    (loop while (ascii-digit-p (peek))
                          do (take)))))
               (when (and (characterp (peek)) (char= (peek) #\.))
                 (take)
                 (loop while (ascii-digit-p (peek))
                       do (take)))
               (when (and (characterp (peek))
                          (member (peek) '(#\e #\E)))
                 (take)
                 (when (and (characterp (peek))
                            (member (peek) '(#\+ #\-)))
                   (take))
                 (loop while (ascii-digit-p (peek))
                       do (take))))
             (read-value-frame ()
               (loop while (whitespace-character-p (peek))
                     do (take))
               (let ((first (peek)))
                 (cond
                   ((eq first eof))
                   ((char= first #\") (take-string))
                   ((member first '(#\{ #\[)) (take-container))
                   ((char= first #\t) (take-count 4))
                   ((char= first #\f) (take-count 5))
                   ((char= first #\n) (take-count 4))
                   ((or (char= first #\-) (ascii-digit-p first))
                    (take-number))
                   (t (take))))))
      (unless (and (streamp stream) (input-stream-p stream))
        (error "JSON parsing requires an input character stream"))
      (multiple-value-bind (character-stream-p certain-p)
          (subtypep (stream-element-type stream) 'character)
        (unless (and certain-p character-stream-p)
          (error "JSON parsing requires an input character stream")))
      (read-value-frame)
      (multiple-value-bind (value end-index)
          (json-kit:parse-prefix (coerce buffer 'string)
                                 :object-type :hash-table
                                 :array-type :vector
                                 :duplicate-key-policy :last
                                 :null-value *json-null*
                                 :false-value nil
                                 :max-input-length +json-max-input-length+)
        (declare (ignore end-index))
        value))))
(defun json-stringify (object)
  (with-output-to-string (stream)
    (json-stringify-stream object stream)))
(defun json-stringify-stream (object stream)
  (let ((active (make-hash-table :test (function eq))))
    (labels ((active-aggregate (value thunk)
               (when (gethash value active)
                 (error "Circular object graphs cannot be serialized as JSON"))
               (setf (gethash value active) t)
               (unwind-protect (funcall thunk)
                 (remhash value active)))
             (adapt (value)
               (cond
                 ((null value) nil)
                 ((eq value t) t)
                 ((eq value *json-null*) nil)
                 ((stringp value) value)
                 ((numberp value) value)
                 ((hash-table-p value)
                  (active-aggregate
                   value
                   (lambda ()
                     (let ((members nil))
                       (maphash
                        (lambda (key item)
                          (push (cons (string key) (adapt item)) members))
                        value)
                       (json-kit:make-json-object (nreverse members))))))
                 ((and (vectorp value) (not (stringp value)))
                  (active-aggregate
                   value
                   (lambda ()
                     (coerce (loop for element across value collect (adapt element))
                             (quote vector)))))
                 ((consp value)
                  (active-aggregate
                   value
                   (lambda ()
                     (cons (adapt (car value))
                           (adapt (cdr value))))))
                 (t (format nil "~S" value)))))
      (json-kit:write-json (adapt object) stream :null-value nil)
      object)))

(in-package :json)

(defvar *json-null* :null)

(defun parse (string)
  (let ((cl-cc/vm/serialization:*json-null* *json-null*))
    (cl-cc/vm/serialization:json-parse string)))

(defun parse-stream (stream)
  (let ((cl-cc/vm/serialization:*json-null* *json-null*))
    (cl-cc/vm/serialization:json-parse-stream stream)))

(defun stringify (object)
  (let ((cl-cc/vm/serialization:*json-null* *json-null*))
    (cl-cc/vm/serialization:json-stringify object)))

(defun stringify-stream (object stream)
  (let ((cl-cc/vm/serialization:*json-null* *json-null*))
    (cl-cc/vm/serialization:json-stringify-stream object stream)))
