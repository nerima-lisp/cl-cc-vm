(in-package #:cl-cc-vm/facade-test)

(describe-sequential "cl-cc-vm facade symbol identity"
  (it "matches facade exports to their reviewed feature packages"
      (let* ((snapshot-path (asdf:system-relative-pathname "cl-cc-vm" "t/export-surface.txt"))
             (changelog-path (asdf:system-relative-pathname "cl-cc-vm" "CHANGELOG.md"))
             (rows
              (with-open-file (stream snapshot-path)
                (let ((*read-eval* nil))
                  (let* ((sentinel (gensym "EOF"))
                         (rows (read stream nil sentinel)))
                    (unless (eq (read stream nil sentinel) sentinel)
                      (error "Export surface contains trailing S-expressions: ~S" snapshot-path))
                    rows))))
             (changelog (uiop:read-file-string changelog-path))
             (facade (find-package "CL-CC/VM"))
             (expected-names (make-hash-table :test (function equal)))
             (mismatches nil)
             (extra-names nil)
             (actual-count 0))
        (unless facade
          (error "Missing cl-cc/vm facade package"))
        (dolist (row rows)
          (destructuring-bind (kind policy facade-name feature-name owner-name name
                                    &optional change-reference)
              row
            (let ((feature (find-package (string-upcase feature-name)))
                  (owner (find-package (string-upcase owner-name))))
              (case policy
                ((:keep :add)
                 (setf (gethash name expected-names) t)
                 (multiple-value-bind (facade-symbol facade-status)
                     (find-symbol name facade)
                   (multiple-value-bind (feature-symbol feature-status)
                       (if feature (find-symbol name feature) (values nil nil))
                     (multiple-value-bind (owner-symbol owner-status)
                         (if owner (find-symbol name owner) (values nil nil))
                       (unless (and (eq kind :symbol)
                                    (string= facade-name "cl-cc/vm")
                                    feature
                                    owner
                                    (symbolp facade-symbol)
                                    (symbolp feature-symbol)
                                    (symbolp owner-symbol)
                                    (eq facade-status :external)
                                    (or (eq policy :keep)
                                        (eq feature-status :external))
                                    (eq owner-status :external)
                                    (eq facade-symbol feature-symbol)
                                    (eq facade-symbol owner-symbol)
                                    (eq (symbol-package facade-symbol) owner))
                         (push name mismatches)))))
                 (when (and (eq policy :keep) change-reference)
                   (push name mismatches))
                 (when (and (eq policy :add)
                            (not (and (stringp change-reference)
                                      (string= change-reference "CHANGELOG.md#unreleased")
                                      (search "## Unreleased"
                                              changelog
                                              :test (function char-equal))))
                            (push name mismatches))))
                (:remove
                 (unless (and (eq kind :symbol)
                              (string= facade-name "cl-cc/vm"))
                   (push name mismatches))
                 (multiple-value-bind (facade-symbol facade-status)
                     (find-symbol name facade)
                   (declare (ignore facade-symbol))
                   (when (eq facade-status :external)
                     (push name mismatches)))
                 (unless (and (stringp change-reference)
                              (string= change-reference "CHANGELOG.md#unreleased")
                              (search "## Unreleased"
                                      changelog
                                      :test (function char-equal)))
                   (push name mismatches)))
                (otherwise
                 (push name mismatches))))))
        (do-external-symbols (symbol facade)
                             (incf actual-count)
                             (unless (gethash (symbol-name symbol) expected-names)
                               (push (symbol-name symbol) extra-names)))
        (expect (cl-cc/vm/data:length rows) :to-be 2072)
        (expect (hash-table-count expected-names) :to-be 2072)
        (expect mismatches :to-be nil)
        (expect actual-count :to-be 2072)
        (expect extra-names :to-be nil)))
  (it "preserves legacy VM2 initial bindings through the facade API"
    (let* ((state (cl-cc/vm:make-vm2-state))
           (globals (cl-cc/vm:vm2-state-global-vars state))
           (expected
             (list
              (cons (quote cl-cc/vm:*features*) (quote (:common-lisp :cl-cc)))
              (cons (quote cl-cc/vm:*active-restarts*) nil)
              (cons (quote cl:*standard-output*) cl:*standard-output*)
              (cons (quote cl:*standard-input*) cl:*standard-input*)
              (cons (quote cl:*error-output*) cl:*error-output*)
              (cons (quote cl-cc/vm:*trace-output*)
                    (symbol-value (quote cl-cc/vm:*trace-output*)))
              (cons (quote cl:*debug-io*) cl:*debug-io*)
              (cons (quote cl:*query-io*) cl:*query-io*))))
      (expect (hash-table-count globals) :to-be 8)
      (expect
       (and
        (eq (symbol-package (quote cl-cc/vm:*features*))
            (find-package "CL-CC/VM/ENVIRONMENT"))
        (eq (symbol-package (quote cl-cc/vm:*active-restarts*))
            (find-package "CL-CC/VM/CONDITIONS"))
        (eq (symbol-package (quote cl-cc/vm:*trace-output*))
            (find-package "CL-CC/VM/DEBUGGING")))
       :to-be t)
      (expect
       (every (lambda (entry)
                (equal (gethash (car entry) globals :missing)
                       (cdr entry)))
              expected)
       :to-be t)))
  (it "imports CLOS-owned names into the VM facade"
  (let ((facade (find-package "CL-CC/VM")))
    (dolist (entry (list (list "ENSURE-GENERIC-FUNCTION" "CL-CC/VM/CLOS")
                         (list "SLOT-VALUE-USING-CLASS" "CL-CC/VM/CLOS")))
      (let* ((name (first entry))
             (owner (find-package (second entry))))
        (multiple-value-bind (facade-symbol facade-status)
            (find-symbol name facade)
          (multiple-value-bind (owner-symbol owner-status)
              (find-symbol name owner)
            (expect (eq facade-symbol owner-symbol) :to-be t)
            (expect (eq (symbol-package facade-symbol) owner) :to-be t)
            (expect (eq facade-status :internal) :to-be t)
            (expect (and owner-symbol owner-status) :to-be-truthy)
            (expect (fboundp facade-symbol) :to-be-truthy)))))
    (let* ((slot-symbol (find-symbol "SLOT-VALUE-USING-CLASS" facade))
           (setf-name (list (find-symbol "SETF" "COMMON-LISP") slot-symbol)))
      (expect (fboundp setf-name) :to-be-truthy))))
  (it "uses shared reader symbols and restores reader calls across features"
    (let ((protocols (find-package "CL-CC/VM/PROTOCOLS"))
          (facade (find-package "CL-CC/VM")))
      (dolist (entry
               (list
                (list "VM-VALUE" "CL-CC/VM/DATA" "CL-CC/VM/EXECUTION" "CL-CC/VM/IO")
                (list "VM-INDEX" "CL-CC/VM/DATA" "CL-CC/VM/EXECUTION")
                (list "VM-ARGS" "CL-CC/VM/CLOS" "CL-CC/VM/EXECUTION")
                (list "VM-HANDLER-LABEL" "CL-CC/VM/CONDITIONS" "CL-CC/VM/EXECUTION")
                (list "VM-HANDLER-RESULT-REG" "CL-CC/VM/CONDITIONS" "CL-CC/VM/EXECUTION")
                (list "VM-VALUE-REG" "CL-CC/VM/CLOS" "CL-CC/VM/EXECUTION")))
        (let* ((name (first entry))
               (protocol-symbol (find-symbol name protocols))
               (facade-symbol (find-symbol name facade)))
          (expect (eq protocol-symbol facade-symbol) :to-be t)
          (expect (eq (symbol-package protocol-symbol) protocols) :to-be t)
          (dolist (package-name (rest entry))
            (let ((feature-symbol
                    (find-symbol name (find-package package-name))))
              (expect (eq feature-symbol protocol-symbol) :to-be t)))))
    (expect (cl-cc/vm:vm-index
             (cl-cc/vm:make-vm-char :dst :dst :string :string :index 3))
            :to-be 3)
    (expect (cl-cc/vm:vm-index
             (cl-cc/vm:make-vm-nth-value :dst :dst :index 2))
            :to-be 2)
    (expect (cl-cc/vm:vm-value
             (cl-cc/vm:make-vm-acons :dst :dst :key :key
                                      :value :data :alist nil))
            :to-be :data)
    (expect (cl-cc/vm:vm-value
             (cl-cc/vm:make-vm-const :dst :dst :value :execution))
            :to-be :execution)
    (expect (cl-cc/vm:vm-value
             (cl-cc/vm:make-vm-eof-p :dst :dst :value :io))
            :to-be :io)
    (expect (cl-cc/vm:vm-args
             (cl-cc/vm:make-vm-generic-call :dst :dst :gf-reg :gf
                                             :args (list :arg-1 :arg-2)))
            :to-equal (list :arg-1 :arg-2))
    (expect (cl-cc/vm:vm-args
             (cl-cc/vm:make-vm-call :dst :dst :func :func
                                    :args (list :call-arg-1 :call-arg-2)))
            :to-equal (list :call-arg-1 :call-arg-2))
    (let ((condition-handler
            (cl-cc/vm/conditions::make-vm-push-handler
             :handler-type :type :handler-label :condition-label
             :result-reg :condition-result))
          (execution-handler
            (cl-cc/vm:make-vm-establish-handler
             :handler-label :execution-label :result-reg :execution-result
             :error-type :type)))
      (expect (cl-cc/vm:vm-handler-label condition-handler)
              :to-be :condition-label)
      (expect (cl-cc/vm:vm-handler-result-reg condition-handler)
              :to-be :condition-result)
      (expect (cl-cc/vm:vm-handler-label execution-handler)
              :to-be :execution-label)
      (expect (cl-cc/vm:vm-handler-result-reg execution-handler)
              :to-be :execution-result))
    (expect (cl-cc/vm:vm-value-reg
             (cl-cc/vm:make-vm-slot-write :obj-reg :object
                                           :slot-name :slot
                                           :value-reg :value))
            :to-be :value)
    (expect (cl-cc/vm:vm-value-reg
             (cl-cc/vm:make-vm-abort-to-prompt :prompt :prompt
                                                :value :abort-value))
            :to-be :abort-value))))

(describe-sequential "cl-cc/vm interop facade API"
  (it "calls foreign functions through the root facade"
    (expect (= 4 (cl-cc/vm:foreign-funcall "strlen" :string "abcd" :int))
            :to-be t))
  (it "registers documentation through the root facade"
    (let ((name (gensym "FACADE-PUBLIC-DOC-"))
          (docstring "facade public documentation"))
      (expect (string=
               docstring
               (cl-cc/vm:register-documentation name :function docstring))
              :to-be t))))
