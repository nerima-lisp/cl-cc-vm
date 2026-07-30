;;;; run-tests.lisp
;;;;
;;;; Test entry point: register this checkout with ASDF, inherit the caller's
;;;; configuration for everything else, and run the test system.
;;;;
;;;; cl-weave arrives through CL_SOURCE_REGISTRY, which flake.nix sets for
;;;; `nix flake check`, `nix run .#test` and `nix develop` alike. That is why
;;;; there is no dependency-locating machinery here any more: the old
;;;; scripts/bootstrap.lisp parsed .asd files by hand and loaded cl-weave's
;;;; sources one by one, because cl-weave used to be pulled in as a bare
;;;; source tree under CL_CC_AST_CL_WEAVE_ROOT rather than as a flake input.
;;;;
;;;; An empty suite still fails: cl-cc-vm/test's :perform passes
;;;; :pass-with-no-tests nil to cl-weave, so a run that registers zero tests
;;;; is an error rather than a pass.
(require :asdf)

(defun script-directory ()
  (make-pathname
    :name
    nil
    :type
    nil
    :defaults
    (or
      *load-truename*
      *compile-file-truename*
      (error "Unable to determine the script location"))))

(progn
  (defun configure-local-source-registry (root)
    (asdf:initialize-source-registry
      `(:source-registry (:tree ,root) :inherit-configuration)))
  (let* ((root (script-directory))
         (report-stream *standard-output*))
    (configure-local-source-registry root)
    (asdf:load-system "cl-cc-vm/test")
    (let* ((plan (uiop:symbol-call :cl-weave :list-tests :stream (make-broadcast-stream)))
           (test-count (length plan)))
      (format report-stream "~&cl-cc-vm: running ~D test~:P~%" test-count)
      (finish-output report-stream)
      (let ((passed-p
            (uiop:symbol-call
              :cl-weave
              :run-all
              :stream
              report-stream
              :pass-with-no-tests
              nil)))
        (format
          report-stream
          "~&cl-cc-vm: ~:[FAILED~;PASSED~] (~D test~:P)~%"
          passed-p
          test-count)
        (finish-output report-stream)
        (uiop:quit
          (if passed-p 0
            1))))))
