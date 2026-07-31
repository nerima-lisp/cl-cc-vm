;;;; run-coverage.lisp
;;;;
;;;; Measures sb-cover expression/branch coverage of src/ while running the
;;;; test suite, and writes an HTML report next to this script. Coverage
;;;; percentages are reported but not gated: cl-cc-vm's own boundary-level
;;;; test suite intentionally exercises only the properties the extraction
;;;; rests on (see t/vm-boundary-test.lisp), not full VM behavior, so a
;;;; coverage floor here would be a fiction. Use the report to find gaps.
(require :asdf)

(defun script-directory ()
  (make-pathname
    :name nil
    :type nil
    :defaults
    (or *load-truename*
        *compile-file-truename*
        (error "Unable to determine the script location"))))

(let* ((root (script-directory))
       (report-stream *standard-output*))
  (asdf:initialize-source-registry
    `(:source-registry (:tree ,root) :inherit-configuration))
  ;; sb-cover only instruments code compiled under this policy, so it must
  ;; be proclaimed -- and the system force-recompiled under it -- before
  ;; RUN-ALL's :coverage t has anything to report. Mirrors cl-weave's own
  ;; --coverage CLI flag (see cl-weave's PREPARE-COVERAGE-COMPILATION).
  (require :sb-cover)
  (proclaim `(optimize (,(find-symbol "STORE-COVERAGE-DATA" "SB-COVER") 3)))
  (asdf:load-system "cl-cc-vm/test" :force t)
  (let ((passed-p
          (uiop:symbol-call
            :cl-weave :run-all
            :stream report-stream
            :pass-with-no-tests nil
            :coverage t
            :coverage-output (merge-pathnames "coverage.data" root)
            :coverage-report-directory (merge-pathnames "coverage-report/" root)
            :coverage-include-pathnames (list (merge-pathnames "src/" root)))))
    (format report-stream "~&cl-cc-vm: coverage report written to coverage-report/~%")
    (finish-output report-stream)
    (uiop:quit (if passed-p 0 1))))
