(defpackage #:cl-cc/vm/serialization
  (:use #:cl)
    (:import-from #:cl-cc/vm/protocols
   #:instruction->sexp
   #:sexp->instruction
  )
  (:import-from #:cl-cc/vm/clos
   #:class-slots
   #:slot-definition-name
  )
  (:import-from #:cl-cc/vm/io
   #:mmap-file
  )
  (:shadowing-import-from #:cl-cc/vm/io
   #:*print-circle*
   #:*print-readably*
  )
  (:shadowing-import-from #:cl-cc/vm/environment
   #:*read-eval*
  )
  (:import-from #:cl-cc/vm/execution #:make-vm-load-time-value-cell
   #:make-vm-program
   #:vm-load-time-value-cell
   #:vm-load-time-value-cell-form
   #:vm-load-time-value-cell-id
   #:vm-load-time-value-cell-read-only-p
   #:vm-load-time-value-cell-resolved-p
   #:vm-load-time-value-cell-value
   #:vm-program
   #:vm-program-calling-convention
   #:vm-program-compilation-tier
   #:vm-program-deopt-info
   #:vm-program-function-conventions
   #:vm-program-instructions
   #:vm-program-leaf-p
   #:vm-program-osr-entry-points
   #:vm-program-result-register
   #:vm-program-tier1-entry-points #:vm-program-load-time-value-cells)
(:export
   #:*FASL-CODE-VERSION*
   #:*FASL-PRELOAD-FORMS*
   #:*FASL-TOC-ENABLED*
   #:*JSON-NULL*
   #:*PGO-DATA-PATH*
   #:FASL-PAGE-FAULT
   #:FASL-TOC-READ
   #:JSON-PARSE
   #:JSON-PARSE-STREAM
   #:JSON-STRINGIFY
   #:JSON-STRINGIFY-STREAM
   #:LOAD-FASL-DEMAND-PAGED
   #:LOAD-PGO-DATA
   #:SAVE-PGO-DATA
   #:VM-COMPILE-FILE-TO-FASL
   #:VM-LOAD-FASL
   #:VM-MAKE-LOAD-FORM
   #:VM-MAKE-LOAD-FORM-SAVING-SLOTS
   #:VM-PRINT-UNREADABLE-OBJECT
   #:VM-READ-FROM-FASL
   #:VM-WRITE-TO-FASL
  )
  (:intern
   #:%PGO-READ-OBJECT
   #:%PGO-WRITE-OBJECT
   #:%VM-SERIALIZE-SLOT-NAMES
   #:+VM-FASL-MAGIC+
   #:FASL-TOC
  )
)
