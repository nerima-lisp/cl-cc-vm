;
(defpackage #:cl-cc/vm/execution
  (:use
    #:cl
  )
  (:import-from #:cl-cc/vm/protocols
    #:vm-value #:vm-index #:vm-args #:vm-handler-label #:vm-handler-result-reg #:vm-value-reg
    #:*instruction-constructors* #:*vm-eval-hook* #:*vm-guest-function-package* #:*vm-macroexpand-1-hook* #:*vm-macroexpand-hook*
    #:build-label-table #:define-simple-instruction #:define-vm-binary-instruction #:define-vm-instruction #:define-vm-unary-instruction #:execute-instruction
    #:instruction->sexp #:vm-dst #:vm-instruction #:vm-lhs #:vm-maybe-dump-type-profile #:vm-reg-get #:vm-reg-set #:vm-register-host-bridge #:vm-resolve-gf-method #:vm-rhs
    #:vm-src #:vm-state-heap #:vm-typep-check #:vm-values-list #:vm-decode-symbol #:vm-encode-common-symbol #:vm-immediate-symbol-p #:vm-trace-output-symbol #:vm-trace-output-stream #:%vm-dispatch-call
    #:%vm-method-function #:%vm-ret-qualified-dispatch #:*vm-self-host-mode* #:vm-call-with-readable-printing #:vm-create-io-state #:vm-environment-canary-value #:vm-execute-instruction-guarded #:vm-fatal-error-p #:vm-feature-initial-globals #:vm-load-pathname
    #:vm-lookup-bridge-callable #:vm-random-integer #:vm-resolve-hash-test #:vm-state-symbol-plists #:vm-state-system-symbol-plists #:vm-state-symbol-plist-lock #:vm-state-symbol-plist-read-barrier #:vm-check-index #:vm-check-row-major-index #:vm-closure-object-p
    #:vm-getenv #:vm-immediate-intern-enabled-p #:vm-safety-checks-enabled-p #:%vm-typep-call-predicate #:%vm-typep-range-check)
  (:import-from #:cl-cc/vm/data #:%execute-vm-gethash #:%execute-vm-specialized-gethash #:%vm-array-dimensions-designator #:%vm-array-object #:%vm-array-total-size #:%vm-array-vector-dimensions-p #:%vm-cow-list-ensure-writable #:%vm-cow-list-materialize #:%vm-cow-vector-ensure-writable #:%vm-cow-vector-materialize
    #:%vm-cow-vector-share #:%vm-host-package-local-nickname-function #:%vm-host-string #:%vm-list-structure-materialize #:%vm-make-displaced-array #:%vm-make-standard-array #:%vm-managed-cons-alloc #:%vm-managed-cons-car #:%vm-managed-cons-cdr #:%vm-managed-cons-materialize
    #:%vm-managed-cons-pointer-p #:%vm-managed-cons-set-slot #:%vm-managed-cons-slot #:%vm-managed-list-length #:%vm-managed-list-tail #:%vm-managed-member #:%vm-managed-nil-p #:%vm-managed-tree-materialize #:%vm-maybe-sso-string #:%vm-normalize-specialized-element-type
    #:%vm-package-operation-result #:%vm-package-result #:%vm-registry-result #:%vm-resolve-package-designator #:%vm-sso-string-p #:%vm-string-char #:%vm-string-length #:%vm-symbol-property-get #:%vm-symbol-property-remprop #:%vm-symbol-property-replace-plist
    #:%vm-symbol-property-set #:*vm-compound-type-handlers* #:*vm-managed-cons-allocation-enabled* #:*vm-managed-cons-sequence-state* #:*vm-primitive-type-dispatch* #:hash-test-symbol #:rt-make-hash-table #:rt-string-intern #:taint-mark #:vm-acons
    #:vm-add-package-local-nickname #:vm-adjust-array #:vm-adjustable #:vm-adjustable-reg #:vm-append #:vm-aref #:vm-aref-multi #:vm-array-dimension #:vm-array-dimension-value #:vm-array-displacement
    #:vm-array-in-bounds-p #:vm-array-in-bounds-p-value #:vm-array-length #:vm-array-reg #:vm-array-row-major-index #:vm-aset #:vm-assoc #:vm-atom #:vm-bit-access #:vm-bit-set
    #:vm-bit-vector-p #:vm-bit-vector-ref #:vm-butlast #:vm-car #:vm-car-reg #:vm-cdr #:vm-cdr-reg #:vm-char #:vm-clrhash #:vm-coerce-to-list
    #:vm-coerce-to-string #:vm-coerce-to-vector #:vm-concatenate #:vm-cons #:vm-cons-reg #:vm-copy-list #:vm-copy-tree #:vm-copy-vector #:vm-digit-char-p #:vm-digit-char-p-value
    #:vm-displaced-to-reg #:vm-dst-array-reg #:vm-element-type #:vm-element-type-reg #:vm-end #:vm-endp #:vm-equal #:vm-export-inst #:vm-fill #:vm-fill-pointer
    #:vm-fill-pointer-reg #:vm-find-package #:vm-find-symbol #:vm-gensym-inst #:vm-gethash #:vm-gethash-eq #:vm-gethash-eql #:vm-gethash-equal #:vm-hash-cons #:vm-hash-ensure-writable
    #:vm-hash-key #:vm-hash-table-count #:vm-hash-table-get-internal #:vm-hash-table-keys #:vm-hash-table-p #:vm-hash-table-reg #:vm-hash-table-test #:vm-hash-table-values #:vm-hash-test #:vm-hash-value
    #:vm-hash-weakness #:vm-hash-with-lock-fallback #:vm-import-inst #:vm-index-reg #:vm-initial-element #:vm-intern-pkg #:vm-intern-symbol #:vm-item-reg #:vm-keywordp #:vm-last
    #:vm-len-reg #:vm-length #:vm-list-all-packages-inst #:vm-list-length #:vm-list-reg #:vm-listp #:vm-local-nickname-nick #:vm-local-nickname-pkg #:vm-local-nickname-target #:vm-make-array
    #:vm-make-hash-table #:vm-make-list #:vm-make-specialized-array #:vm-make-string #:vm-member #:vm-nconc #:vm-nreverse #:vm-nth #:vm-nthcdr #:vm-null
    #:vm-package-name-inst #:vm-parts #:vm-pattern #:vm-pop #:vm-push #:vm-record-weak-hash-entry #:vm-remhash #:vm-remove-package-local-nickname #:vm-remprop #:vm-rest
    #:vm-reverse #:vm-row-major-aref #:vm-rplaca #:vm-rplacd #:vm-runtime-type-of #:vm-sbit #:vm-search-string #:vm-sequence-elt #:vm-sequence-length #:vm-set-fill-pointer
    #:vm-set-symbol-plist #:vm-sethash #:vm-shadow-inst #:vm-size-reg #:vm-specialized-array-length #:vm-specialized-array-p #:vm-specialized-array-ref #:vm-src-array-reg #:vm-start #:vm-str-reg
    #:vm-str1 #:vm-str2 #:vm-string-length #:vm-string-materialize #:vm-string-reg #:vm-string-set #:vm-string-set-char #:vm-string-subseq #:vm-subseq #:vm-subst
    #:vm-svref #:vm-svset #:vm-sweep-weak-hash-table #:vm-symbol-get #:vm-symbol-name #:vm-symbol-plist #:vm-symbol-plist-read-snapshot #:vm-symbol-set #:vm-unintern-inst #:vm-unuse-package-inst
    #:vm-use-package-inst #:vm-val-reg #:vm-vector #:vm-vector-push #:vm-vector-push-extend #:vm-vectorp #:vm-arr #:vm-copy-on-write #:vm-copy-on-write-reg #:vm-default
    #:vm-dims #:vm-displaced-index-offset-reg #:vm-element-regs #:vm-export-pkg #:vm-find-symbol-pkg #:vm-hash-table-object #:vm-import-pkg #:vm-import-symbols #:vm-index-regs #:vm-new-val
    #:vm-old-val #:vm-plist-reg #:vm-shadow-names #:vm-shadow-pkg #:vm-size #:vm-subs #:vm-tree #:vm-unintern-pkg #:vm-unintern-symbol #:vm-unuse-packages
    #:vm-unuse-pkg #:vm-use-packages #:vm-use-pkg #:vm-val #:vm-idx #:vm-cow-vector-p #:vm-dimensions-reg #:vm-export-symbols #:vm-hash-rehash-size #:vm-hash-rehash-threshold
    #:vm-hash-size #:vm-hash-table-weak-entries #:vm-package-name-pkg #:vm-assoc-key #:vm-assoc-alist #:vm-char-index #:vm-char-string #:vm-acons-key #:vm-acons-value #:vm-acons-alist #:vm-remprop-sym #:vm-remprop-indicator #:vm-symbol-get-sym #:vm-symbol-get-indicator #:vm-symbol-get-default #:vm-symbol-set-sym #:vm-symbol-set-indicator #:vm-symbol-set-value
    #:vm-sym)
  (:export #:%documentation-table* #:*adaptive-jit-enabled* #:*blind-constants-enabled* #:*bytecode-threshold* #:*deopt-enabled* #:*documentation-table* #:*jit-compilation-queue* #:*jit-hardening-enabled* #:*jit-random-key* #:*jit-tier1-threshold*
    #:*opcode-dispatch-table* #:*opcode-encoder-table* #:*opcode-name-table* #:*optimize-for-instruction-count* #:*osr-enabled* #:*pass-pipeline* #:*stack-segment-size* #:*stack-size* #:*tier-upgrade-threshold* #:*trampoline-optimization-enabled*
    #:*vm-current-state* #:*vm-eval-deadline* #:*vm-forward-reference-auto-resolve-enabled* #:*vm-instruction-profile* #:*vm-instruction-profile-enabled* #:*vm-recompile-function-hook* #:*vm-state* #:*vm-tier-call-counters* #:*vm-value-profile* #:+bc-call+
    #:+bc-closure+ #:+bc-const+ #:+bc-jmp+ #:+bc-jnil+ #:+bc-load+ #:+bc-make-env+ #:+bc-ret+ #:+bc-store+ #:+bc-tcall+ #:+maximum-multiple-values+
    #:+op2-add-imm2+ #:+op2-add2+ #:+op2-car+ #:+op2-cdr+ #:+op2-cons+ #:+op2-consp+ #:+op2-const+ #:+op2-const-halt2+ #:+op2-dec+ #:+op2-div+
    #:+op2-eq+ #:+op2-eql+ #:+op2-equal+ #:+op2-fixnump+ #:+op2-functionp+ #:+op2-get-global+ #:+op2-halt2+ #:+op2-hash-ref+ #:+op2-hash-set+ #:+op2-inc+
    #:+op2-jump+ #:+op2-jump-if-nil+ #:+op2-jump-if-true+ #:+op2-load-const+ #:+op2-load-fixnum+ #:+op2-load-nil+ #:+op2-load-true+ #:+op2-make-hash+ #:+op2-make-instance+ #:+op2-make-vector+
    #:+op2-mod+ #:+op2-move+ #:+op2-mul-imm2+ #:+op2-mul2+ #:+op2-neg+ #:+op2-nop+ #:+op2-num-eq-imm2+ #:+op2-num-eq2+ #:+op2-num-ge-imm2+ #:+op2-num-ge2+
    #:+op2-num-gt-imm2+ #:+op2-num-gt2+ #:+op2-num-le-imm2+ #:+op2-num-le2+ #:+op2-num-lt-imm2+ #:+op2-num-lt2+ #:+op2-recv-values+ #:+op2-return+ #:+op2-return-nil+ #:+op2-set-global+
    #:+op2-stringp+ #:+op2-sub-imm2+ #:+op2-sub2+ #:+op2-symbolp+ #:+op2-values+ #:+op2-vector-ref+ #:+op2-vector-set+ #:+vm-register-count+ #:add-imm2 #:add2
    #:as-pattern #:blind-aot-constant #:blind-constant #:bracket #:bytecode-disassemble #:check-stack-overflow-prologue #:cl-cc-do #:cmpxchg128 #:compile-time-test #:compile-to-bytecode
    #:const-halt2 #:copy-vm-program #:coroutine #:coroutine-resume #:coroutine-yield #:dec #:defcodata #:defgenerator #:define-syntax #:defopaque-type
    #:deftype-rec #:dequeue-jit-compilation #:detect-fusion-pairs #:div #:emit-cmov #:enqueue-jit-compilation #:fixnump #:get-global #:halt2 #:hash-ref
    #:hash-set #:inc #:initialize-jit-hardening #:install-stack-guard-page #:jump-if-nil #:jump-if-true #:list-of #:load-bytecode #:load-const #:load-fixnum
    #:load-nil #:load-true #:make-coroutine #:make-hash #:make-thunk #:make-vector #:make-vm-abort-to-prompt #:make-vm-and #:make-vm-apply #:make-vm-call
    #:make-vm-call-next-method #:make-vm-call-with-prompt #:make-vm-call/cc #:make-vm-clear-values #:make-vm-closure #:make-vm-closure-ref-idx #:make-vm-cons-p #:make-vm-const #:make-vm-declare-forward-reference #:make-vm-deopt
    #:make-vm-deopt-frame #:make-vm-deopt-info #:make-vm-ensure-values #:make-vm-eq #:make-vm-establish-catch #:make-vm-establish-handler #:make-vm-eval #:make-vm-forward-reference-cell #:make-vm-func-ref #:make-vm-function-p
    #:make-vm-get-global #:make-vm-halt #:make-vm-heap-address #:make-vm-instance #:make-vm-jump #:make-vm-jump-zero #:make-vm-label #:make-vm-load-time-value #:make-vm-load-time-value-cell #:make-vm-macroexpand-1-inst
    #:make-vm-macroexpand-inst #:make-vm-make-closure #:make-vm-move #:make-vm-mv-bind #:make-vm-mv-bind-regs #:make-vm-next-method-p #:make-vm-not #:make-vm-nth-value #:make-vm-null-p #:make-vm-or
    #:make-vm-osr-entry #:make-vm-parent-environment #:make-vm-prefetch #:make-vm-print #:make-vm-program #:make-vm-progv-enter #:make-vm-progv-exit #:make-vm-recompile #:make-vm-register-function #:make-vm-remove-handler
    #:make-vm-ret #:make-vm-select #:make-vm-set-global #:make-vm-signal-error #:make-vm-spread-values #:make-vm-state #:make-vm-symbol-p #:make-vm-sync-handler-regs #:make-vm-tail-call #:make-vm-throw
    #:make-vm-trampoline #:make-vm-type-check #:make-vm-type-of #:make-vm-typep #:make-vm-values #:make-vm-values-regs #:make-vm-values-to-list #:make-vm-values-typep #:make-vm2-state #:move
    #:mul-imm2 #:mul2 #:neg #:num-eq-imm2 #:num-eq2 #:num-ge-imm2 #:num-ge2 #:num-gt-imm2 #:num-gt2 #:num-le-imm2
    #:num-le2 #:num-lt-imm2 #:num-lt2 #:pack #:preserve-analysis #:recv-values #:return-nil #:run-bytecode #:run-compiled #:run-program-slice
    #:set-global #:specialize-loop-count #:specialize-simd #:splice #:split-hot-cold #:static-assert #:sub-imm2 #:sub2 #:the-value #:trampoline
    #:transfer-to #:transfer-value #:unpack #:vector-ref #:vector-set #:version-function #:version-loops-with-alias-checks #:vm-abort-to-prompt #:vm-acas-addr #:vm-acas-expected
    #:vm-acas-newval #:vm-aincf-addr #:vm-aincf-delta #:vm-aload-addr #:vm-and #:vm-apply #:vm-arg-slot-name #:vm-args #:vm-astore-addr #:vm-astore-val
    #:vm-atomic-cas #:vm-atomic-incf #:vm-atomic-load #:vm-atomic-store #:vm-atomic-swap #:vm-bind-arg-slots #:vm-bind-closure-args #:vm-build-list #:vm-bytecode-function #:vm-bytecode-function-constant-pool
    #:vm-bytecode-function-env-size #:vm-bytecode-function-name #:vm-bytecode-function-opcode-bytes #:vm-bytecode-function-p #:vm-call #:vm-call-next-method #:vm-call-next-method-args-reg #:vm-call-p #:vm-call-stack #:vm-call-with-prompt
    #:vm-call/cc #:vm-capture-continuation #:vm-capture-deopt-frame #:vm-captured-vars #:vm-catch-handler-label #:vm-catch-result-reg #:vm-catch-tag-reg #:vm-class-registry #:vm-closure #:vm-closure-captured-regs
    #:vm-closure-captured-vals #:vm-closure-compilation-tier #:vm-closure-entry-label #:vm-closure-env #:vm-closure-index #:vm-closure-inline-policy #:vm-closure-inst-dispatch-tag #:vm-closure-invocation-count #:vm-closure-key-params #:vm-closure-label-table
    #:vm-closure-object #:vm-closure-optional-params #:vm-closure-p #:vm-closure-params #:vm-closure-program-flat #:vm-closure-ref-idx #:vm-closure-reg #:vm-closure-rest-param #:vm-closure-rest-stack-alloc-p #:vm-compile-osr-entry-if-hot
    #:vm-cons-cell #:vm-cons-cell-car #:vm-cons-cell-cdr #:vm-cons-p #:vm-const #:vm-const-p #:vm-const-value #:vm-continuation #:vm-continuation-p #:vm-current-deopt-frame
    #:vm-current-state #:vm-declare-forward-reference #:vm-deopt #:vm-deopt-frame #:vm-deopt-frame-call-stack #:vm-deopt-frame-closure-env #:vm-deopt-frame-info #:vm-deopt-frame-inline-stack #:vm-deopt-frame-pc #:vm-deopt-frame-physical-registers
    #:vm-deopt-frame-reason #:vm-deopt-frame-registers #:vm-deopt-frame-values-list #:vm-deopt-history #:vm-deopt-id #:vm-deopt-info #:vm-deopt-info-description #:vm-deopt-info-env #:vm-deopt-info-inline-stack #:vm-deopt-info-label
    #:vm-deopt-info-live-regs #:vm-deopt-info-pc #:vm-deopt-info-vreg->preg #:vm-deopt-label #:vm-deopt-reason #:vm-dispatch-lock-free #:vm-dst-regs #:vm-env-regs #:vm-eq #:vm-error-reg
    #:vm-error-type #:vm-error-type-matches-p #:vm-establish-catch #:vm-establish-handler #:vm-eval #:vm-eval-deadline-exceeded #:vm-falsep #:vm-forward-reference-cell #:vm-forward-reference-cell-name #:vm-forward-reference-cell-p
    #:vm-forward-reference-cell-ref #:vm-forward-reference-name #:vm-func-name #:vm-func-ref #:vm-func-ref-dispatch-tag #:vm-func-reg #:vm-function-p #:vm-function-registry #:vm-generic-function-p #:vm-get-global
    #:vm-get-global-dst #:vm-get-global-name #:vm-get-global-p #:vm-get-profile-bb-counts #:vm-get-profile-branch-counts #:vm-get-profile-call-counts #:vm-get-profile-call-times #:vm-get-profile-inst-counts #:vm-get-profile-samples #:vm-get-profile-type-feedback
    #:vm-global-name #:vm-global-vars #:vm-halt #:vm-handler-entry-saved-regs #:vm-handler-label #:vm-handler-result-reg #:vm-handler-stack #:vm-heap-address #:vm-heap-address-p #:vm-heap-address-value
    #:vm-heap-alloc #:vm-heap-counter #:vm-heap-get #:vm-heap-object #:vm-heap-set #:vm-index #:vm-instance-function-value #:vm-instance-global-value #:vm-instance-lock #:vm-instance-parent-env
    #:vm-instruction-profile-alist #:vm-invoke-continuation #:vm-jump #:vm-jump-p #:vm-jump-zero #:vm-label #:vm-label-name #:vm-label-p #:vm-label-table-lookup #:vm-label-table-store
    #:vm-lbl-name #:vm-list-to-lisp-list #:vm-load-fence #:vm-load-time-value #:vm-load-time-value-cell #:vm-load-time-value-cell-form #:vm-load-time-value-cell-id #:vm-load-time-value-cell-read-only-p #:vm-load-time-value-cell-resolved-p #:vm-load-time-value-cell-value
    #:vm-load-time-values #:vm-macroexpand-1-inst #:vm-macroexpand-inst #:vm-make-closure #:vm-make-closure-p #:vm-make-closure-params #:vm-materialize-osr-frame #:vm-memory-barrier #:vm-method-call-stack #:vm-move
    #:vm-move-dst #:vm-move-p #:vm-move-src #:vm-mv-bind #:vm-mv-bind-regs #:vm-mv-bind-regs-count #:vm-mv-buffer #:vm-mv-count #:vm-name #:vm-next-method-p
    #:vm-not #:vm-nth-value #:vm-null-p #:vm-or #:vm-osr-entry #:vm-osr-id #:vm-osr-label #:vm-osr-register-map #:vm-osr-stub-enter #:vm-output-stream
    #:vm-parent-environment #:vm-parent-environment-functions #:vm-parent-environment-globals #:vm-parent-environment-symbols #:vm-prefetch #:vm-prefetch-base-reg #:vm-prefetch-index-reg #:vm-prefetch-kind #:vm-prefetch-locality #:vm-prefetch-offset
    #:vm-prefetch-scale #:vm-print #:vm-print-backtrace #:vm-profile-call-stack #:vm-profile-call-start-times #:vm-profile-call-times #:vm-profile-enabled-p #:vm-profile-inst-counts #:vm-profile-inst-hit #:vm-profile-samples
    #:vm-program #:vm-program-calling-convention #:vm-program-compilation-tier #:vm-program-deopt-info #:vm-program-function-conventions #:vm-program-instructions #:vm-program-leaf-p #:vm-program-osr-entry-points #:vm-program-p #:vm-program-result-register
    #:vm-program-tier1-entry-points #:vm-prompt-reg #:vm-push-call-frame #:vm-recompile #:vm-reconstruct-interpreter-frame #:vm-record-value-profile #:vm-reg #:vm-register-function #:vm-register-tier1-osr-entry #:vm-remove-handler
    #:vm-reset-instruction-profile #:vm-resolve-forward-references #:vm-resolve-function #:vm-restore-registers #:vm-restore-registers-subset #:vm-ret #:vm-ret-p #:vm-save-registers #:vm-save-registers-subset #:vm-select
    #:vm-select-cond-reg #:vm-select-else-reg #:vm-select-then-reg #:vm-set-global #:vm-set-global-name #:vm-set-global-p #:vm-set-global-src #:vm-signal-error #:vm-spread-values #:vm-src-regs
    #:vm-state #:vm-state-registers #:vm-store-fence #:vm-symbol-p #:vm-sync-handler-regs #:vm-tail-call #:vm-tail-p #:vm-throw #:vm-throw-tag-reg #:vm-throw-value-reg
    #:vm-tier-hot-p #:vm-tier-note-call #:vm-tier-reset-counters #:vm-tier1-code #:vm-trampoline #:vm-trigger-deopt #:vm-type-check #:vm-type-check-deopt-id #:vm-type-check-deopt-label #:vm-type-name
    #:vm-type-of #:vm-typecase-dispatch #:vm-typep #:vm-typep-type-name #:vm-value #:vm-value-profile-snapshot #:vm-value-reg #:vm-values #:vm-values-regs #:vm-values-to-list
    #:vm-values-typep #:vm-var-name #:vm-vr-count #:vm-vr0 #:vm-vr1 #:vm-vr2 #:vm-write-instruction-profile #:vm2-state-global-vars #:vm2-state-output-stream #:vm2-state-p
    #:vm2-state-registers #:vm2-state-values-buffer #:with-vm-instance-lock #:with-vm-state-dispatch
   #:vm-trampoline-thunk-p #:vm-force-trampoline-result #:%vm-call-closure-sync #:%vm-closure-object-p #:vm-closure-dispatch-tag #:vm-current-closure-stack #:vm-initialize-class-registry)
  (:intern
    #:%bc-emit #:%bc-read-word #:%make-vm2-fast-opcode-handler-table #:%opcode-make-vector #:%resolve-direct-function-designator #:%resolve-symbol-function-designator #:%run-vm-core #:%set-vm-handler-entry-saved-regs #:%set-vm-profile-call-stack #:%set-vm-profile-call-start-times
    #:%vm-apply-spread-args #:%vm-call-type-refinement-accessor #:%vm-callable-registry-entry-p #:%vm-copy-register-table #:%vm-current-deopt-info #:%vm-decode-immediate-symbol-for-eql #:%vm-deopt-resume-pc #:%vm-frame-argument-values
    #:%vm-initialize-package-global #:%vm-label-for-return-pc #:%vm-label-table-entries #:%vm-parent-hash #:%vm-php-array-values-list #:%vm-profile-call-stack #:%vm-profile-call-start-times #:%vm-profile-enabled-p #:%vm-profile-finish-call #:%vm-profile-now-ns
    #:%vm-replace-stack-segments #:%vm-stack-segment-snapshot #:%vm-tier1-osr-target-pc #:%vm-type-refinement-accessor #:%vm-type-refinement-components #:%vm-typep-clos-class #:%vm-typep-clos-instance-p #:%vm-typep-normalize-sym #:%vm-unwind-to-handler #:%vm-values-type-parts
    #:%vm2-build-fusion-table #:%vm2-emit4 #:%vm2-fast-op-add-imm2 #:%vm2-fast-op-const #:%vm2-fast-op-const-halt2 #:%vm2-fast-op-halt2 #:%vm2-fast-op-move #:%vm2-fast-op-mul-imm2 #:%vm2-fast-op-sub-imm2 #:*max-call-stack-depth*
    #:*safety-level* #:*security-canaries* #:*tls-stack-limit* #:*vm-current-compilation-tier* #:*vm-current-program-deopt-info* #:*vm-current-program-osr-entry-points* #:*vm-direct-function-designator-resolvers* #:*vm-exec-flat* #:*vm-exec-labels* #:*vm-immediate-symbol-indexes*
    #:*vm-immediate-symbol-table* #:*vm-initial-globals* #:*vm-instance-locks* #:*vm-instance-parent-envs* #:*vm-package-initial-globals* #:*vm-safepoint-heap* #:*vm-unresolved-forward-refs* #:*vm2-const-fusion-table* #:*vm2-fast-opcode-handler-symbols* #:*vm2-fast-opcode-handler-table*
    #:*vm2-fusion-opcode-symbol-pairs* #:+vm-arg-slot-count+ #:+vm-immediate-symbol-base+ #:+vm-immediate-symbol-index-mask+ #:+vm-immediate-symbol-mask+ #:call-with-current-continuation #:defopcode #:defopcode-binary-fn #:defopcode-binary-fn-imm #:defopcode-binary-pred
    #:defopcode-binary-pred-imm #:defopcode-load-literal #:defopcode-unary-fn #:defopcode-unary-pred #:defopcode-unary-type-pred #:memory-fault #:vm-check-call-stack-depth #:vm-closure-note-invocation #:vm-maybe-tier-upgrade-closure
    #:vm-mutex
    #:MAKE-MUTEX #:vm-pop-call-frame #:vm-profile-bb-hit #:vm-profile-branch-edge #:vm-profile-enter-call #:vm-profile-return #:vm-profile-sample #:vm-random-canary #:vm-rwlock #:MAKE-RWLOCK #:vm-safepoint-poll
    #:vm-safety-level #:vm-stack-overflow #:vm-stack-protection-enabled-p #:vm-trampoline-thunk #:vm-values-typep-check #:vm-verify-stack-canary #:vm2-collect-opcode-bigrams #:vm2-fuse-immediate-superinstructions #:vm2-state #:*vm-current-dst-reg*
    #:*vm-current-labels* #:*vm-current-pc* #:vm-current-stack-segment #:vm-program-load-time-value-cells #:vm-symbol-plists #:vm-system-symbol-plists #:vm-symbol-plist-lock #:vm-symbol-plist-read-barrier #:vm-clos-shadow-stack #:VM-CONTINUATION-PROMPTS #:vm-tail-call-optimization-enabled-p #:vm-store-multiple-values
  )
)

