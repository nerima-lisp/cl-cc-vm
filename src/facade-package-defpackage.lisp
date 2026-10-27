;;;; packages/vm/src/facade-package.lisp -- cl-cc/vm package definition
;;;;
;;;; VM instruction set subsystem: instruction defstructs, VM state, heap
;;;; operations, I/O, conditions, CLOS dispatch, hash tables, strings, etc.
;;;;
;;;; Extracted as a Phase 2 sibling system (:cl-cc-vm). The package
;;;; facade is loaded first (no dependencies); the umbrella :cl-cc system
;;;; then sets up (use-package :cl-cc :cl-cc/vm) so the source files
;;;; can access all symbols unqualified.
;;;;
;;;; Export lists are organized by domain in separate files:
;;;;   exports-instructions.lisp  — instruction types, constructors, predicates
;;;;   exports-runtime.lisp       — VM state, I/O, strings, hash, CLOS, macros
;;;;   exports-opcodes.lisp       — opcode constants, VM2 state, dispatch tables
;;;;   exports-conditions.lisp    — condition types, handlers, misc symbols

(defpackage #:cl-cc/vm
  (:use
    #:cl
  )
  (:import-from #:cl-cc/vm/protocols
    #:*instruction-constructors* #:*vm-compile-string-hook* #:*vm-eval-hook* #:*vm-host-bridge-functions* #:*vm-macroexpand-1-hook* #:*vm-macroexpand-hook* #:*vm-parse-forms-hook* #:build-label-table #:define-simple-instruction #:define-vm-binary-instruction
    #:define-vm-char-comparison #:define-vm-instruction #:define-vm-unary-instruction #:execute-instruction #:instruction->sexp #:make-vm-instruction #:sexp->instruction #:vm-char1 #:vm-char2 #:vm-dst
    #:vm-instruction #:vm-lhs #:vm-maybe-dump-type-profile #:vm-reg-get #:vm-reg-set #:vm-register-host-bridge #:vm-resolve-gf-method #:vm-rhs #:vm-src #:vm-state-heap
    #:vm-typep-check #:vm-values-list #:vm-decode-symbol #:vm-encode-common-symbol #:vm-immediate-symbol-p    )
  (:import-from #:cl-cc/vm/execution #:%documentation-table* #:*adaptive-jit-enabled* #:*blind-constants-enabled* #:*bytecode-threshold* #:*deopt-enabled* #:*documentation-table* #:*jit-compilation-queue* #:*jit-hardening-enabled* #:*jit-random-key* #:*jit-tier1-threshold*
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
    #:vm2-state-registers #:vm2-state-values-buffer #:with-vm-instance-lock #:with-vm-state-dispatch #:%vm-callable-registry-entry-p #:vm-current-stack-segment #:vm-program-load-time-value-cells #:*vm-immediate-symbol-table* #:vm-current-closure-stack #:%vm-closure-object-p #:vm-closure-dispatch-tag #:%vm-call-closure-sync)
  (:import-from #:cl-cc/vm/numbers
    #:*arith-dispatch-table* #:*associative-math* #:*fast-math* #:*floating-point-modes* #:*fuse-fma* #:*numeric-contagion-table* #:*numeric-precision* #:*vm-arithmetic-safety* #:+arith-operation-tags+ #:+arith-type-tags+
    #:+numeric-contagion-types+ #:arithmetic-dispatch-entry #:arithmetic-dispatch-index #:arithmetic-op-tag #:arithmetic-type-tag #:bfloat16 #:bswap #:clamp #:dd* #:dd+
    #:dd-to-string #:double-double #:double-double-hi #:double-double-lo #:double-double-p #:fixnum-add #:fixnum-mul #:fixnum-p #:fixnum-value #:float16
    #:fma #:get-float-traps #:infer-numeric-result-type #:inline-arithmetic-dispatch #:interval #:interval-add #:lerp #:make-bfloat16 #:make-double-double #:make-fixnum
    #:make-float16 #:make-interval #:make-vm-abs #:make-vm-acos-inst #:make-vm-acosh-inst #:make-vm-add #:make-vm-add-checked #:make-vm-arith-dispatch #:make-vm-ash #:make-vm-asin-inst
    #:make-vm-asinh-inst #:make-vm-atan-inst #:make-vm-atan2-inst #:make-vm-atanh-inst #:make-vm-bignump #:make-vm-binop #:make-vm-bswap #:make-vm-ceiling-inst #:make-vm-cl-div #:make-vm-complex
    #:make-vm-complexp #:make-vm-conjugate #:make-vm-cos-inst #:make-vm-cosh-inst #:make-vm-dec #:make-vm-decode-float #:make-vm-denominator #:make-vm-div #:make-vm-evenp #:make-vm-exp-inst
    #:make-vm-expt #:make-vm-fceiling #:make-vm-ffloor #:make-vm-float-add #:make-vm-float-digits #:make-vm-float-div #:make-vm-float-inst #:make-vm-float-mul #:make-vm-float-precision #:make-vm-float-radix
    #:make-vm-float-sign #:make-vm-float-sub #:make-vm-floatp #:make-vm-floor-inst #:make-vm-fma #:make-vm-fround #:make-vm-ftruncate #:make-vm-gcd #:make-vm-ge #:make-vm-generic-add
    #:make-vm-generic-div #:make-vm-generic-eq #:make-vm-generic-gt #:make-vm-generic-lt #:make-vm-generic-mul #:make-vm-generic-sub #:make-vm-gt #:make-vm-imagpart #:make-vm-inc #:make-vm-integer-add
    #:make-vm-integer-decode-float #:make-vm-integer-length #:make-vm-integer-mul #:make-vm-integer-mul-high-s #:make-vm-integer-mul-high-u #:make-vm-integer-p #:make-vm-integer-sub #:make-vm-lcm #:make-vm-le #:make-vm-lisp-implementation-type
    #:make-vm-lisp-implementation-version #:make-vm-log-inst #:make-vm-logand #:make-vm-logbitp #:make-vm-logcount #:make-vm-logeqv #:make-vm-logior #:make-vm-lognot #:make-vm-logtest #:make-vm-logxor
    #:make-vm-long-site-name #:make-vm-lt #:make-vm-machine-instance #:make-vm-machine-type #:make-vm-machine-version #:make-vm-max #:make-vm-min #:make-vm-mod #:make-vm-mul #:make-vm-mul-checked
    #:make-vm-neg #:make-vm-num-eq #:make-vm-number-p #:make-vm-numerator #:make-vm-oddp #:make-vm-phase #:make-vm-quire #:make-vm-rational #:make-vm-rationalize #:make-vm-rationalp
    #:make-vm-realp #:make-vm-realpart #:make-vm-rem #:make-vm-rotate #:make-vm-round-inst #:make-vm-scale-float #:make-vm-short-site-name #:make-vm-simd-vector-op #:make-vm-sin-inst #:make-vm-sinh-inst
    #:make-vm-software-type #:make-vm-software-version #:make-vm-sqrt #:make-vm-sub #:make-vm-sub-checked #:make-vm-tan-inst #:make-vm-tanh-inst #:make-vm-truncate #:numeric-contagion-rank #:numeric-contagion-result-type
    #:numeric-contagion-type-of #:rotate-right #:swar-and #:vm-a #:vm-abs #:vm-acos-inst #:vm-add #:vm-add-checked #:vm-add-p #:vm-arith-dispatch
    #:vm-arith-dispatch-op #:vm-ash #:vm-asin-inst #:vm-atan-inst #:vm-b #:vm-bignump #:vm-binop #:vm-bswap #:vm-c #:vm-ceiling-inst
    #:vm-cl-div #:vm-clamp #:vm-complexp #:vm-cos-inst #:vm-dec #:vm-denominator #:vm-div #:vm-exp-inst #:vm-fixnum-add-with-overflow-detection #:vm-fixnum-mul-with-overflow-detection
    #:vm-fixnum-overflow-detected-p #:vm-fixnum-sub-with-overflow-detection #:vm-float-add #:vm-float-div #:vm-float-mul #:vm-float-precision #:vm-float-sign #:vm-float-sub #:vm-float-to-string #:vm-floatp
    #:vm-floor-inst #:vm-fma #:vm-gcd #:vm-ge #:vm-generic-add #:vm-generic-div #:vm-generic-eq #:vm-generic-gt #:vm-generic-lt #:vm-generic-mul
    #:vm-generic-sub #:vm-gt #:vm-inc #:vm-integer-add #:vm-integer-length #:vm-integer-mul #:vm-integer-mul-high-s #:vm-integer-mul-high-u #:vm-integer-p #:vm-integer-sub
    #:vm-lcm #:vm-le #:vm-lerp #:vm-log-inst #:vm-logand #:vm-logbitp #:vm-logcount #:vm-logeqv #:vm-logior #:vm-lognot
    #:vm-logtest #:vm-logxor #:vm-lt #:vm-max #:vm-min #:vm-mod #:vm-mul #:vm-mul-checked #:vm-neg #:vm-neg-p
    #:vm-num-eq #:vm-number-p #:vm-numerator #:vm-posit #:vm-posit-add #:vm-posit-bits #:vm-posit-decode #:vm-posit-div #:vm-posit-encode #:vm-posit-es
    #:vm-posit-from-bits #:vm-posit-mul #:vm-posit-nar-p #:vm-posit-nbits #:vm-posit-p #:vm-posit-sub #:vm-posit-zero-p #:vm-quire #:vm-quire-add-product! #:vm-quire-es
    #:vm-quire-nbits #:vm-quire-p #:vm-quire-to-posit #:vm-quire-value #:vm-rational #:vm-rationalize #:vm-rationalp #:vm-realp #:vm-rem #:vm-rotate
    #:vm-round-inst #:vm-ryu-double-to-string #:vm-ryu-float-to-string #:vm-ryu-single-to-string #:vm-signum #:vm-simd-vector-op #:vm-simd-vector-op-dst-array #:vm-simd-vector-op-element-type #:vm-simd-vector-op-index-reg #:vm-simd-vector-op-lanes
    #:vm-simd-vector-op-lhs-array #:vm-simd-vector-op-op #:vm-simd-vector-op-p #:vm-simd-vector-op-rhs-array #:vm-sin-inst #:vm-sqrt #:vm-sub #:vm-sub-checked #:vm-sub-p #:vm-tan-inst
    #:vm-truncate #:vm-wrap #:vpshufb #:with-float-traps-masked #:with-precision #:wrap #:vm-bignum-add #:vm-bignum-gcd #:vm-bignum-mul #:vm-bignum-negate
    #:vm-bignum-sub #:vm-bignum-to-integer #:vm-integer->bignum    )
  (:shadowing-import-from #:cl-cc/vm/clos
    #:%eql-specializer-matches-p #:%eql-specializer-p #:%resolve-combination-operator #:%vm-extract-eql-specializer-keys #:%vm-gf-eql-methods #:%vm-gf-uses-composite-keys-p #:%vm-resolve-single-dispatch #:*collect-profile* #:*method-combination-operators* #:*profile-output*
    #:collect-inherited-initargs #:collect-inherited-slots #:compute-class-precedence-list #:deep-copy #:generic-function-method-combination #:generic-function-methods #:make-vm-class-def #:make-vm-class-name-fn #:make-vm-class-of-fn #:make-vm-compute-effective-slot-definition #:make-vm-find-class
    #:make-vm-generic-call #:make-vm-make-obj #:make-vm-register-method #:make-vm-slot-boundp #:make-vm-slot-exists-p #:make-vm-slot-makunbound #:make-vm-slot-read #:make-vm-slot-write #:satiate-generic-function #:satiating-gfs-p
    #:slot-definition-name #:vm-class-def #:vm-class-name-fn #:vm-class-name-sym #:vm-class-of-fn #:vm-class-reg #:vm-class-slots #:vm-classify-arg #:vm-compute-effective-slot-definition #:vm-dump-type-profile
    #:vm-find-class #:vm-generic-call #:vm-generic-call-metadata #:vm-gf-reg #:vm-ic-type-counters #:vm-initarg-regs #:vm-make-obj #:vm-method-qualifier #:vm-method-reg #:vm-method-specializer
    #:vm-obj-reg #:vm-pgo-specializer #:vm-register-method #:vm-sealed-p #:vm-slot-boundp #:vm-slot-exists-p #:vm-slot-initargs #:vm-slot-makunbound #:vm-slot-name-sym #:vm-slot-names
    #:vm-slot-read #:vm-slot-read-dst #:vm-slot-read-obj-reg #:vm-slot-read-slot-name #:vm-slot-types #:vm-slot-write #:vm-slot-write-obj-reg #:vm-slot-write-slot-name #:vm-slot-write-value-reg #:vm-superclasses
    #:vm-try-dispatch-combinations #:vm-try-dispatch-sub #:vm-type-profile-alist #:with-satiating-gfs #:%lookup-qualified-methods #:%vm-direct-primary-method-p #:%vm-global-generic-function #:%vm-same-function-name-p #:vm-get-all-applicable-methods    )
  (:import-from #:cl-cc/vm/conditions
    #:*active-restarts* #:*dynamic-wind-stack* #:*vm-error-output-format* #:*vm-program-exception-tables* #:describe-restart #:dynamic-wind #:format-condition-json #:format-rich-condition #:format-stack-trace #:make-vm-cerror
    #:make-vm-division-by-zero #:make-vm-exception-entry #:make-vm-signal #:make-vm-simple-error #:make-vm-simple-warning #:make-vm-type-error #:make-vm-type-error-condition #:make-vm-unbound-variable #:make-vm-undefined-function #:make-vm-warn
    #:restart-interactive #:retry #:vm-abort-restart #:vm-add-restart #:vm-arithmetic-error #:vm-bind-restart #:vm-call-with-dynamic-wind-context #:vm-cell-error #:vm-cerror #:vm-check-type
    #:vm-clear-condition-context #:vm-compute-active-restarts #:vm-compute-restarts #:vm-condition #:vm-condition-error-code #:vm-condition-fix-it #:vm-condition-state #:vm-control-error #:vm-datum-reg #:vm-define-condition
    #:vm-did-you-mean #:vm-dividend #:vm-division-by-zero #:vm-dynamic-wind #:vm-dynamic-wind-frame #:vm-dynamic-wind-frame-after-thunk #:vm-dynamic-wind-frame-before-thunk #:vm-dynamic-wind-frame-p #:vm-dynamic-wind-reenter #:vm-end-of-file
    #:vm-error #:vm-error-instruction #:vm-exception-entry-condition-type #:vm-exception-entry-end-pc #:vm-exception-entry-handler-pc #:vm-exception-entry-result-reg #:vm-exception-entry-start-pc #:vm-expected-type #:vm-fatal-error #:vm-find-handler
    #:vm-find-restart #:vm-find-standard-restart #:vm-floating-point-overflow #:vm-floating-point-underflow #:vm-get-handler-stack #:vm-get-restarts #:vm-handler-case #:vm-handler-type #:vm-invoke-restart #:vm-invoke-restart-interactively
    #:vm-levenshtein-distance #:vm-muffle-warning-restart #:vm-package-error #:vm-pop-handler #:vm-pop-handler-from-stack #:vm-program-error #:vm-push-handler #:vm-push-handler-to-stack #:vm-push-handler-type #:vm-reader-error
    #:vm-register-program-exception-table #:vm-restart-label #:vm-restart-name #:vm-restart-name-inst #:vm-retry-restart #:vm-serious-condition #:vm-show-restart-menu #:vm-signal #:vm-signal-condition #:vm-simple-condition
    #:vm-simple-error #:vm-simple-warning #:vm-storage-condition #:vm-store-value-restart #:vm-stream-error #:vm-style-warning #:vm-type-error #:vm-type-error-condition #:vm-type-error-values-p #:vm-unbound-slot
    #:vm-unbound-variable #:vm-undefined-function #:vm-use-value-restart #:vm-warn #:vm-warning #:vm-with-simple-restart    )
  (:import-from #:cl-cc/vm/data
    #:*deferred-rc-enabled* #:*hash-table-rehash-policy* #:*interpolation-prefix* #:*perfect-hash-tables-enabled* #:*rt-string-dedup-table* #:*string-intern-table* #:*vm-hash-cons-table* #:*vm-primitive-type-dispatch* #:+hash-table-rehash-policies+ #:+sso-max-length+
    #:assoc! #:build-perfect-hash-table #:conj #:conj! #:detect-rc-cycles #:dissoc #:dissoc! #:hash-table-simd-lookup-available-p #:hash-with #:hcons
    #:hhead #:hnil #:htail #:intern-string #:iterate #:lazy-filter #:lazy-force #:lazy-map #:lazy-range #:lazy-seq
    #:lazy-seq-p #:lazy-take-seq #:make-lazy-seq #:make-mphf-table #:make-rope #:make-sso-string #:make-string-builder #:make-vm-acons #:make-vm-add-package-local-nickname #:make-vm-adjust-array
    #:make-vm-adjustable-array-p #:make-vm-alpha-char-p #:make-vm-alphanumericp #:make-vm-append #:make-vm-aref #:make-vm-aref-multi #:make-vm-array-adjustable-p #:make-vm-array-dimension #:make-vm-array-dimensions #:make-vm-array-displacement
    #:make-vm-array-element-type #:make-vm-array-has-fill-pointer-p #:make-vm-array-in-bounds-p #:make-vm-array-length #:make-vm-array-rank #:make-vm-array-row-major-index #:make-vm-array-total-size #:make-vm-aset #:make-vm-assoc #:make-vm-atom
    #:make-vm-bit-access #:make-vm-bit-and #:make-vm-bit-not #:make-vm-bit-or #:make-vm-bit-set #:make-vm-bit-xor #:make-vm-both-case-p #:make-vm-butlast #:make-vm-car #:make-vm-cdr
    #:make-vm-char #:make-vm-char-code #:make-vm-char-downcase #:make-vm-char-equal #:make-vm-char-greaterp #:make-vm-char-lessp #:make-vm-char-name #:make-vm-char-not-equal #:make-vm-char-not-greaterp #:make-vm-char-not-lessp
    #:make-vm-char-upcase #:make-vm-char/= #:make-vm-char< #:make-vm-char<= #:make-vm-char= #:make-vm-char> #:make-vm-char>= #:make-vm-characterp #:make-vm-clrhash #:make-vm-code-char
    #:make-vm-coerce-to-list #:make-vm-coerce-to-string #:make-vm-coerce-to-vector #:make-vm-complement #:make-vm-concatenate #:make-vm-cons #:make-vm-constantly #:make-vm-copy-list #:make-vm-copy-tree #:make-vm-copy-vector
    #:make-vm-digit-char #:make-vm-digit-char-p #:make-vm-dynamic #:make-vm-eighth #:make-vm-endp #:make-vm-equal #:make-vm-export-inst #:make-vm-fifth #:make-vm-fill #:make-vm-fill-pointer-inst
    #:make-vm-find-package #:make-vm-find-symbol #:make-vm-first #:make-vm-fourth #:make-vm-gensym-inst #:make-vm-gethash #:make-vm-gethash-eq #:make-vm-gethash-eql #:make-vm-gethash-equal #:make-vm-graphic-char-p
    #:make-vm-hash-cons #:make-vm-hash-table-count #:make-vm-hash-table-keys #:make-vm-hash-table-p #:make-vm-hash-table-rehash-size #:make-vm-hash-table-rehash-threshold #:make-vm-hash-table-size #:make-vm-hash-table-test #:make-vm-hash-table-values #:make-vm-identity
    #:make-vm-import-inst #:make-vm-intern-symbol #:make-vm-keywordp #:make-vm-last #:make-vm-length #:make-vm-list-all-packages-inst #:make-vm-list-length #:make-vm-listp #:make-vm-lower-case-p #:make-vm-make-array
    #:make-vm-make-hash-table #:make-vm-make-list #:make-vm-make-string #:make-vm-make-symbol #:make-vm-member #:make-vm-name-char #:make-vm-nbutlast #:make-vm-nconc #:make-vm-ninth #:make-vm-nreconc
    #:make-vm-nreverse #:make-vm-nstring-capitalize #:make-vm-nstring-downcase #:make-vm-nstring-upcase #:make-vm-nth #:make-vm-nthcdr #:make-vm-null #:make-vm-package-name-inst #:make-vm-parse-float #:make-vm-parse-integer #:make-vm-pop
    #:make-vm-push #:make-vm-remhash #:make-vm-remove-package-local-nickname #:make-vm-remprop #:make-vm-rest #:make-vm-reverse #:make-vm-row-major-aref #:make-vm-rplaca #:make-vm-rplacd #:make-vm-sbit
    #:make-vm-search-string #:make-vm-second #:make-vm-set-fill-pointer #:make-vm-set-symbol-plist #:make-vm-sethash #:make-vm-seventh #:make-vm-shadow-inst #:make-vm-simple-vector-p #:make-vm-sixth #:make-vm-standard-char-p
    #:make-vm-string #:make-vm-string-capitalize #:make-vm-string-coerce #:make-vm-string-downcase #:make-vm-string-equal #:make-vm-string-greaterp #:make-vm-string-left-trim #:make-vm-string-length #:make-vm-string-lessp #:make-vm-string-not-equal
    #:make-vm-string-not-greaterp #:make-vm-string-not-lessp #:make-vm-string-right-trim #:make-vm-string-set #:make-vm-string-trim #:make-vm-string-upcase #:make-vm-string< #:make-vm-string<= #:make-vm-string= #:make-vm-string>
    #:make-vm-string>= #:make-vm-stringp #:make-vm-subseq #:make-vm-subst #:make-vm-svref #:make-vm-svset #:make-vm-sxhash #:make-vm-symbol-get #:make-vm-symbol-name #:make-vm-symbol-plist
    #:make-vm-symbol-set #:make-vm-tenth #:make-vm-third #:make-vm-type-rep #:make-vm-unintern-inst #:make-vm-unuse-package-inst #:make-vm-upper-case-p #:make-vm-use-package-inst #:make-vm-vector #:make-vm-vector-pop
    #:make-vm-vector-push #:make-vm-vector-push-extend #:make-vm-vectorp #:mphf-hash #:mphf-lookup #:mphf-table #:persistent! #:persistent-map #:persistent-map-count #:persistent-map-p
    #:persistent-map-test #:pget #:pmap-assoc #:pmap-get #:pvec #:pvec->list #:pvec-assoc #:pvec-conj #:pvec-count #:pvec-get
    #:refine #:rope #:rope-concat #:rope-length #:rope-p #:rope-split #:rope-to-string #:rt-hash-table-weakness #:rt-make-hash-table #:rt-string-dedup
    #:rt-string-intern #:sso-string-char #:sso-string-length #:sso-string-p #:string-builder #:string-builder-append! #:string-builder-finish #:string-builder-p #:string-contains-simd #:string-interned-p
    #:string-to-utf8-bytes #:transient #:transient! #:transient-hash #:transient-hash-p #:transient-p #:unicode-alpha-char-p #:unicode-alphanumericp #:unicode-char-code #:unicode-char-downcase
    #:unicode-char-upcase #:unicode-code-char #:unicode-digit-char-p #:unicode-general-category #:unicode-lower-case-p #:unicode-normalize #:unicode-normalize-nfc #:unicode-normalize-nfd #:unicode-normalize-nfkc #:unicode-normalize-nfkd
    #:unicode-normalize-string #:unicode-syntax-class #:unicode-upper-case-p #:utf8-bytes-to-string #:valid-hash-table-rehash-policy-p #:vm-add-package-local-nickname #:vm-adjust-sequence #:vm-adjustable #:vm-adjustable-reg #:vm-alpha-char-p
    #:vm-alphanumericp #:vm-append #:vm-aref #:vm-aref-multi #:vm-array-adjustable-p #:vm-array-has-fill-pointer-p #:vm-array-length #:vm-array-reg #:vm-array-row-major-index #:vm-aset
    #:vm-bit-vector-p #:vm-bit-vector-ref #:vm-both-case-p #:vm-butlast #:vm-car #:vm-car-reg #:vm-cast-with-type-rep #:vm-cdr #:vm-cdr-reg #:vm-char
    #:vm-char-bag #:vm-char-code #:vm-char< #:vm-char= #:vm-clear-hash-cons-table #:vm-clrhash #:vm-code-char #:vm-coerce-to-list #:vm-coerce-to-string #:vm-coerce-to-vector
    #:vm-complement #:vm-concatenate #:vm-cons #:vm-cons-reg #:vm-constantly #:vm-copy-vector #:vm-digit-char-p #:vm-displaced-to-reg #:vm-dst-array-reg #:vm-dynamic
    #:vm-dynamic-p #:vm-dynamic-type-rep #:vm-dynamic-value #:vm-eighth #:vm-element-type #:vm-element-type-reg #:vm-end #:vm-endp #:vm-fifth #:vm-fill
    #:vm-fill-pointer #:vm-fill-pointer-reg #:vm-first #:vm-found-dst #:vm-fourth #:vm-gethash #:vm-gethash-default #:vm-gethash-eq #:vm-gethash-eql #:vm-gethash-equal
    #:vm-graphic-char-p #:vm-hash-cons #:vm-hash-cons-p #:vm-hash-default #:vm-hash-fn #:vm-hash-key #:vm-hash-table-count #:vm-hash-table-internal #:vm-hash-table-keys #:vm-hash-table-object
    #:vm-hash-table-p #:vm-hash-table-reg #:vm-hash-table-rehash-size #:vm-hash-table-rehash-threshold #:vm-hash-table-size #:vm-hash-table-test #:vm-hash-table-values #:vm-hash-table-weakness #:vm-hash-test #:vm-hash-value
    #:vm-hash-weakness #:vm-identity #:vm-index-reg #:vm-initial-element #:vm-intern-pkg #:vm-item-reg #:vm-last #:vm-len-reg #:vm-length #:vm-list-length
    #:vm-list-reg #:vm-listp #:vm-local-nickname-nick #:vm-local-nickname-pkg #:vm-local-nickname-target #:vm-lower-case-p #:vm-make-array #:vm-make-array-adjustable #:vm-make-array-fill-pointer #:vm-make-hash-table
    #:vm-make-hash-table-test #:vm-make-list #:vm-make-sequence-like #:vm-make-specialized-array #:vm-make-string #:vm-make-string-char #:vm-member #:vm-nbutlast #:vm-ninth #:vm-nreconc
    #:vm-nstring-capitalize #:vm-nstring-downcase #:vm-nstring-upcase #:vm-nth #:vm-nthcdr #:vm-null #:vm-parts #:vm-pattern #:vm-pop #:vm-push
    #:vm-random-state #:vm-random-state-p #:vm-remhash #:vm-remove-package-local-nickname #:vm-rest #:vm-reverse #:vm-rplaca #:vm-rplacd #:vm-runtime-type-of #:vm-search-string
    #:vm-second #:vm-sequence-elt #:vm-sequence-length #:vm-sethash #:vm-seventh #:vm-simple-vector-p #:vm-sixth #:vm-size-reg #:vm-specialized-array #:vm-specialized-array-element-pointer-free-p
    #:vm-specialized-array-element-type #:vm-specialized-array-gc-skip-p #:vm-specialized-array-header #:vm-specialized-array-length #:vm-specialized-array-p #:vm-specialized-array-ref #:vm-specialized-array-storage #:vm-src-array-reg #:vm-standard-char-p #:vm-start
    #:vm-str-reg #:vm-str1 #:vm-str2 #:vm-string-capitalize #:vm-string-coerce #:vm-string-downcase #:vm-string-equal #:vm-string-greaterp #:vm-string-left-trim #:vm-string-length
    #:vm-string-lessp #:vm-string-not-equal #:vm-string-reg #:vm-string-right-trim #:vm-string-set #:vm-string-trim #:vm-string-upcase #:vm-string< #:vm-string<= #:vm-string=
    #:vm-string> #:vm-string>= #:vm-stringp #:vm-subseq #:vm-svref #:vm-sxhash #:vm-tenth #:vm-third #:vm-type-rep #:vm-type-rep-equal
    #:vm-type-rep-of #:vm-type-rep-p #:vm-type-rep-specifier #:vm-unwrap-dynamic #:vm-upper-case-p #:vm-val-reg #:vm-vector #:vm-vectorp #:vm-wrap-dynamic #:with-string-builder
    #:vm-string-materialize
    )
  (:shadowing-import-from #:cl-cc/vm/data
    #:random-state #:random-state-p
  )
  (:import-from #:cl-cc/vm/io
    #:*default-external-format* #:*vm-load-global-sync-hook* #:+eof-value+ #:+stdin-handle+ #:+stdout-handle+ #:clone-vm-state #:decode-external-format #:defsession #:dns-async-result #:dns-async-result-done-p
    #:dns-async-result-error #:dns-async-result-p #:dns-async-result-result #:dns-resolve #:dns-resolve-async #:dns-reverse-resolve #:encode-external-format #:fundamental-binary-input-stream #:fundamental-binary-output-stream #:fundamental-binary-stream
    #:fundamental-character-input-stream #:fundamental-character-output-stream #:fundamental-character-stream #:fundamental-stream #:get-pprint-dispatch #:getaddrinfo #:handle-signal #:make-pprint-dispatch-table #:make-readtable #:make-socket-stream
    #:make-tcp-socket #:make-tls-context #:make-udp-socket #:make-vm-clear-input #:make-vm-clear-output #:make-vm-close-file #:make-vm-eof-p #:make-vm-file-length #:make-vm-file-position #:make-vm-finish-output
    #:make-vm-force-output #:make-vm-format-inst #:make-vm-fresh-line-inst #:make-vm-get-output-stream-string-inst #:make-vm-get-string-from-stream #:make-vm-input-stream-p #:make-vm-interactive-stream-p #:make-vm-listen-inst #:make-vm-load-file #:make-vm-make-pathname
    #:make-vm-make-string-output-stream-inst #:make-vm-make-string-stream #:make-vm-namestring #:make-vm-open-file #:make-vm-open-stream-p #:make-vm-output-stream-p #:make-vm-pathname #:make-vm-pathname-name #:make-vm-pathname-type #:make-vm-pathnamep
    #:make-vm-peek-char #:make-vm-prin1 #:make-vm-prin1-sexp #:make-vm-princ #:make-vm-princ-sexp #:make-vm-princ-to-string-inst #:make-vm-print-inst #:make-vm-print-sexp #:make-vm-read-byte #:make-vm-read-char
    #:make-vm-read-from-string-inst #:make-vm-read-from-string-rp #:make-vm-read-line #:make-vm-read-sexp #:make-vm-read-sexp-inst #:make-vm-reader-advance-rp #:make-vm-reader-peek-rp #:make-vm-reader-read #:make-vm-stream-element-type-inst #:make-vm-stream-write-string-inst
    #:make-vm-streamp #:make-vm-terpri-inst #:make-vm-unread-char #:make-vm-write-byte #:make-vm-write-char #:make-vm-write-line #:make-vm-write-string #:make-vm-write-to-string-inst #:make-vm-write-to-string-rp #:mmap-advice
    #:mmap-array #:mmap-close #:mmap-file #:mmap-sync #:pprint-dispatch-table #:pprint-dispatch-table-p #:read-balanced-form #:run-compiled-with-io #:run-string-with-io #:sendfile
    #:shm-open #:socket-accept #:socket-bind #:socket-close #:socket-connect #:socket-listen #:socket-local-address #:socket-receive #:socket-send #:stream-advance-to-column
    #:stream-clear-input #:stream-clear-output #:stream-finish-output #:stream-force-output #:stream-fresh-line #:stream-line-column #:stream-listen #:stream-peek-char #:stream-read-byte #:stream-read-char
    #:stream-read-char-no-hang #:stream-read-line #:stream-start-line-p #:stream-terpri #:stream-unread-char #:stream-write-byte #:stream-write-char #:stream-write-string #:tls-server-context #:tls-socket-stream
    #:tls-unsupported #:tls-wrap-socket #:vm-allocate-file-handle #:vm-char-reg #:vm-close-file #:vm-decode-bytes #:vm-encode-string #:vm-eof-p #:vm-file-counter #:vm-file-direction
    #:vm-file-handle #:vm-file-length #:vm-file-position #:vm-format-inst #:vm-fresh-line-inst #:vm-get-output-stream-string-inst #:vm-get-stream #:vm-get-string-from-stream #:vm-initial-string #:vm-input-stream-p
    #:vm-interactive-stream-p #:vm-io-state #:vm-listen-inst #:vm-make-string-output-stream-inst #:vm-make-string-stream #:vm-make-string-stream-direction #:vm-open #:vm-open-file #:vm-open-file-direction #:vm-open-files
    #:vm-open-stream-p #:vm-output-stream-p #:vm-peek-char #:vm-position-reg #:vm-prin1 #:vm-prin1-sexp #:vm-princ #:vm-princ-sexp #:vm-princ-to-string-inst #:vm-print-inst
    #:vm-print-sexp #:vm-read-byte #:vm-read-char #:vm-read-from-string-inst #:vm-read-line #:vm-read-sexp #:vm-read-sexp-inst #:vm-reader-read #:vm-set-stream-external-format #:vm-socket
    #:vm-socket-p #:vm-standard-input #:vm-standard-output #:vm-stream-direction #:vm-stream-external-format #:vm-stream-open-p #:vm-stream-write-string-inst #:vm-string-streams #:vm-terpri-inst #:vm-unread-char
    #:vm-write-char #:vm-write-string #:vm-write-to-string-inst #:with-mmap #:with-socket #:%vm-format-native    )
  (:shadowing-import-from #:cl-cc/vm/io
    #:*print-array* #:*print-base* #:*print-case* #:*print-circle* #:*print-escape* #:*print-gensym* #:*print-length* #:*print-level* #:*print-lines* #:*print-pprint-dispatch*
    #:*print-pretty* #:*print-radix* #:*print-readably* #:*print-right-margin* #:*readtable* #:copy-pprint-dispatch #:copy-readtable #:get-dispatch-macro-character #:get-macro-character #:open
    #:pprint-indent #:pprint-logical-block #:pprint-newline #:pprint-tab #:readtable-case #:set-dispatch-macro-character #:set-macro-character #:set-pprint-dispatch #:stream-external-format #:with-standard-io-syntax
  )
  (:import-from #:cl-cc/vm/serialization
    #:*fasl-code-version* #:*fasl-preload-forms* #:*fasl-toc-enabled* #:*json-null* #:*pgo-data-path* #:fasl-page-fault #:fasl-toc-read #:json-parse #:json-parse-stream #:json-stringify
    #:json-stringify-stream #:load-fasl-demand-paged #:load-pgo-data #:save-pgo-data #:vm-compile-file-to-fasl #:vm-load-fasl #:vm-make-load-form #:vm-make-load-form-saving-slots #:vm-print-unreadable-object #:vm-read-from-fasl
    #:vm-write-to-fasl
  )
  (:import-from #:cl-cc/vm/environment
    #:*command-line-args* #:*command-line-arguments* #:*compile-shared* #:*compile-static* #:*current-source-location* #:*feature-flags* #:*liquid-types-enabled* #:*source-location-map* #:*target-kernel-module* #:*target-unikernel*
    #:audit-dependencies #:crypto-random #:defmodule #:defvar-tls #:exit #:generate-from-schema #:generate-lockfile #:getenv #:make-source-location #:make-source-span
    #:make-vm-boundp #:make-vm-decode-universal-time #:make-vm-encode-universal-time #:make-vm-fboundp #:make-vm-fdefinition #:make-vm-get-internal-real-time #:make-vm-get-internal-run-time #:make-vm-get-universal-time #:make-vm-make-random-state #:make-vm-makunbound
    #:make-vm-random #:make-vm-set-symbol-value #:make-vm-sleep-inst #:make-vm-source-location #:runtime-tuning-report #:setenv #:source-location #:source-location-column #:source-location-file #:source-location-form
    #:source-location-line #:source-location-p #:source-location-pathname #:source-span #:vm-attach-source-location #:vm-boundp #:vm-condition-source-location #:vm-decode-universal-time #:vm-encode-universal-time #:vm-fboundp
    #:vm-fdefinition #:vm-get-internal-real-time #:vm-get-internal-run-time #:vm-get-source-location #:vm-get-universal-time #:vm-handle-runtime-flag #:vm-make-random-state #:vm-makunbound #:vm-random #:vm-record-gc-pause
    #:vm-signal-with-location #:vm-source-context-lines #:vm-source-location #:vm-source-location-column #:vm-source-location-compilation-unit #:vm-source-location-form #:vm-source-location-line #:vm-source-location-macro-context #:vm-source-location-p #:vm-source-location-pathname
    #:vm-source-location-string #:when-feature #:with-timeout #:check-package-lock #:vm-lock-package #:vm-unlock-package
    )
  (:shadowing-import-from #:cl-cc/vm/environment
    #:*features* #:*load-pathname* #:*load-truename* #:*random-state* #:*read-base* #:*read-default-float-format* #:*read-eval* #:*read-suppress* #:apropos #:apropos-list
    #:decode-universal-time #:encode-universal-time #:get-internal-real-time #:get-internal-run-time #:get-universal-time #:internal-time-units-per-second #:lisp-implementation-type #:lisp-implementation-version #:machine-instance #:machine-type
    #:machine-version #:make-random-state #:random #:room #:sleep #:software-type #:software-version #:time    )
  (:import-from #:cl-cc/vm/debugging
    #:*repl-journal* #:*repl-syntax-colors* #:analyze-assertion-density #:bisect-regression #:extract-docstrings #:generate-api-docs #:macrostep #:objdump #:profile-values #:read-pmu-counters
    #:repl-complete #:repl-undo #:report-stack-overflow #:run-doctests #:show-types #:start-cpu-profile #:vm-ansi-color #:vm-ansi-reset #:vm-apropos #:vm-apropos-list
    #:vm-break #:vm-describe #:vm-dribble #:vm-ed #:vm-inspect #:vm-invoke-debugger #:vm-isatty #:vm-read-key #:vm-readline #:vm-room
    #:vm-step #:vm-step-prompt #:vm-terminal-size #:vm-trace #:vm-traced-p #:vm-untrace #:vm-with-raw-terminal #:vm-with-trace    )
  (:shadowing-import-from #:cl-cc/vm/debugging
    #:*trace-output* #:break #:describe #:dribble #:ed #:inspect #:invoke-debugger #:step #:trace #:untrace
  )
  (:import-from #:cl-cc/vm/interop #:%all-symbols #:%class-slot-definitions #:%class-slot-initargs-for-slot #:%class-slot-metadata #:%make-instances-obsolete #:%package-external-symbols #:%package-symbols #:*code-sign-identity* #:*sandbox-enabled* #:*seccomp-filter-rules*
    #:*shape-registry* #:*value-types* #:+imt-size+ #:aes-encrypt #:class-metaclass #:compute-effective-slot-definition #:defvalue-type #:emit-header #:export-c-api #:find-or-create-shape
    #:foreign-call-varargs #:hash-table-values #:imt-entry #:imt-insert #:imt-lookup #:initialize-sandbox #:inline-method-table #:make-callback #:make-imt-entry #:make-inline-method-table
    #:make-mark-word #:make-object-shape #:mark-word-gc-color #:object-shape #:octets-to-string #:regex-all-matches #:regex-replace #:regex-replace-all #:regex-scan #:regex-split
    #:register-gc-root #:resolve-hash-test #:sha256-rounds #:shape-slot-offset #:sign-binary #:slot-definition-allocation #:slot-definition-initargs #:slot-definition-initform #:slot-definition-initfunction #:slot-definition-location
    #:slot-definition-type #:string-to-octets #:value-type-def #:with-pinned-object #:with-platform #:vm-bridge-callable #:%vm-runtime-callable)
  (:import-from #:cl-cc/vm/clos #:%vm-fixed-slot-layout-p #:%vm-hashlike-storage #:%vm-raw-allocate-instance #:%vm-slot-vector-index #:%vm-vector-instance-p #:*unbound-slot-marker* #:class-slots #:copy-instance)
  (:import-from #:cl
    #:*break-on-signals* #:*debugger-hook* #:abort #:arithmetic-error #:arithmetic-error-operands #:arithmetic-error-operation #:cell-error #:cell-error-name #:check-type #:compute-restarts
    #:condition #:continue #:control-error #:division-by-zero #:end-of-file #:error #:find-restart #:floating-point-overflow #:floating-point-underflow #:invoke-restart
    #:muffle-warning #:package-error #:package-error-package #:program-error #:reader-error #:restart-bind #:restart-case #:restart-name #:serious-condition #:signum
    #:simple-condition-format-arguments #:simple-condition-format-control #:simple-error #:simple-warning #:storage-condition #:store-value #:stream-error #:stream-error-stream #:style-warning #:type-error
    #:type-error-datum #:type-error-expected-type #:unbound-slot #:unbound-slot-instance #:unbound-variable #:undefined-function #:use-value #:warning #:with-simple-restart
  )
  (:import-from #:cl-cc/bootstrap
    #:const
  )
  (:export #:%all-symbols #:%class-slot-definitions #:%class-slot-initargs-for-slot #:%class-slot-metadata #:%documentation-table* #:%eql-specializer-matches-p #:%eql-specializer-p #:%make-instances-obsolete #:%package-external-symbols #:%package-symbols
    #:%resolve-combination-operator #:%vm-extract-eql-specializer-keys #:%vm-gf-eql-methods #:%vm-gf-uses-composite-keys-p #:%vm-resolve-single-dispatch #:*active-restarts* #:*adaptive-jit-enabled* #:*arith-dispatch-table* #:*associative-math* #:*blind-constants-enabled*
    #:*break-on-signals* #:*bytecode-threshold* #:*code-sign-identity* #:*collect-profile* #:*command-line-args* #:*command-line-arguments* #:*compile-shared* #:*compile-static* #:*current-source-location* #:*debugger-hook*
    #:*default-external-format* #:*deferred-rc-enabled* #:*deopt-enabled* #:*documentation-table* #:*dynamic-wind-stack* #:*fasl-code-version* #:*fasl-preload-forms* #:*fasl-toc-enabled* #:*fast-math* #:*feature-flags*
    #:*features* #:*floating-point-modes* #:*fuse-fma* #:*hash-table-rehash-policy* #:*instruction-constructors* #:*interpolation-prefix* #:*jit-compilation-queue* #:*jit-hardening-enabled* #:*jit-random-key* #:*jit-tier1-threshold*
    #:*json-null* #:*liquid-types-enabled* #:*load-pathname* #:*load-truename* #:*method-combination-operators* #:*numeric-contagion-table* #:*numeric-precision* #:*opcode-dispatch-table* #:*opcode-encoder-table* #:*opcode-name-table*
    #:*optimize-for-instruction-count* #:*osr-enabled* #:*pass-pipeline* #:*perfect-hash-tables-enabled* #:*pgo-data-path* #:*print-array* #:*print-base* #:*print-case* #:*print-circle* #:*print-escape*
    #:*print-gensym* #:*print-length* #:*print-level* #:*print-lines* #:*print-pprint-dispatch* #:*print-pretty* #:*print-radix* #:*print-readably* #:*print-right-margin* #:*profile-output*
    #:*random-state* #:*read-base* #:*read-default-float-format* #:*read-eval* #:*read-suppress* #:*readtable* #:*repl-journal* #:*repl-syntax-colors* #:*rt-string-dedup-table* #:*sandbox-enabled*
    #:*seccomp-filter-rules* #:*shape-registry* #:*source-location-map* #:*stack-segment-size* #:*stack-size* #:*string-intern-table* #:*target-kernel-module* #:*target-unikernel* #:*tier-upgrade-threshold* #:*trace-output*
    #:*trampoline-optimization-enabled* #:*value-types* #:*vm-arithmetic-safety* #:*vm-compile-string-hook* #:*vm-current-state* #:*vm-error-output-format* #:*vm-eval-deadline* #:*vm-eval-hook* #:*vm-forward-reference-auto-resolve-enabled* #:*vm-hash-cons-table*
    #:*vm-host-bridge-functions* #:*vm-instruction-profile* #:*vm-instruction-profile-enabled* #:*vm-load-global-sync-hook* #:*vm-macroexpand-1-hook* #:*vm-macroexpand-hook* #:*vm-parse-forms-hook* #:*vm-primitive-type-dispatch* #:*vm-program-exception-tables* #:*vm-recompile-function-hook*
    #:*vm-state* #:*vm-tier-call-counters* #:*vm-value-profile* #:+arith-operation-tags+ #:+arith-type-tags+ #:+bc-call+ #:+bc-closure+ #:+bc-const+ #:+bc-jmp+ #:+bc-jnil+
    #:+bc-load+ #:+bc-make-env+ #:+bc-ret+ #:+bc-store+ #:+bc-tcall+ #:+eof-value+ #:+hash-table-rehash-policies+ #:+imt-size+ #:+maximum-multiple-values+ #:+numeric-contagion-types+
    #:+op2-add-imm2+ #:+op2-add2+ #:+op2-car+ #:+op2-cdr+ #:+op2-cons+ #:+op2-consp+ #:+op2-const+ #:+op2-const-halt2+ #:+op2-dec+ #:+op2-div+
    #:+op2-eq+ #:+op2-eql+ #:+op2-equal+ #:+op2-fixnump+ #:+op2-functionp+ #:+op2-get-global+ #:+op2-halt2+ #:+op2-hash-ref+ #:+op2-hash-set+ #:+op2-inc+
    #:+op2-jump+ #:+op2-jump-if-nil+ #:+op2-jump-if-true+ #:+op2-load-const+ #:+op2-load-fixnum+ #:+op2-load-nil+ #:+op2-load-true+ #:+op2-make-hash+ #:+op2-make-instance+ #:+op2-make-vector+
    #:+op2-mod+ #:+op2-move+ #:+op2-mul-imm2+ #:+op2-mul2+ #:+op2-neg+ #:+op2-nop+ #:+op2-num-eq-imm2+ #:+op2-num-eq2+ #:+op2-num-ge-imm2+ #:+op2-num-ge2+
    #:+op2-num-gt-imm2+ #:+op2-num-gt2+ #:+op2-num-le-imm2+ #:+op2-num-le2+ #:+op2-num-lt-imm2+ #:+op2-num-lt2+ #:+op2-recv-values+ #:+op2-return+ #:+op2-return-nil+ #:+op2-set-global+
    #:+op2-stringp+ #:+op2-sub-imm2+ #:+op2-sub2+ #:+op2-symbolp+ #:+op2-values+ #:+op2-vector-ref+ #:+op2-vector-set+ #:+sso-max-length+ #:+stdin-handle+ #:+stdout-handle+
    #:+vm-register-count+ #:abort #:add-imm2 #:add2 #:aes-encrypt #:analyze-assertion-density #:apropos #:apropos-list #:arithmetic-dispatch-entry #:arithmetic-dispatch-index
    #:arithmetic-error #:arithmetic-error-operands #:arithmetic-error-operation #:arithmetic-op-tag #:arithmetic-type-tag #:as-pattern #:assoc! #:audit-dependencies #:bfloat16 #:bisect-regression
    #:blind-aot-constant #:blind-constant #:bracket #:break #:bswap #:build-label-table #:build-perfect-hash-table #:bytecode-disassemble #:cell-error #:cell-error-name
    #:check-stack-overflow-prologue #:check-type #:cl-cc-do #:clamp #:class-metaclass #:clone-vm-state #:cmpxchg128 #:collect-inherited-initargs #:collect-inherited-slots #:compile-time-test
    #:compile-to-bytecode #:compute-class-precedence-list #:compute-effective-slot-definition #:compute-restarts #:condition #:conj #:conj! #:const #:const-halt2 #:continue
    #:control-error #:copy-pprint-dispatch #:copy-readtable #:copy-vm-program #:coroutine #:coroutine-resume #:coroutine-yield #:crypto-random #:dd* #:dd+
    #:dd-to-string #:dec #:decode-external-format #:decode-universal-time #:deep-copy #:defcodata #:defgenerator #:define-simple-instruction #:define-syntax #:define-vm-binary-instruction #:define-vm-char-comparison
    #:define-vm-instruction #:define-vm-unary-instruction #:defmodule #:defopaque-type #:defsession #:deftype-rec #:defvalue-type #:defvar-tls #:dequeue-jit-compilation #:describe
    #:describe-restart #:detect-fusion-pairs #:detect-rc-cycles #:dissoc #:dissoc! #:div #:division-by-zero #:dns-async-result #:dns-async-result-done-p #:dns-async-result-error
    #:dns-async-result-p #:dns-async-result-result #:dns-resolve #:dns-resolve-async #:dns-reverse-resolve #:double-double #:double-double-hi #:double-double-lo #:double-double-p #:dribble
    #:dynamic-wind #:ed #:emit-cmov #:emit-header #:encode-external-format #:encode-universal-time #:end-of-file #:enqueue-jit-compilation #:error #:execute-instruction
    #:exit #:export-c-api #:extract-docstrings #:fasl-page-fault #:fasl-toc-read #:find-or-create-shape #:find-restart #:fixnum-add #:fixnum-mul #:fixnum-p
    #:fixnum-value #:fixnump #:float16 #:floating-point-overflow #:floating-point-underflow #:fma #:foreign-call-varargs #:format-condition-json #:format-rich-condition #:format-stack-trace
    #:fundamental-binary-input-stream #:fundamental-binary-output-stream #:fundamental-binary-stream #:fundamental-character-input-stream #:fundamental-character-output-stream #:fundamental-character-stream #:fundamental-stream #:generate-api-docs #:generate-from-schema #:generate-lockfile
    #:generic-function-method-combination #:generic-function-methods #:get-dispatch-macro-character #:get-float-traps #:get-global #:get-internal-real-time #:get-internal-run-time #:get-macro-character #:get-pprint-dispatch #:get-universal-time
    #:getaddrinfo #:getenv #:halt2 #:handle-signal #:hash-ref #:hash-set #:hash-table-simd-lookup-available-p #:hash-table-values #:hash-with #:hcons
    #:hhead #:hnil #:htail #:imt-entry #:imt-insert #:imt-lookup #:inc #:infer-numeric-result-type #:initialize-jit-hardening #:initialize-sandbox
    #:inline-arithmetic-dispatch #:inline-method-table #:inspect #:install-stack-guard-page #:instruction->sexp #:intern-string #:internal-time-units-per-second #:interval #:interval-add #:invoke-debugger
    #:invoke-restart #:iterate #:json-parse #:json-parse-stream #:json-stringify #:json-stringify-stream #:jump-if-nil #:jump-if-true #:lazy-filter #:lazy-force
    #:lazy-map #:lazy-range #:lazy-seq #:lazy-seq-p #:lazy-take-seq #:lerp #:lisp-implementation-type #:lisp-implementation-version #:list-of #:load-bytecode
    #:load-const #:load-fasl-demand-paged #:load-fixnum #:load-nil #:load-pgo-data #:load-true #:machine-instance #:machine-type #:machine-version #:macrostep
    #:make-bfloat16 #:make-callback #:make-coroutine #:make-double-double #:make-fixnum #:make-float16 #:make-hash #:make-imt-entry #:make-inline-method-table #:make-interval
    #:make-lazy-seq #:make-mark-word #:make-mphf-table #:make-object-shape #:make-pprint-dispatch-table #:make-random-state #:make-readtable #:make-rope #:make-socket-stream #:make-source-location
    #:make-source-span #:make-sso-string #:make-string-builder #:make-tcp-socket #:make-thunk #:make-tls-context #:make-udp-socket #:make-vector #:make-vm-abort-to-prompt #:make-vm-abs
    #:make-vm-acons #:make-vm-acos-inst #:make-vm-acosh-inst #:make-vm-add #:make-vm-add-checked #:make-vm-add-package-local-nickname #:make-vm-adjust-array #:make-vm-adjustable-array-p #:make-vm-alpha-char-p #:make-vm-alphanumericp
    #:make-vm-and #:make-vm-append #:make-vm-apply #:make-vm-aref #:make-vm-aref-multi #:make-vm-arith-dispatch #:make-vm-array-adjustable-p #:make-vm-array-dimension #:make-vm-array-dimensions #:make-vm-array-displacement
    #:make-vm-array-element-type #:make-vm-array-has-fill-pointer-p #:make-vm-array-in-bounds-p #:make-vm-array-length #:make-vm-array-rank #:make-vm-array-row-major-index #:make-vm-array-total-size #:make-vm-aset #:make-vm-ash #:make-vm-asin-inst
    #:make-vm-asinh-inst #:make-vm-assoc #:make-vm-atan-inst #:make-vm-atan2-inst #:make-vm-atanh-inst #:make-vm-atom #:make-vm-bignump #:make-vm-binop #:make-vm-bit-access #:make-vm-bit-and
    #:make-vm-bit-not #:make-vm-bit-or #:make-vm-bit-set #:make-vm-bit-xor #:make-vm-both-case-p #:make-vm-boundp #:make-vm-bswap #:make-vm-butlast #:make-vm-call #:make-vm-call-next-method
    #:make-vm-call-with-prompt #:make-vm-call/cc #:make-vm-car #:make-vm-cdr #:make-vm-ceiling-inst #:make-vm-cerror #:make-vm-char #:make-vm-char-code #:make-vm-char-downcase #:make-vm-char-equal
    #:make-vm-char-greaterp #:make-vm-char-lessp #:make-vm-char-name #:make-vm-char-not-equal #:make-vm-char-not-greaterp #:make-vm-char-not-lessp #:make-vm-char-upcase #:make-vm-char/= #:make-vm-char< #:make-vm-char<=
    #:make-vm-char= #:make-vm-char> #:make-vm-char>= #:make-vm-characterp #:make-vm-cl-div #:make-vm-class-def #:make-vm-class-name-fn #:make-vm-class-of-fn #:make-vm-clear-input #:make-vm-clear-output
    #:make-vm-clear-values #:make-vm-close-file #:make-vm-closure #:make-vm-closure-ref-idx #:make-vm-clrhash #:make-vm-code-char #:make-vm-coerce-to-list #:make-vm-coerce-to-string #:make-vm-coerce-to-vector #:make-vm-complement
    #:make-vm-complex #:make-vm-complexp #:make-vm-compute-effective-slot-definition #:make-vm-concatenate #:make-vm-conjugate #:make-vm-cons #:make-vm-cons-p #:make-vm-const #:make-vm-constantly #:make-vm-copy-list
    #:make-vm-copy-tree #:make-vm-copy-vector #:make-vm-cos-inst #:make-vm-cosh-inst #:make-vm-dec #:make-vm-declare-forward-reference #:make-vm-decode-float #:make-vm-decode-universal-time #:make-vm-denominator #:make-vm-deopt
    #:make-vm-deopt-frame #:make-vm-deopt-info #:make-vm-digit-char #:make-vm-digit-char-p #:make-vm-div #:make-vm-division-by-zero #:make-vm-dynamic #:make-vm-eighth #:make-vm-encode-universal-time #:make-vm-endp
    #:make-vm-ensure-values #:make-vm-eof-p #:make-vm-eq #:make-vm-equal #:make-vm-establish-catch #:make-vm-establish-handler #:make-vm-eval #:make-vm-evenp #:make-vm-exception-entry #:make-vm-exp-inst
    #:make-vm-export-inst #:make-vm-expt #:make-vm-fboundp #:make-vm-fceiling #:make-vm-fdefinition #:make-vm-ffloor #:make-vm-fifth #:make-vm-file-length #:make-vm-file-position #:make-vm-fill
    #:make-vm-fill-pointer-inst #:make-vm-find-class #:make-vm-find-package #:make-vm-find-symbol #:make-vm-finish-output #:make-vm-first #:make-vm-float-add #:make-vm-float-digits #:make-vm-float-div #:make-vm-float-inst
    #:make-vm-float-mul #:make-vm-float-precision #:make-vm-float-radix #:make-vm-float-sign #:make-vm-float-sub #:make-vm-floatp #:make-vm-floor-inst #:make-vm-fma #:make-vm-force-output #:make-vm-format-inst
    #:make-vm-forward-reference-cell #:make-vm-fourth #:make-vm-fresh-line-inst #:make-vm-fround #:make-vm-ftruncate #:make-vm-func-ref #:make-vm-function-p #:make-vm-gcd #:make-vm-ge #:make-vm-generic-add
    #:make-vm-generic-call #:make-vm-generic-div #:make-vm-generic-eq #:make-vm-generic-gt #:make-vm-generic-lt #:make-vm-generic-mul #:make-vm-generic-sub #:make-vm-gensym-inst #:make-vm-get-global #:make-vm-get-internal-real-time
    #:make-vm-get-internal-run-time #:make-vm-get-output-stream-string-inst #:make-vm-get-string-from-stream #:make-vm-get-universal-time #:make-vm-gethash #:make-vm-gethash-eq #:make-vm-gethash-eql #:make-vm-gethash-equal #:make-vm-graphic-char-p #:make-vm-gt
    #:make-vm-halt #:make-vm-hash-cons #:make-vm-hash-table-count #:make-vm-hash-table-keys #:make-vm-hash-table-p #:make-vm-hash-table-rehash-size #:make-vm-hash-table-rehash-threshold #:make-vm-hash-table-size #:make-vm-hash-table-test #:make-vm-hash-table-values
    #:make-vm-heap-address #:make-vm-identity #:make-vm-imagpart #:make-vm-import-inst #:make-vm-inc #:make-vm-input-stream-p #:make-vm-instance #:make-vm-instruction #:make-vm-integer-add #:make-vm-integer-decode-float
    #:make-vm-integer-length #:make-vm-integer-mul #:make-vm-integer-mul-high-s #:make-vm-integer-mul-high-u #:make-vm-integer-p #:make-vm-integer-sub #:make-vm-interactive-stream-p #:make-vm-intern-symbol #:make-vm-jump #:make-vm-jump-zero
    #:make-vm-keywordp #:make-vm-label #:make-vm-last #:make-vm-lcm #:make-vm-le #:make-vm-length #:make-vm-lisp-implementation-type #:make-vm-lisp-implementation-version #:make-vm-list-all-packages-inst #:make-vm-list-length
    #:make-vm-listen-inst #:make-vm-listp #:make-vm-load-file #:make-vm-load-time-value #:make-vm-load-time-value-cell #:make-vm-log-inst #:make-vm-logand #:make-vm-logbitp #:make-vm-logcount #:make-vm-logeqv
    #:make-vm-logior #:make-vm-lognot #:make-vm-logtest #:make-vm-logxor #:make-vm-long-site-name #:make-vm-lower-case-p #:make-vm-lt #:make-vm-machine-instance #:make-vm-machine-type #:make-vm-machine-version
    #:make-vm-macroexpand-1-inst #:make-vm-macroexpand-inst #:make-vm-make-array #:make-vm-make-closure #:make-vm-make-hash-table #:make-vm-make-list #:make-vm-make-obj #:make-vm-make-pathname #:make-vm-make-random-state #:make-vm-make-string
    #:make-vm-make-string-output-stream-inst #:make-vm-make-string-stream #:make-vm-make-symbol #:make-vm-makunbound #:make-vm-max #:make-vm-member #:make-vm-min #:make-vm-mod #:make-vm-move #:make-vm-mul
    #:make-vm-mul-checked #:make-vm-mv-bind #:make-vm-mv-bind-regs #:make-vm-name-char #:make-vm-namestring #:make-vm-nbutlast #:make-vm-nconc #:make-vm-neg #:make-vm-next-method-p #:make-vm-ninth
    #:make-vm-not #:make-vm-nreconc #:make-vm-nreverse #:make-vm-nstring-capitalize #:make-vm-nstring-downcase #:make-vm-nstring-upcase #:make-vm-nth #:make-vm-nth-value #:make-vm-nthcdr #:make-vm-null
    #:make-vm-null-p #:make-vm-num-eq #:make-vm-number-p #:make-vm-numerator #:make-vm-oddp #:make-vm-open-file #:make-vm-open-stream-p #:make-vm-or #:make-vm-osr-entry #:make-vm-output-stream-p
    #:make-vm-package-name-inst
    #:make-vm-parent-environment #:make-vm-parse-float #:make-vm-parse-integer #:make-vm-pathname #:make-vm-pathname-name #:make-vm-pathname-type #:make-vm-pathnamep #:make-vm-peek-char #:make-vm-phase #:make-vm-pop
    #:make-vm-prefetch #:make-vm-prin1 #:make-vm-prin1-sexp #:make-vm-princ #:make-vm-princ-sexp #:make-vm-princ-to-string-inst #:make-vm-print #:make-vm-print-inst #:make-vm-print-sexp #:make-vm-program
    #:make-vm-progv-enter #:make-vm-progv-exit #:make-vm-push #:make-vm-quire #:make-vm-random #:make-vm-rational #:make-vm-rationalize #:make-vm-rationalp #:make-vm-read-byte #:make-vm-read-char
    #:make-vm-read-from-string-inst #:make-vm-read-from-string-rp #:make-vm-read-line #:make-vm-read-sexp #:make-vm-read-sexp-inst #:make-vm-reader-advance-rp #:make-vm-reader-peek-rp #:make-vm-reader-read #:make-vm-realp #:make-vm-realpart
    #:make-vm-recompile #:make-vm-register-function #:make-vm-register-method #:make-vm-rem #:make-vm-remhash #:make-vm-remove-handler #:make-vm-remove-package-local-nickname #:make-vm-remprop #:make-vm-rest #:make-vm-ret
    #:make-vm-reverse #:make-vm-rotate #:make-vm-round-inst #:make-vm-row-major-aref #:make-vm-rplaca #:make-vm-rplacd #:make-vm-sbit #:make-vm-scale-float #:make-vm-search-string #:make-vm-second
    #:make-vm-select #:make-vm-set-fill-pointer #:make-vm-set-global #:make-vm-set-symbol-plist #:make-vm-set-symbol-value #:make-vm-sethash #:make-vm-seventh #:make-vm-shadow-inst #:make-vm-short-site-name #:make-vm-signal
    #:make-vm-signal-error #:make-vm-simd-vector-op #:make-vm-simple-error #:make-vm-simple-vector-p #:make-vm-simple-warning #:make-vm-sin-inst #:make-vm-sinh-inst #:make-vm-sixth #:make-vm-sleep-inst #:make-vm-slot-boundp
    #:make-vm-slot-exists-p #:make-vm-slot-makunbound #:make-vm-slot-read #:make-vm-slot-write #:make-vm-software-type #:make-vm-software-version #:make-vm-source-location #:make-vm-spread-values #:make-vm-sqrt #:make-vm-standard-char-p
    #:make-vm-state #:make-vm-stream-element-type-inst #:make-vm-stream-write-string-inst #:make-vm-streamp #:make-vm-string #:make-vm-string-capitalize #:make-vm-string-coerce #:make-vm-string-downcase #:make-vm-string-equal #:make-vm-string-greaterp
    #:make-vm-string-left-trim #:make-vm-string-length #:make-vm-string-lessp #:make-vm-string-not-equal #:make-vm-string-not-greaterp #:make-vm-string-not-lessp #:make-vm-string-right-trim #:make-vm-string-set #:make-vm-string-trim #:make-vm-string-upcase
    #:make-vm-string< #:make-vm-string<= #:make-vm-string= #:make-vm-string> #:make-vm-string>= #:make-vm-stringp #:make-vm-sub #:make-vm-sub-checked #:make-vm-subseq #:make-vm-subst
    #:make-vm-svref #:make-vm-svset #:make-vm-sxhash #:make-vm-symbol-get #:make-vm-symbol-name #:make-vm-symbol-p #:make-vm-symbol-plist #:make-vm-symbol-set #:make-vm-sync-handler-regs #:make-vm-tail-call
    #:make-vm-tan-inst #:make-vm-tanh-inst #:make-vm-tenth #:make-vm-terpri-inst #:make-vm-third #:make-vm-throw #:make-vm-trampoline #:make-vm-truncate #:make-vm-type-check #:make-vm-type-error
    #:make-vm-type-error-condition #:make-vm-type-of #:make-vm-type-rep #:make-vm-typep #:make-vm-unbound-variable #:make-vm-undefined-function #:make-vm-unintern-inst #:make-vm-unread-char #:make-vm-unuse-package-inst #:make-vm-upper-case-p
    #:make-vm-use-package-inst #:make-vm-values #:make-vm-values-regs #:make-vm-values-to-list #:make-vm-values-typep #:make-vm-vector #:make-vm-vector-pop #:make-vm-vector-push #:make-vm-vector-push-extend #:make-vm-vectorp
    #:make-vm-warn #:make-vm-write-byte #:make-vm-write-char #:make-vm-write-line #:make-vm-write-string #:make-vm-write-to-string-inst #:make-vm-write-to-string-rp #:make-vm2-state #:mark-word-gc-color #:mmap-advice
    #:mmap-array #:mmap-close #:mmap-file #:mmap-sync #:move #:mphf-hash #:mphf-lookup #:mphf-table #:muffle-warning #:mul-imm2
    #:mul2 #:neg #:num-eq-imm2 #:num-eq2 #:num-ge-imm2 #:num-ge2 #:num-gt-imm2 #:num-gt2 #:num-le-imm2 #:num-le2
    #:num-lt-imm2 #:num-lt2 #:numeric-contagion-rank #:numeric-contagion-result-type #:numeric-contagion-type-of #:objdump #:object-shape #:octets-to-string #:open #:pack
    #:package-error #:package-error-package #:persistent! #:persistent-map #:persistent-map-count #:persistent-map-p #:persistent-map-test #:pget #:pmap-assoc #:pmap-get
    #:pprint-dispatch-table #:pprint-dispatch-table-p #:pprint-indent #:pprint-logical-block #:pprint-newline #:pprint-tab #:preserve-analysis #:profile-values #:program-error #:pvec
    #:pvec->list #:pvec-assoc #:pvec-conj #:pvec-count #:pvec-get #:random #:random-state #:random-state-p #:read-balanced-form #:read-pmu-counters
    #:reader-error #:readtable-case #:recv-values #:refine #:regex-all-matches #:regex-replace #:regex-replace-all #:regex-scan #:regex-split #:register-gc-root
    #:repl-complete #:repl-undo #:report-stack-overflow #:resolve-hash-test #:restart-bind #:restart-case #:restart-interactive #:restart-name #:retry #:return-nil
    #:room #:rope #:rope-concat #:rope-length #:rope-p #:rope-split #:rope-to-string #:rotate-right #:rt-hash-table-weakness #:rt-make-hash-table
    #:rt-string-dedup #:rt-string-intern #:run-bytecode #:run-compiled #:run-compiled-with-io #:run-doctests #:run-program-slice #:run-string-with-io #:runtime-tuning-report #:satiate-generic-function
    #:satiating-gfs-p #:save-pgo-data #:sendfile #:serious-condition #:set-dispatch-macro-character #:set-global #:set-macro-character #:set-pprint-dispatch #:setenv #:sexp->instruction
    #:sha256-rounds #:shape-slot-offset #:shm-open #:show-types #:sign-binary #:signum #:simple-condition-format-arguments #:simple-condition-format-control #:simple-error #:simple-warning
    #:sleep #:slot-definition-allocation #:slot-definition-initargs #:slot-definition-initform #:slot-definition-initfunction #:slot-definition-location #:slot-definition-name #:slot-definition-type #:socket-accept #:socket-bind
    #:socket-close #:socket-connect #:socket-listen #:socket-local-address #:socket-receive #:socket-send #:software-type #:software-version #:source-location #:source-location-column
    #:source-location-file #:source-location-form #:source-location-line #:source-location-p #:source-location-pathname #:source-span #:specialize-loop-count #:specialize-simd #:splice #:split-hot-cold
    #:sso-string-char #:sso-string-length #:sso-string-p #:start-cpu-profile #:static-assert #:step #:storage-condition #:store-value #:stream-advance-to-column #:stream-clear-input
    #:stream-clear-output #:stream-error #:stream-error-stream #:stream-external-format #:stream-finish-output #:stream-force-output #:stream-fresh-line #:stream-line-column #:stream-listen #:stream-peek-char
    #:stream-read-byte #:stream-read-char #:stream-read-char-no-hang #:stream-read-line #:stream-start-line-p #:stream-terpri #:stream-unread-char #:stream-write-byte #:stream-write-char #:stream-write-string
    #:string-builder #:string-builder-append! #:string-builder-finish #:string-builder-p #:string-contains-simd #:string-interned-p #:string-to-octets #:string-to-utf8-bytes #:style-warning #:sub-imm2
    #:sub2 #:swar-and #:the-value #:time #:tls-server-context #:tls-socket-stream #:tls-unsupported #:tls-wrap-socket #:trace #:trampoline
    #:transfer-to #:transfer-value #:transient #:transient! #:transient-hash #:transient-hash-p #:transient-p #:type-error #:type-error-datum #:type-error-expected-type
    #:unbound-slot #:unbound-slot-instance #:unbound-variable #:undefined-function #:unicode-alpha-char-p #:unicode-alphanumericp #:unicode-char-code #:unicode-char-downcase #:unicode-char-upcase #:unicode-code-char
    #:unicode-digit-char-p #:unicode-general-category #:unicode-lower-case-p #:unicode-normalize #:unicode-normalize-nfc #:unicode-normalize-nfd #:unicode-normalize-nfkc #:unicode-normalize-nfkd #:unicode-normalize-string #:unicode-syntax-class
    #:unicode-upper-case-p #:unpack #:untrace #:use-value #:utf8-bytes-to-string #:valid-hash-table-rehash-policy-p #:value-type-def #:vector-ref #:vector-set #:version-function
    #:version-loops-with-alias-checks #:vm-a #:vm-abort-restart #:vm-abort-to-prompt #:vm-abs #:vm-acas-addr #:vm-acas-expected #:vm-acas-newval #:vm-acos-inst #:vm-add
    #:vm-add-checked #:vm-add-p #:vm-add-package-local-nickname #:vm-add-restart #:vm-adjust-sequence #:vm-adjustable #:vm-adjustable-reg #:vm-aincf-addr #:vm-aincf-delta #:vm-allocate-file-handle
    #:vm-aload-addr #:vm-alpha-char-p #:vm-alphanumericp #:vm-and #:vm-ansi-color #:vm-ansi-reset #:vm-append #:vm-apply #:vm-apropos #:vm-apropos-list
    #:vm-aref #:vm-aref-multi #:vm-arg-slot-name #:vm-args #:vm-arith-dispatch #:vm-arith-dispatch-op #:vm-arithmetic-error #:vm-array-adjustable-p #:vm-array-has-fill-pointer-p #:vm-array-length
    #:vm-array-reg #:vm-array-row-major-index #:vm-aset #:vm-ash #:vm-asin-inst #:vm-astore-addr #:vm-astore-val #:vm-atan-inst #:vm-atomic-cas #:vm-atomic-incf
    #:vm-atomic-load #:vm-atomic-store #:vm-atomic-swap #:vm-attach-source-location #:vm-b #:vm-bignump #:vm-bind-arg-slots #:vm-bind-closure-args #:vm-bind-restart #:vm-binop
    #:vm-bit-vector-p #:vm-bit-vector-ref #:vm-both-case-p #:vm-boundp #:vm-break #:vm-bswap #:vm-build-list #:vm-butlast #:vm-bytecode-function #:vm-bytecode-function-constant-pool
    #:vm-bytecode-function-env-size #:vm-bytecode-function-name #:vm-bytecode-function-opcode-bytes #:vm-bytecode-function-p #:vm-c #:vm-call #:vm-call-next-method #:vm-call-next-method-args-reg #:vm-call-p #:vm-call-stack
    #:vm-call-with-dynamic-wind-context #:vm-call-with-prompt #:vm-call/cc #:vm-capture-continuation #:vm-capture-deopt-frame #:vm-captured-vars #:vm-car #:vm-car-reg #:vm-cast-with-type-rep #:vm-catch-handler-label
    #:vm-catch-result-reg #:vm-catch-tag-reg #:vm-cdr #:vm-cdr-reg #:vm-ceiling-inst #:vm-cell-error #:vm-cerror #:vm-char #:vm-char-bag #:vm-char-code
    #:vm-char-reg #:vm-char1 #:vm-char2 #:vm-char< #:vm-char= #:vm-check-type #:vm-cl-div #:vm-clamp #:vm-class-def #:vm-class-name-fn
    #:vm-class-name-sym #:vm-class-of-fn #:vm-class-reg #:vm-class-registry #:vm-class-slots #:vm-classify-arg #:vm-clear-condition-context #:vm-clear-hash-cons-table #:vm-close-file #:vm-closure
    #:vm-closure-captured-regs #:vm-closure-captured-vals #:vm-closure-compilation-tier #:vm-closure-entry-label #:vm-closure-env #:vm-closure-index #:vm-closure-inline-policy #:vm-closure-inst-dispatch-tag #:vm-closure-invocation-count #:vm-closure-key-params
    #:vm-closure-label-table #:vm-closure-object #:vm-closure-optional-params #:vm-closure-p #:vm-closure-params #:vm-closure-program-flat #:vm-closure-ref-idx #:vm-closure-reg #:vm-closure-rest-param #:vm-closure-rest-stack-alloc-p
    #:vm-clrhash #:vm-code-char #:vm-coerce-to-list #:vm-coerce-to-string #:vm-coerce-to-vector #:vm-compile-file-to-fasl #:vm-compile-osr-entry-if-hot #:vm-complement #:vm-complexp #:vm-compute-active-restarts
    #:vm-compute-effective-slot-definition #:vm-compute-restarts #:vm-concatenate #:vm-condition #:vm-condition-error-code #:vm-condition-fix-it #:vm-condition-source-location #:vm-condition-state #:vm-cons #:vm-cons-cell
    #:vm-cons-cell-car #:vm-cons-cell-cdr #:vm-cons-p #:vm-cons-reg #:vm-const #:vm-const-p #:vm-const-value #:vm-constantly #:vm-continuation #:vm-continuation-p
    #:vm-control-error #:vm-copy-vector #:vm-cos-inst #:vm-current-deopt-frame #:vm-current-state #:vm-datum-reg #:vm-dec #:vm-declare-forward-reference #:vm-decode-bytes #:vm-decode-universal-time
    #:vm-define-condition #:vm-denominator #:vm-deopt #:vm-deopt-frame #:vm-deopt-frame-call-stack #:vm-deopt-frame-closure-env #:vm-deopt-frame-info #:vm-deopt-frame-inline-stack #:vm-deopt-frame-pc #:vm-deopt-frame-physical-registers
    #:vm-deopt-frame-reason #:vm-deopt-frame-registers #:vm-deopt-frame-values-list #:vm-deopt-history #:vm-deopt-id #:vm-deopt-info #:vm-deopt-info-description #:vm-deopt-info-env #:vm-deopt-info-inline-stack #:vm-deopt-info-label
    #:vm-deopt-info-live-regs #:vm-deopt-info-pc #:vm-deopt-info-vreg->preg #:vm-deopt-label #:vm-deopt-reason #:vm-describe #:vm-did-you-mean #:vm-digit-char-p #:vm-dispatch-lock-free #:vm-displaced-to-reg
    #:vm-div #:vm-dividend #:vm-division-by-zero #:vm-dribble #:vm-dst #:vm-dst-array-reg #:vm-dst-regs #:vm-dump-type-profile #:vm-dynamic #:vm-dynamic-p
    #:vm-dynamic-type-rep #:vm-dynamic-value #:vm-dynamic-wind #:vm-dynamic-wind-frame #:vm-dynamic-wind-frame-after-thunk #:vm-dynamic-wind-frame-before-thunk #:vm-dynamic-wind-frame-p #:vm-dynamic-wind-reenter #:vm-ed #:vm-eighth
    #:vm-element-type #:vm-element-type-reg #:vm-encode-string #:vm-encode-universal-time #:vm-end #:vm-end-of-file #:vm-endp #:vm-env-regs #:vm-eof-p #:vm-eq
    #:vm-error #:vm-error-instruction #:vm-error-reg #:vm-error-type #:vm-error-type-matches-p #:vm-establish-catch #:vm-establish-handler #:vm-eval #:vm-eval-deadline-exceeded #:vm-exception-entry-condition-type
    #:vm-exception-entry-end-pc #:vm-exception-entry-handler-pc #:vm-exception-entry-result-reg #:vm-exception-entry-start-pc #:vm-exp-inst #:vm-expected-type #:vm-falsep #:vm-fatal-error #:vm-fboundp #:vm-fdefinition
    #:vm-fifth #:vm-file-counter #:vm-file-direction #:vm-file-handle #:vm-file-length #:vm-file-position #:vm-fill #:vm-fill-pointer #:vm-fill-pointer-reg #:vm-find-class
    #:vm-find-handler #:vm-find-restart #:vm-find-standard-restart #:vm-first #:vm-fixnum-add-with-overflow-detection #:vm-fixnum-mul-with-overflow-detection #:vm-fixnum-overflow-detected-p #:vm-fixnum-sub-with-overflow-detection #:vm-float-add #:vm-float-div
    #:vm-float-mul #:vm-float-precision #:vm-float-sign #:vm-float-sub #:vm-float-to-string #:vm-floating-point-overflow #:vm-floating-point-underflow #:vm-floatp #:vm-floor-inst #:vm-fma
    #:vm-format-inst #:vm-forward-reference-cell #:vm-forward-reference-cell-name #:vm-forward-reference-cell-p #:vm-forward-reference-cell-ref #:vm-forward-reference-name #:vm-found-dst #:vm-fourth #:vm-fresh-line-inst #:vm-func-name
    #:vm-func-ref #:vm-func-ref-dispatch-tag #:vm-func-reg #:vm-function-p #:vm-function-registry #:vm-gcd #:vm-ge #:vm-generic-add #:vm-generic-call #:vm-generic-call-metadata
    #:vm-generic-div #:vm-generic-eq #:vm-generic-function-p #:vm-generic-gt #:vm-generic-lt #:vm-generic-mul #:vm-generic-sub #:vm-get-global #:vm-get-global-dst #:vm-get-global-name
    #:vm-get-global-p #:vm-get-handler-stack #:vm-get-internal-real-time #:vm-get-internal-run-time #:vm-get-output-stream-string-inst #:vm-get-profile-bb-counts #:vm-get-profile-branch-counts #:vm-get-profile-call-counts #:vm-get-profile-call-times #:vm-get-profile-inst-counts
    #:vm-get-profile-samples #:vm-get-profile-type-feedback #:vm-get-restarts #:vm-get-source-location #:vm-get-stream #:vm-get-string-from-stream #:vm-get-universal-time #:vm-gethash #:vm-gethash-default #:vm-gethash-eq
    #:vm-gethash-eql #:vm-gethash-equal #:vm-gf-reg #:vm-global-name #:vm-global-vars #:vm-graphic-char-p #:vm-gt #:vm-halt #:vm-handle-runtime-flag #:vm-handler-case
    #:vm-handler-entry-saved-regs #:vm-handler-label #:vm-handler-result-reg #:vm-handler-stack #:vm-handler-type #:vm-hash-cons #:vm-hash-cons-p #:vm-hash-default #:vm-hash-fn #:vm-hash-key
    #:vm-hash-table-count #:vm-hash-table-internal #:vm-hash-table-keys #:vm-hash-table-object #:vm-hash-table-p #:vm-hash-table-reg #:vm-hash-table-rehash-size #:vm-hash-table-rehash-threshold #:vm-hash-table-size #:vm-hash-table-test
    #:vm-hash-table-values #:vm-hash-table-weakness #:vm-hash-test #:vm-hash-value #:vm-hash-weakness #:vm-heap-address #:vm-heap-address-p #:vm-heap-address-value #:vm-heap-alloc #:vm-heap-counter
    #:vm-heap-get #:vm-heap-object #:vm-heap-set #:vm-ic-type-counters #:vm-identity #:vm-inc #:vm-index #:vm-index-reg #:vm-initarg-regs #:vm-initial-element
    #:vm-initial-string #:vm-input-stream-p #:vm-inspect #:vm-instance-function-value #:vm-instance-global-value #:vm-instance-lock #:vm-instance-parent-env #:vm-instruction #:vm-instruction-profile-alist #:vm-integer-add
    #:vm-integer-length #:vm-integer-mul #:vm-integer-mul-high-s #:vm-integer-mul-high-u #:vm-integer-p #:vm-integer-sub #:vm-interactive-stream-p #:vm-intern-pkg #:vm-invoke-continuation #:vm-invoke-debugger
    #:vm-invoke-restart #:vm-invoke-restart-interactively #:vm-io-state #:vm-isatty #:vm-item-reg #:vm-jump #:vm-jump-p #:vm-jump-zero #:vm-label #:vm-label-name
    #:vm-label-p #:vm-label-table-lookup #:vm-label-table-store #:vm-last #:vm-lbl-name #:vm-lcm #:vm-le #:vm-len-reg #:vm-length #:vm-lerp
    #:vm-levenshtein-distance #:vm-lhs #:vm-list-length #:vm-list-reg #:vm-list-to-lisp-list #:vm-listen-inst #:vm-listp #:vm-load-fasl #:vm-load-fence #:vm-load-time-value
    #:vm-load-time-value-cell #:vm-load-time-value-cell-form #:vm-load-time-value-cell-id #:vm-load-time-value-cell-read-only-p #:vm-load-time-value-cell-resolved-p #:vm-load-time-value-cell-value #:vm-load-time-values #:vm-local-nickname-nick #:vm-local-nickname-pkg #:vm-local-nickname-target
    #:vm-log-inst #:vm-logand #:vm-logbitp #:vm-logcount #:vm-logeqv #:vm-logior #:vm-lognot #:vm-logtest #:vm-logxor #:vm-lower-case-p
    #:vm-lt #:vm-macroexpand-1-inst #:vm-macroexpand-inst #:vm-make-array #:vm-make-array-adjustable #:vm-make-array-fill-pointer #:vm-make-closure #:vm-make-closure-p #:vm-make-closure-params #:vm-make-hash-table
    #:vm-make-hash-table-test #:vm-make-list #:vm-make-load-form #:vm-make-load-form-saving-slots #:vm-make-obj #:vm-make-random-state #:vm-make-sequence-like #:vm-make-specialized-array #:vm-make-string #:vm-make-string-char
    #:vm-make-string-output-stream-inst #:vm-make-string-stream #:vm-make-string-stream-direction #:vm-makunbound #:vm-materialize-osr-frame #:vm-max #:vm-maybe-dump-type-profile #:vm-member #:vm-memory-barrier #:vm-method-call-stack
    #:vm-method-qualifier #:vm-method-reg #:vm-method-specializer #:vm-min #:vm-mod #:vm-move #:vm-move-dst #:vm-move-p #:vm-move-src #:vm-muffle-warning-restart
    #:vm-mul #:vm-mul-checked #:vm-mv-bind #:vm-mv-bind-regs #:vm-mv-bind-regs-count #:vm-mv-buffer #:vm-mv-count #:vm-name #:vm-nbutlast #:vm-neg
    #:vm-neg-p #:vm-next-method-p #:vm-ninth #:vm-not #:vm-nreconc #:vm-nstring-capitalize #:vm-nstring-downcase #:vm-nstring-upcase #:vm-nth #:vm-nth-value
    #:vm-nthcdr #:vm-null #:vm-null-p #:vm-num-eq #:vm-number-p #:vm-numerator #:vm-obj-reg #:vm-open #:vm-open-file #:vm-open-file-direction
    #:vm-open-files #:vm-open-stream-p #:vm-or #:vm-osr-entry #:vm-osr-id #:vm-osr-label #:vm-osr-register-map #:vm-osr-stub-enter #:vm-output-stream #:vm-output-stream-p
    #:vm-package-error #:vm-parent-environment #:vm-parent-environment-functions #:vm-parent-environment-globals #:vm-parent-environment-symbols #:vm-parts #:vm-pattern #:vm-peek-char #:vm-pgo-specializer #:vm-pop
    #:vm-pop-handler #:vm-pop-handler-from-stack #:vm-posit #:vm-posit-add #:vm-posit-bits #:vm-posit-decode #:vm-posit-div #:vm-posit-encode #:vm-posit-es #:vm-posit-from-bits
    #:vm-posit-mul #:vm-posit-nar-p #:vm-posit-nbits #:vm-posit-p #:vm-posit-sub #:vm-posit-zero-p #:vm-position-reg #:vm-prefetch #:vm-prefetch-base-reg #:vm-prefetch-index-reg
    #:vm-prefetch-kind #:vm-prefetch-locality #:vm-prefetch-offset #:vm-prefetch-scale #:vm-prin1 #:vm-prin1-sexp #:vm-princ #:vm-princ-sexp #:vm-princ-to-string-inst #:vm-print
    #:vm-print-backtrace #:vm-print-inst #:vm-print-sexp #:vm-print-unreadable-object #:vm-profile-call-stack #:vm-profile-call-start-times #:vm-profile-call-times #:vm-profile-enabled-p #:vm-profile-inst-counts #:vm-profile-inst-hit
    #:vm-profile-samples #:vm-program #:vm-program-calling-convention #:vm-program-compilation-tier #:vm-program-deopt-info #:vm-program-error #:vm-program-function-conventions #:vm-program-instructions #:vm-program-leaf-p #:vm-program-osr-entry-points
    #:vm-program-p #:vm-program-result-register #:vm-program-tier1-entry-points #:vm-prompt-reg #:vm-push #:vm-push-call-frame #:vm-push-handler #:vm-push-handler-to-stack #:vm-push-handler-type #:vm-quire
    #:vm-quire-add-product! #:vm-quire-es #:vm-quire-nbits #:vm-quire-p #:vm-quire-to-posit #:vm-quire-value #:vm-random #:vm-random-state #:vm-random-state-p #:vm-rational
    #:vm-rationalize #:vm-rationalp #:vm-read-byte #:vm-read-char #:vm-read-from-fasl #:vm-read-from-string-inst #:vm-read-key #:vm-read-line #:vm-read-sexp #:vm-read-sexp-inst
    #:vm-reader-error #:vm-reader-read #:vm-readline #:vm-realp #:vm-recompile #:vm-reconstruct-interpreter-frame #:vm-record-gc-pause #:vm-record-value-profile #:vm-reg #:vm-reg-get
    #:vm-reg-set #:vm-register-function #:vm-register-host-bridge #:vm-register-method #:vm-register-program-exception-table #:vm-register-tier1-osr-entry #:vm-rem #:vm-remhash #:vm-remove-handler #:vm-remove-package-local-nickname
    #:vm-reset-instruction-profile #:vm-resolve-forward-references #:vm-resolve-function #:vm-resolve-gf-method #:vm-rest #:vm-restart-label #:vm-restart-name #:vm-restart-name-inst #:vm-restore-registers #:vm-restore-registers-subset
    #:vm-ret #:vm-ret-p #:vm-retry-restart #:vm-reverse #:vm-rhs #:vm-room #:vm-rotate #:vm-round-inst #:vm-rplaca #:vm-rplacd
    #:vm-runtime-type-of #:vm-ryu-double-to-string #:vm-ryu-float-to-string #:vm-ryu-single-to-string #:vm-save-registers #:vm-save-registers-subset #:vm-sealed-p #:vm-search-string #:vm-second #:vm-select
    #:vm-select-cond-reg #:vm-select-else-reg #:vm-select-then-reg #:vm-sequence-elt #:vm-sequence-length #:vm-serious-condition #:vm-set-global #:vm-set-global-name #:vm-set-global-p #:vm-set-global-src
    #:vm-set-stream-external-format #:vm-sethash #:vm-seventh #:vm-show-restart-menu #:vm-signal #:vm-signal-condition #:vm-signal-error #:vm-signal-with-location #:vm-signum #:vm-simd-vector-op
    #:vm-simd-vector-op-dst-array #:vm-simd-vector-op-element-type #:vm-simd-vector-op-index-reg #:vm-simd-vector-op-lanes #:vm-simd-vector-op-lhs-array #:vm-simd-vector-op-op #:vm-simd-vector-op-p #:vm-simd-vector-op-rhs-array #:vm-simple-condition #:vm-simple-error
    #:vm-simple-vector-p #:vm-simple-warning #:vm-sin-inst #:vm-sixth #:vm-size-reg #:vm-slot-boundp #:vm-slot-exists-p #:vm-slot-initargs #:vm-slot-makunbound #:vm-slot-name-sym
    #:vm-slot-names #:vm-slot-read #:vm-slot-read-dst #:vm-slot-read-obj-reg #:vm-slot-read-slot-name #:vm-slot-types #:vm-slot-write #:vm-slot-write-obj-reg #:vm-slot-write-slot-name #:vm-slot-write-value-reg
    #:vm-socket #:vm-socket-p #:vm-source-context-lines #:vm-source-location #:vm-source-location-column #:vm-source-location-compilation-unit #:vm-source-location-form #:vm-source-location-line #:vm-source-location-macro-context #:vm-source-location-p
    #:vm-source-location-pathname #:vm-source-location-string #:vm-specialized-array #:vm-specialized-array-element-pointer-free-p #:vm-specialized-array-element-type #:vm-specialized-array-gc-skip-p #:vm-specialized-array-header #:vm-specialized-array-length #:vm-specialized-array-p #:vm-specialized-array-ref
    #:vm-specialized-array-storage #:vm-spread-values #:vm-sqrt #:vm-src #:vm-src-array-reg #:vm-src-regs #:vm-standard-char-p #:vm-standard-input #:vm-standard-output #:vm-start
    #:vm-state #:vm-state-heap #:vm-state-registers #:vm-step #:vm-step-prompt #:vm-storage-condition #:vm-store-fence #:vm-store-value-restart #:vm-str-reg #:vm-str1
    #:vm-str2 #:vm-stream-direction #:vm-stream-error #:vm-stream-external-format #:vm-stream-open-p #:vm-stream-write-string-inst #:vm-string-capitalize #:vm-string-coerce #:vm-string-downcase #:vm-string-equal
    #:vm-string-greaterp #:vm-string-left-trim #:vm-string-length #:vm-string-lessp #:vm-string-not-equal #:vm-string-reg #:vm-string-right-trim #:vm-string-set #:vm-string-streams #:vm-string-trim
    #:vm-string-upcase #:vm-string< #:vm-string<= #:vm-string= #:vm-string> #:vm-string>= #:vm-stringp #:vm-style-warning #:vm-sub #:vm-sub-checked
    #:vm-sub-p #:vm-subseq #:vm-superclasses #:vm-svref #:vm-sxhash #:vm-symbol-p #:vm-sync-handler-regs #:vm-tail-call #:vm-tail-p #:vm-tan-inst
    #:vm-tenth #:vm-terminal-size #:vm-terpri-inst #:vm-third #:vm-throw #:vm-throw-tag-reg #:vm-throw-value-reg #:vm-tier-hot-p #:vm-tier-note-call #:vm-tier-reset-counters
    #:vm-tier1-code #:vm-trace #:vm-traced-p #:vm-trampoline #:vm-trigger-deopt #:vm-truncate #:vm-try-dispatch-combinations #:vm-try-dispatch-sub #:vm-type-check #:vm-type-check-deopt-id
    #:vm-type-check-deopt-label #:vm-type-error #:vm-type-error-condition #:vm-type-error-values-p #:vm-type-name #:vm-type-of #:vm-type-profile-alist #:vm-type-rep #:vm-type-rep-equal #:vm-type-rep-of
    #:vm-type-rep-p #:vm-type-rep-specifier #:vm-typecase-dispatch #:vm-typep #:vm-typep-check #:vm-typep-type-name #:vm-unbound-slot #:vm-unbound-variable #:vm-undefined-function #:vm-unread-char
    #:vm-untrace #:vm-unwrap-dynamic #:vm-upper-case-p #:vm-use-value-restart #:vm-val-reg #:vm-value #:vm-value-profile-snapshot #:vm-value-reg #:vm-values #:vm-values-list
    #:vm-values-regs #:vm-values-to-list #:vm-values-typep #:vm-var-name #:vm-vector #:vm-vectorp #:vm-vr-count #:vm-vr0 #:vm-vr1 #:vm-vr2
    #:vm-warn #:vm-warning #:vm-with-raw-terminal #:vm-with-simple-restart #:vm-with-trace #:vm-wrap #:vm-wrap-dynamic #:vm-write-char #:vm-write-instruction-profile #:vm-write-string
    #:vm-write-to-fasl #:vm-write-to-string-inst #:vm2-state-global-vars #:vm2-state-output-stream #:vm2-state-p #:vm2-state-registers #:vm2-state-values-buffer #:vpshufb #:warning #:when-feature
    #:with-float-traps-masked #:with-mmap #:with-pinned-object #:with-platform #:with-precision #:with-satiating-gfs #:with-simple-restart #:with-socket #:with-standard-io-syntax #:with-string-builder
    #:with-timeout #:with-vm-instance-lock #:with-vm-state-dispatch #:wrap #:%vm-vector-instance-p #:%vm-slot-vector-index #:copy-instance #:class-slots #:%vm-hashlike-storage #:*unbound-slot-marker* #:%vm-fixed-slot-layout-p #:%vm-raw-allocate-instance #:vm-current-closure-stack #:%vm-closure-object-p #:vm-closure-dispatch-tag #:%vm-call-closure-sync #:%vm-runtime-callable)
)


(in-package :cl-cc/vm)
