(defpackage #:cl-cc/vm/data
  (:use
    #:cl
  )
  (:shadow
    #:copy-structure #:elt #:length #:random-state #:random-state-p #:sequence #:subseq
  )
  (:import-from #:cl-cc/vm/protocols #:vm-value #:vm-index #:*instruction-constructors* #:define-simple-instruction #:define-vm-binary-instruction #:define-vm-char-comparison #:define-vm-instruction #:define-vm-unary-instruction #:execute-instruction #:instruction->sexp #:vm-char1 #:vm-char2
    #:vm-dst #:vm-instruction #:vm-lhs #:vm-reg-get #:vm-reg-set #:vm-register-host-bridge #:vm-rhs #:vm-src #:vm-state-heap #:vm-typep-check
    #:vm-values-list #:%vm-typep-call-predicate #:%vm-typep-range-check #:vm-check-index #:vm-closure-object-p #:vm-copy-instance #:vm-decode-symbol #:vm-immediate-symbol-p #:vm-random-integer #:vm-state-symbol-plists
    #:vm-state-system-symbol-plists #:vm-state-symbol-plist-lock #:vm-state-symbol-plist-read-barrier)
  (:export #:*deferred-rc-enabled* #:*hash-table-rehash-policy* #:*interpolation-prefix* #:*perfect-hash-tables-enabled* #:*rt-string-dedup-table* #:*string-intern-table* #:*vm-hash-cons-table* #:*vm-primitive-type-dispatch* #:+hash-table-rehash-policies+ #:+sso-max-length+
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
    #:random-state #:random-state-p #:refine #:rope #:rope-concat #:rope-length #:rope-p #:rope-split #:rope-to-string #:rt-hash-table-weakness
    #:rt-make-hash-table #:rt-string-dedup #:rt-string-intern #:sso-string-char #:sso-string-length #:sso-string-p #:string-builder #:string-builder-append! #:string-builder-finish #:string-builder-p
    #:string-contains-simd #:string-interned-p #:string-to-utf8-bytes #:transient #:transient! #:transient-hash #:transient-hash-p #:transient-p #:unicode-alpha-char-p #:unicode-alphanumericp
    #:unicode-char-code #:unicode-char-downcase #:unicode-char-upcase #:unicode-code-char #:unicode-digit-char-p #:unicode-general-category #:unicode-lower-case-p #:unicode-normalize #:unicode-normalize-nfc #:unicode-normalize-nfd
    #:unicode-normalize-nfkc #:unicode-normalize-nfkd #:unicode-normalize-string #:unicode-syntax-class #:unicode-upper-case-p #:utf8-bytes-to-string #:valid-hash-table-rehash-policy-p #:vm-add-package-local-nickname #:vm-adjust-sequence #:vm-adjustable
    #:vm-adjustable-reg #:vm-alpha-char-p #:vm-alphanumericp #:vm-append #:vm-aref #:vm-aref-multi #:vm-array-adjustable-p #:vm-array-has-fill-pointer-p #:vm-array-length #:vm-array-reg
    #:vm-array-row-major-index #:vm-aset #:vm-bit-vector-p #:vm-bit-vector-ref #:vm-both-case-p #:vm-butlast #:vm-car #:vm-car-reg #:vm-cast-with-type-rep #:vm-cdr
    #:vm-cdr-reg #:vm-char #:vm-char-bag #:vm-char-code #:vm-char< #:vm-char= #:vm-clear-hash-cons-table #:vm-clrhash #:vm-code-char #:vm-coerce-to-list
    #:vm-coerce-to-string #:vm-coerce-to-vector #:vm-complement #:vm-concatenate #:vm-cons #:vm-cons-reg #:vm-constantly #:vm-copy-vector #:vm-digit-char-p #:vm-displaced-to-reg
    #:vm-dst-array-reg #:vm-dynamic #:vm-dynamic-p #:vm-dynamic-type-rep #:vm-dynamic-value #:vm-eighth #:vm-element-type #:vm-element-type-reg #:vm-end #:vm-endp
    #:vm-fifth #:vm-fill #:vm-fill-pointer #:vm-fill-pointer-reg #:vm-first #:vm-found-dst #:vm-fourth #:vm-gethash #:vm-gethash-default #:vm-gethash-eq
    #:vm-gethash-eql #:vm-gethash-equal #:vm-graphic-char-p #:vm-hash-cons #:vm-hash-cons-p #:vm-hash-default #:vm-hash-fn #:vm-hash-key #:vm-hash-table-count #:vm-hash-table-internal
    #:vm-hash-table-keys #:vm-hash-table-object #:vm-hash-table-p #:vm-hash-table-reg #:vm-hash-table-rehash-size #:vm-hash-table-rehash-threshold #:vm-hash-table-size #:vm-hash-table-test #:vm-hash-table-values #:vm-hash-table-weakness
    #:vm-hash-test #:vm-hash-value #:vm-hash-weakness #:vm-identity #:vm-index-reg #:vm-initial-element #:vm-intern-pkg #:vm-item-reg #:vm-last #:vm-len-reg
    #:vm-length #:vm-list-length #:vm-list-reg #:vm-listp #:vm-local-nickname-nick #:vm-local-nickname-pkg #:vm-local-nickname-target #:vm-lower-case-p #:vm-make-array #:vm-make-array-adjustable
    #:vm-make-array-fill-pointer #:vm-make-hash-table #:vm-make-hash-table-test #:vm-make-list #:vm-make-sequence-like #:vm-make-specialized-array #:vm-make-string #:vm-make-string-char #:vm-member #:vm-nbutlast
    #:vm-ninth #:vm-nreconc #:vm-nstring-capitalize #:vm-nstring-downcase #:vm-nstring-upcase #:vm-nth #:vm-nthcdr #:vm-null #:vm-parts #:vm-pattern
    #:vm-pop #:vm-push #:vm-random-state #:vm-random-state-p #:vm-remhash #:vm-remove-package-local-nickname #:vm-rest #:vm-reverse #:vm-rplaca #:vm-rplacd
    #:vm-runtime-type-of #:vm-search-string #:vm-second #:vm-sequence-elt #:vm-sequence-length #:vm-sethash #:vm-seventh #:vm-simple-vector-p #:vm-sixth #:vm-size-reg
    #:vm-specialized-array #:vm-specialized-array-element-pointer-free-p #:vm-specialized-array-element-type #:vm-specialized-array-gc-skip-p #:vm-specialized-array-header #:vm-specialized-array-length #:vm-specialized-array-p #:vm-specialized-array-ref #:vm-specialized-array-storage #:vm-src-array-reg
    #:vm-standard-char-p #:vm-start #:vm-str-reg #:vm-str1 #:vm-str2 #:vm-string-capitalize #:vm-string-coerce #:vm-string-downcase #:vm-string-equal #:vm-string-greaterp
    #:vm-string-left-trim #:vm-string-length #:vm-string-lessp #:vm-string-not-equal #:vm-string-reg #:vm-string-right-trim #:vm-string-set #:vm-string-trim #:vm-string-upcase #:vm-string<
    #:vm-string<= #:vm-string= #:vm-string> #:vm-string>= #:vm-stringp #:vm-subseq #:vm-svref #:vm-sxhash #:vm-tenth #:vm-third
    #:vm-type-rep #:vm-type-rep-equal #:vm-type-rep-of #:vm-type-rep-p #:vm-type-rep-specifier #:vm-unwrap-dynamic #:vm-upper-case-p #:vm-val-reg #:vm-vector #:vm-vectorp
    #:vm-wrap-dynamic #:with-string-builder #:vm-assoc-key #:vm-assoc-alist #:vm-char-index #:vm-char-string #:vm-acons-key #:vm-acons-value #:vm-acons-alist #:vm-remprop-sym #:vm-remprop-indicator #:vm-symbol-get-sym #:vm-symbol-get-indicator #:vm-symbol-get-default #:vm-symbol-set-sym #:vm-symbol-set-indicator #:vm-symbol-set-value #:vm-find-symbol
    #:vm-sym
    #:sequence #:length #:elt #:subseq
    #:make-sequence-like #:sequence-protocol-p #:vm-copy-symbol-property-entry)
  (:intern
    #:%ascii-char-class-logtest #:%ascii-control-p #:%char-code<=255-p #:%copy-hash-table #:%ensure-even-key-values #:%ensure-persistent-map #:%ensure-unfrozen-transient #:%execute-vm-gethash #:%execute-vm-specialized-gethash #:%host-general-category
    #:%lazy-range-finished-p #:%lazy-realize #:%make-char-class-table #:%make-rope-from-root #:%make-ucd-bmp-category-ranges #:%make-vm-specialized-array-header #:%offset-case-map #:%parse-persistent-map-args #:%persistent-map-test-name #:%pvec->mutable-buffer
    #:%rope-coerce #:%rope-flatten-root #:%rope-root-length #:%rt-ensure-string-dedup-table #:%sb-unicode-call #:%unicode-category-index #:%unicode-char #:%unicode-code #:%unicode-code-point-p #:%unicode-codes-to-string
    #:%unicode-combining-class #:%unicode-compose-latin-1 #:%unicode-host-category #:%unicode-host-digit-value #:%unicode-latin-1-decomposition #:%unicode-letter-category-p #:%unicode-nfd-codes #:%unicode-normalize-nfc #:%unicode-normalize-nfd #:%unicode-push-code-char
    #:%unicode-zs-code-p #:%utf8-continuation #:%utf8-push-code #:%vm-array-dimensions-designator #:%vm-array-object #:%vm-array-total-size #:%vm-array-vector-dimensions-p #:%vm-bridge-replace #:%vm-copy-array-backing #:%vm-cow-list-ensure-writable #:%vm-cow-list-materialize #:%vm-cow-string-ensure-writable #:%vm-cow-string-length #:%vm-cow-vector-ensure-writable #:%vm-cow-vector-materialize #:%vm-cow-vector-share #:%vm-decode-sso-string #:%vm-find-package-local-nickname #:%vm-hash-cons-canonicalize
    #:%vm-hash-cons-intern-pair #:%vm-host-package-local-nickname-function #:%vm-host-string #:%vm-list-structure-materialize #:%vm-make-displaced-array #:%vm-make-standard-array #:%vm-managed-cons-alloc #:%vm-managed-cons-car #:%vm-managed-cons-cdr #:%vm-managed-cons-heap
    #:%vm-managed-cons-heap-table #:%vm-managed-cons-materialize #:%vm-managed-cons-pointer-p #:%vm-managed-cons-register-root #:%vm-managed-cons-runtime-available-p #:%vm-managed-cons-set-slot #:%vm-managed-cons-slot #:%vm-managed-list-elt #:%vm-managed-list-length #:%vm-managed-list-tail
    #:%vm-managed-member #:%vm-managed-nil-p #:%vm-managed-tree-materialize #:%vm-maybe-sso-string #:%vm-normalize-specialized-element-type #:%vm-normalize-symbol-property-entry #:%vm-package-operation-result #:%vm-package-result #:%vm-plist-entry-count #:%vm-promote-symbol-property-plist
    #:%vm-registry-result #:%vm-resolve-package-designator #:%vm-runtime-function #:%vm-runtime-symbol #:%vm-runtime-value #:%vm-small-plist-get #:%vm-small-plist-remprop #:%vm-small-plist-set #:%vm-specialized-storage #:%vm-sso-function
    #:%vm-sso-string-p #:%vm-string-char #:%vm-string-header-flag #:%vm-string-length #:%vm-symbol-property-entry->plist #:%vm-symbol-property-entry-get #:%vm-symbol-property-entry-p #:%vm-symbol-property-entry-remprop #:%vm-symbol-property-entry-set #:%vm-symbol-property-get
    #:%vm-symbol-property-hash-table->plist #:%vm-symbol-property-remprop #:%vm-symbol-property-replace-plist #:%vm-symbol-property-set #:%vm-sync-host-symbol-plist #:%vm-touch-symbol-property-barrier #:%vm-type-rep-compatible-p #:%vm-type-rep-value-compatible-p #:*char-class-table* #:*taint-mode*
    #:*ucd-bmp-category-ranges* #:*ucd-category-index* #:*vm-compound-type-handlers* #:*vm-cow-vector-enabled* #:*vm-hash-enable-lock-elision-p* #:*vm-hash-htm-abort-threshold* #:*vm-hash-htm-supported-p* #:*vm-hash-low-contention-p* #:*vm-managed-cons-allocation-enabled* #:*vm-managed-cons-sequence-state*
    #:*vm-primitive-type-predicates* #:*vm-standard-character-names* #:*vm-taint-table* #:*vm-type-of-dispatch* #:+char-class-alpha+ #:+char-class-alphanumeric+ #:+char-class-digit+ #:+char-class-graphic+ #:+char-class-lower+ #:+char-class-standard+
    #:+char-class-upper+ #:+char-class-whitespace+ #:+mt19937-n+ #:+rope-inline-threshold+ #:+ucd-categories+ #:+unicode-bmp-limit+ #:+unicode-max-code-point+ #:+unicode-replacement-character+ #:+vm-bit-vector-tag+ #:+vm-hash-table-weakness-modes+
    #:+vm-specialized-array-tag+ #:+vm-symbol-plist-linear-limit+ #:define-builtin-sequence-methods #:define-managed-list-nth-accessor #:define-vm-hash-property-executors #:define-vm-sso-string-comparison-executor #:define-vm-sso-string-destructive-unary-executor #:define-vm-sso-string-unary-executor #:define-vm-string-comparison #:define-vm-string-trim-executor
    #:define-vm-string-trim-instruction #:rope-length* #:rope-node #:ROPE-NODE-LEFT #:ROPE-NODE-P #:ROPE-NODE-RIGHT #:ROPE-ROOT #:string-builder-append-string! #:string-builder-ensure-capacity! #:string-builder-length #:string-freeze #:string-unfreeze
    #:taint-mark #:tainted-p #:ucd-init-category-index #:unicode-init #:untaint #:vm-acons #:vm-adjust-array #:vm-adjustable-array-p #:vm-adjustable-array-p-value #:vm-alpha-char-p-value
    #:vm-alphanumericp-value #:vm-array-dimension #:vm-array-dimension-value #:vm-array-dimensions #:vm-array-dimensions-value #:vm-array-displacement #:vm-array-element-type #:vm-array-element-type-value #:vm-array-has-fill-pointer-p-value #:vm-array-in-bounds-p
    #:vm-array-in-bounds-p-value #:vm-array-rank #:vm-array-rank-value #:vm-array-total-size #:vm-array-total-size-value #:vm-assoc #:vm-atom #:vm-bit-access #:vm-bit-and #:vm-bit-not
    #:vm-bit-or #:vm-bit-set #:vm-bit-xor #:vm-both-case-p-value #:vm-char-downcase #:vm-char-equal #:vm-char-greaterp #:vm-char-lessp #:vm-char-name #:vm-char-name-value
    #:vm-char-not-equal #:vm-char-not-greaterp #:vm-char-not-lessp #:vm-char-upcase #:vm-char/= #:vm-char<= #:vm-char> #:vm-char>= #:vm-characterp #:vm-check-tainted-usage
    #:vm-copy-list #:vm-copy-tree #:vm-cow-list #:vm-cow-storage #:vm-cow-string #:VM-COW-STRING-BACKING #:VM-COW-STRING-START #:vm-cow-vector #:vm-cow-vector-backing #:vm-digit-char #:vm-digit-char-p-value #:vm-ephemeron
    #:vm-equal #:vm-export #:vm-export-inst #:vm-fill-pointer-inst #:vm-find-package #:vm-gensym-inst #:vm-graphic-char-p-value #:vm-hash-ensure-writable #:vm-hash-htm-eligible-p
    #:vm-hash-table-get-internal
    #:VM-HASH-TABLE-HTM-ABORT-COUNT #:vm-hash-with-lock-fallback #:vm-import #:vm-import-inst #:vm-intern-symbol #:vm-keywordp #:vm-list-all-packages #:vm-list-all-packages-inst #:vm-lower-case-p-value #:vm-make-host-hash-table
    #:vm-make-symbol #:vm-name-char #:vm-name-char-value #:vm-nconc #:vm-nreverse #:vm-package-name #:vm-package-name-inst #:vm-parse-float #:vm-parse-integer #:vm-record-weak-hash-entry #:vm-remprop
    #:vm-row-major-aref #:vm-sbit #:vm-set-fill-pointer #:vm-set-symbol-plist #:vm-shadow #:vm-shadow-inst #:vm-standard-char-p-value #:vm-string-copy #:vm-string-materialize #:vm-string-set-char
    #:vm-string-subseq #:vm-subst #:vm-svset #:vm-sweep-weak-hash-table #:vm-symbol-get #:vm-symbol-name #:vm-symbol-plist #:vm-symbol-plist-read-snapshot #:vm-symbol-property-hash-store #:vm-symbol-set
    #:vm-unintern #:vm-unintern-inst #:vm-unuse-package #:vm-unuse-package-inst #:vm-upper-case-p-value #:vm-use-package #:vm-use-package-inst #:vm-valid-hash-weakness-p #:vm-vector-pop #:vm-vector-push
    #:vm-vector-push-extend #:vm-weak-hash-entry #:hash-test-symbol #:*hash-test-function-alist* #:%make-vm-random-state #:vm-random-state-index #:vm-random-state-mt #:vm-arr #:vm-copy-on-write #:vm-copy-on-write-reg
    #:vm-default #:vm-dims #:vm-displaced-index-offset-reg #:vm-element-regs #:vm-export-pkg #:vm-find-symbol-pkg #:vm-import-pkg #:vm-import-symbols #:vm-index-regs #:vm-new-val
    #:vm-old-val #:vm-plist-reg #:vm-shadow-names #:vm-shadow-pkg #:vm-size #:vm-subs #:vm-tree #:vm-unintern-pkg #:vm-unintern-symbol #:vm-unuse-packages
    #:vm-unuse-pkg #:vm-use-packages #:vm-use-pkg #:vm-val #:vm-idx #:vm-cow-list-backing #:vm-cow-list-p #:vm-cow-vector-p #:vm-dimensions-reg #:vm-export-symbols
    #:vm-hash-rehash-size #:vm-hash-rehash-threshold #:vm-hash-size #:vm-hash-table-weak-entries #:vm-package-name-pkg
  )
)

