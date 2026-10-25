(in-package #:cl-cc/vm/execution)

(defmethod execute-instruction ((inst vm-hash-table-test) state pc labels)
  (declare (ignore labels))
  (let* ((table-obj (vm-reg-get state (vm-hash-table-reg inst)))
         (table (vm-hash-table-get-internal table-obj))
         (test-val (hash-table-test table))
         (test-sym (hash-test-symbol test-val)))
    (vm-reg-set state (vm-dst inst) test-sym)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-hash-table-values) state pc labels)
  (declare (ignore labels))
  (let ((table-obj (vm-reg-get state (vm-hash-table-reg inst))))
    (when (typep table-obj 'vm-hash-table-object)
      (vm-sweep-weak-hash-table table-obj))
    (let* ((table (vm-hash-table-get-internal table-obj))
           (values (loop for val being the hash-values of table collect val)))
      (vm-reg-set state (vm-dst inst) values)
      (values (1+ pc) nil nil))))

(defmethod execute-instruction ((inst vm-hash-table-keys) state pc labels)
  (declare (ignore labels))
  (let ((table-obj (vm-reg-get state (vm-hash-table-reg inst))))
    (when (typep table-obj 'vm-hash-table-object)
      (vm-sweep-weak-hash-table table-obj))
    (let* ((table (vm-hash-table-get-internal table-obj))
           (keys (loop for key being the hash-keys of table collect key)))
      (vm-reg-set state (vm-dst inst) keys)
      (values (1+ pc) nil nil))))

(defmethod execute-instruction ((inst vm-hash-table-p) state pc labels)
  (declare (ignore labels))
  (let* ((value (vm-reg-get state (vm-src inst)))
         (result (if (or (typep value 'vm-hash-table-object)
                         (hash-table-p value))
                     1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-hash-table-count) state pc labels)
  (declare (ignore labels))
  (let ((table-obj (vm-reg-get state (vm-hash-table-reg inst))))
    (when (typep table-obj 'vm-hash-table-object)
      (vm-sweep-weak-hash-table table-obj))
    (let* ((table (vm-hash-table-get-internal table-obj))
           (count (hash-table-count table)))
      (vm-reg-set state (vm-dst inst) count)
      (values (1+ pc) nil nil))))

(defmethod execute-instruction ((inst vm-clrhash) state pc labels)
  (declare (ignore labels))
  (let* ((table-obj (vm-reg-get state (vm-hash-table-reg inst)))
         (table (vm-hash-table-get-internal (vm-hash-ensure-writable table-obj))))
    (if (typep table-obj 'vm-hash-table-object)
        (vm-hash-with-lock-fallback table-obj
                                    (lambda ()
                                       (clrhash table)))
        (clrhash table))
    (when (and (typep table-obj 'vm-hash-table-object)
               (vm-hash-table-weak-entries table-obj))
      (clrhash (vm-hash-table-weak-entries table-obj)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-remhash) state pc labels)
  (declare (ignore labels))
  (let* ((key (vm-reg-get state (vm-hash-key inst)))
         (table-obj (vm-reg-get state (vm-hash-table-reg inst)))
         (table (vm-hash-table-get-internal (vm-hash-ensure-writable table-obj))))
    (if (typep table-obj 'vm-hash-table-object)
        (vm-hash-with-lock-fallback table-obj
                                    (lambda ()
                                       (remhash key table)))
        (remhash key table))
    (when (and (typep table-obj 'vm-hash-table-object)
               (vm-hash-table-weak-entries table-obj))
      (remhash key (vm-hash-table-weak-entries table-obj)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-sethash) state pc labels)
  (declare (ignore labels))
  (let* ((key (vm-reg-get state (vm-hash-key inst)))
         (value (vm-reg-get state (vm-hash-value inst)))
         (table-obj (vm-reg-get state (vm-hash-table-reg inst)))
         (table (vm-hash-table-get-internal (vm-hash-ensure-writable table-obj))))
    (if (typep table-obj 'vm-hash-table-object)
        (vm-hash-with-lock-fallback table-obj
                                    (lambda ()
                                       (setf (gethash key table) value)))
        (setf (gethash key table) value))
    (vm-record-weak-hash-entry table-obj key value)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-gethash-equal) state pc labels)
  (declare (ignore labels))
  (%execute-vm-specialized-gethash inst state pc))

(defmethod execute-instruction ((inst vm-gethash-eql) state pc labels)
  (declare (ignore labels))
  (%execute-vm-specialized-gethash inst state pc))

(defmethod execute-instruction ((inst vm-gethash-eq) state pc labels)
  (declare (ignore labels))
  (%execute-vm-specialized-gethash inst state pc))

(defmethod execute-instruction ((inst vm-gethash) state pc labels)
  (declare (ignore labels))
  (%execute-vm-gethash inst state pc))

;;; Instruction Execution - Hash Table Operations

(defmethod execute-instruction ((inst vm-make-hash-table) state pc labels)
  (declare (ignore labels))
  (let* ((test-sym (when (vm-hash-test inst)
                        (vm-reg-get state (vm-hash-test inst))))
          (size (when (vm-hash-size inst)
                  (vm-reg-get state (vm-hash-size inst))))
          (rehash-size (when (vm-hash-rehash-size inst)
                         (vm-reg-get state (vm-hash-rehash-size inst))))
          (rehash-threshold (when (vm-hash-rehash-threshold inst)
                              (vm-reg-get state (vm-hash-rehash-threshold inst))))
          (weakness (when (vm-hash-weakness inst)
                      (vm-reg-get state (vm-hash-weakness inst))))
          (test-designator (vm-resolve-hash-test test-sym))
          (hash-obj (rt-make-hash-table :test test-designator
                                        :size size
                                        :rehash-size rehash-size
                                        :rehash-threshold rehash-threshold
                                        :weakness weakness)))
    (vm-reg-set state (vm-dst inst) hash-obj)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-set-fill-pointer) state pc labels)
  (declare (ignore labels))
  (let* ((arr (vm-reg-get state (vm-array-reg inst)))
         (fp (vm-reg-get state (vm-val-reg inst))))
    (setf (fill-pointer (%vm-cow-vector-ensure-writable arr)) fp)
    (vm-reg-set state (vm-dst inst) fp)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-vector-push) state pc labels)
  (declare (ignore labels))
  (let ((val (vm-reg-get state (vm-val-reg inst)))
        (arr (vm-reg-get state (vm-array-reg inst))))
    (vm-reg-set state (vm-dst inst) (vector-push val (%vm-cow-vector-ensure-writable arr)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-svset) state pc labels)
  (declare (ignore labels))
  (let* ((arr (%vm-cow-vector-ensure-writable (vm-reg-get state (vm-array-reg inst))))
         (idx (vm-reg-get state (vm-index-reg inst)))
         (val (vm-reg-get state (vm-val-reg inst))))
    (vm-check-index arr idx 'svset)
    (setf (svref arr idx) val)
    (vm-reg-set state (vm-dst inst) val)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-svref) state pc labels)
  (declare (ignore labels))
  (let ((vector (%vm-cow-vector-materialize (vm-reg-get state (vm-lhs inst))))
        (index (vm-reg-get state (vm-rhs inst))))
    (vm-check-index vector index 'svref)
    (vm-reg-set state (vm-dst inst) (svref vector index)))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-array-row-major-index) state pc labels)
  (declare (ignore labels))
  (let ((arr (vm-reg-get state (vm-arr inst)))
        (subs (vm-reg-get state (vm-subs inst))))
    (vm-reg-set state (vm-dst inst) (apply #'array-row-major-index (%vm-array-object arr) subs))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-row-major-aref) state pc labels) (declare (ignore labels)) (let ((array (%vm-array-object (vm-reg-get state (vm-lhs inst)))) (index (vm-reg-get state (vm-rhs inst)))) (vm-reg-set state (vm-dst inst) (if (vm-specialized-array-p array) (vm-specialized-array-ref array index) (progn (vm-check-row-major-index array index) (row-major-aref array index))))) (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-array-in-bounds-p) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (if (vm-array-in-bounds-p-value (vm-reg-get state (vm-arr inst))
                                              (vm-reg-get state (vm-subs inst)))
                  t
                  nil))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-array-dimension) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (vm-array-dimension-value (vm-reg-get state (vm-lhs inst))
                                        (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-vectorp) state pc labels)
  (declare (ignore labels))
  (let ((src (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst)
                (if (or (vectorp src) (vm-cow-vector-p src)
                        (vm-specialized-array-p src)) 1 0))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-array-length) state pc labels)
  (declare (ignore labels))
  (let ((src (%vm-cow-vector-materialize (vm-reg-get state (vm-src inst)))))
    (vm-reg-set state (vm-dst inst)
                (if (vm-specialized-array-p src)
                    (vm-specialized-array-length src)
                    (length src)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-vector-push-extend) state pc labels)
  (declare (ignore labels))
  (let ((val (vm-reg-get state (vm-val-reg inst)))
        (arr (%vm-cow-vector-ensure-writable (vm-reg-get state (vm-array-reg inst)))))
    (vm-reg-set state (vm-dst inst) (vector-push-extend val arr))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-vector) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (coerce (mapcar (lambda (reg) (vm-reg-get state reg))
                              (vm-element-regs inst))
                      'vector))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-copy-vector) state pc labels)
  (declare (ignore labels))
  (let ((dst (%vm-cow-vector-ensure-writable (vm-reg-get state (vm-dst-array-reg inst))))
        (src (%vm-cow-vector-materialize (vm-reg-get state (vm-src-array-reg inst))))
        (len (vm-reg-get state (vm-len-reg inst))))
    (cond
      ((or (vm-specialized-array-p dst) (vm-specialized-array-p src))
       (loop for i below len
             for value = (if (vm-specialized-array-p src)
                             (vm-specialized-array-ref src i)
                             (aref src i))
             do (if (vm-specialized-array-p dst)
                    (setf (vm-specialized-array-ref dst i) value)
                    (setf (aref dst i) value))))
      (t
       (replace dst src :end1 len :end2 len)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-fill) state pc labels)
  (declare (ignore labels))
  (let ((arr (%vm-cow-vector-ensure-writable (vm-reg-get state (vm-array-reg inst))))
        (val (vm-reg-get state (vm-val-reg inst))))
    (if (vm-specialized-array-p arr)
        (loop for i below (vm-specialized-array-length arr)
              do (setf (vm-specialized-array-ref arr i) val))
        (fill arr val))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-aset) state pc labels)
  (declare (ignore labels))
  (let ((arr (%vm-cow-vector-ensure-writable (vm-reg-get state (vm-array-reg inst))))
        (idx (vm-reg-get state (vm-index-reg inst)))
        (val (vm-reg-get state (vm-val-reg inst))))
    (unless (vm-specialized-array-p arr)
      (vm-check-index arr idx 'aset))
    (if (vm-specialized-array-p arr)
        (setf (vm-specialized-array-ref arr idx) val)
        (setf (aref arr idx) val))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-aref-multi) state pc labels)
  (declare (ignore labels))
  (let ((arr (%vm-cow-vector-materialize (vm-reg-get state (vm-array-reg inst))))
        (idxs (mapcar (lambda (r) (vm-reg-get state r)) (vm-index-regs inst))))
    (when (vm-safety-checks-enabled-p)
      (unless (apply #'array-in-bounds-p arr idxs)
        (error 'type-error :datum idxs :expected-type 'valid-array-subscripts)))
    (vm-reg-set state (vm-dst inst) (apply #'aref arr idxs))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-aref) state pc labels)
  (declare (ignore labels))
  (let ((arr (%vm-cow-vector-materialize (vm-reg-get state (vm-array-reg inst))))
        (idx (vm-reg-get state (vm-index-reg inst))))
    (unless (vm-specialized-array-p arr)
      (vm-check-index arr idx 'aref))
    (vm-reg-set state (vm-dst inst)
                (if (vm-specialized-array-p arr)
                    (vm-specialized-array-ref arr idx)
                    (aref arr idx)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-make-array) state pc labels)
  (declare (ignore labels))
  (let* ((size (vm-reg-get state (vm-size-reg inst)))
          (dimensions (and (vm-dimensions-reg inst)
                           (vm-reg-get state (vm-dimensions-reg inst))))
          (dimensions-designator (%vm-array-dimensions-designator size dimensions))
          (total-size (%vm-array-total-size dimensions-designator))
          (vector-dimensions-p (%vm-array-vector-dimensions-p dimensions-designator))
          (init-present-p (vm-initial-element inst))
          (fp (if (vm-fill-pointer-reg inst)
                  (vm-reg-get state (vm-fill-pointer-reg inst))
                  (vm-fill-pointer inst)))
          (adj (if (vm-adjustable-reg inst)
                   (vm-reg-get state (vm-adjustable-reg inst))
                   (vm-adjustable inst)))
          (elt-type (if (vm-element-type-reg inst)
                        (vm-reg-get state (vm-element-type-reg inst))
                        (vm-element-type inst)))
          (displaced-to (and (vm-displaced-to-reg inst)
                              (vm-reg-get state (vm-displaced-to-reg inst))))
           (displaced-index-offset (if (vm-displaced-index-offset-reg inst)
                                       (vm-reg-get state (vm-displaced-index-offset-reg inst))
                                       0))
           (copy-on-write (if (vm-copy-on-write-reg inst)
                              (vm-reg-get state (vm-copy-on-write-reg inst))
                              (vm-copy-on-write inst)))
          (default-init (case elt-type
                          (character #\Nul)
                          (single-float 0.0f0)
                         (double-float 0.0d0)
                         (bit 0)
                         (otherwise 0)))
          (init-elem (if init-present-p
                         (vm-reg-get state (vm-initial-element inst))
                         default-init))
           (specialized-type (%vm-normalize-specialized-element-type elt-type))
           (arr (cond
                     (displaced-to
                      (%vm-make-displaced-array dimensions-designator elt-type displaced-to
                                                displaced-index-offset fp adj))
                    ((and vector-dimensions-p
                          (member specialized-type '(:fixnum :double-float :character :bit)
                                   :test #'eq)
                           (not fp)
                          (not adj)
                          (not init-present-p))
                     (vm-make-specialized-array total-size specialized-type))
                   (t
                    (%vm-make-standard-array dimensions-designator elt-type init-elem fp adj)))))
    (vm-reg-set state (vm-dst inst) (if copy-on-write (%vm-cow-vector-share arr) arr))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-array-displacement) state pc labels) (declare (ignore labels)) (let ((array (%vm-cow-vector-materialize (vm-reg-get state (vm-src inst))))) (multiple-value-bind (displaced-to offset) (if (vm-specialized-array-p array) (values nil 0) (array-displacement array)) (vm-reg-set state (vm-dst inst) displaced-to) (setf (vm-values-list state) (list displaced-to offset)) (values (1+ pc) nil nil))))

(defmethod execute-instruction ((inst vm-adjust-array) state pc labels) (declare (ignore labels)) (let* ((arr (vm-reg-get state (vm-arr inst))) (dims (vm-reg-get state (vm-dims inst))) (new-dims (if (or (integerp dims) (vectorp dims)) dims (copy-list dims))) (init-present-p (vm-initial-element inst)) (fp (if (vm-fill-pointer-reg inst) (vm-reg-get state (vm-fill-pointer-reg inst)) (vm-fill-pointer inst))) (elt-type (if (vm-element-type-reg inst) (vm-reg-get state (vm-element-type-reg inst)) (vm-element-type inst))) (displaced-to (and (vm-displaced-to-reg inst) (%vm-cow-vector-materialize (vm-reg-get state (vm-displaced-to-reg inst))))) (displaced-index-offset (if (vm-displaced-index-offset-reg inst) (vm-reg-get state (vm-displaced-index-offset-reg inst)) 0)) (base-arr (%vm-cow-vector-ensure-writable arr)) (args (append (list base-arr new-dims) (when init-present-p (list :initial-element (vm-reg-get state (vm-initial-element inst)))) (when fp (list :fill-pointer (if (eq fp t) 0 fp))) (when elt-type (list :element-type elt-type)) (when displaced-to (list :displaced-to displaced-to :displaced-index-offset displaced-index-offset))))) (vm-reg-set state (vm-dst inst) (apply #'adjust-array args)) (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-sbit) state pc labels) (declare (ignore labels)) (let ((arr (%vm-cow-vector-materialize (vm-reg-get state (vm-arr inst)))) (idx (vm-reg-get state (vm-idx inst)))) (vm-reg-set state (vm-dst inst) (if (vm-bit-vector-p arr) (vm-bit-vector-ref arr idx) (sbit arr idx)))) (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-bit-set) state pc labels) (declare (ignore labels)) (let ((arr (%vm-cow-vector-ensure-writable (vm-reg-get state (vm-arr inst)))) (idx (vm-reg-get state (vm-idx inst))) (v (vm-reg-get state (vm-val inst)))) (cond ((vm-bit-vector-p arr) (setf (vm-bit-vector-ref arr idx) v)) ((bit-vector-p arr) (setf (bit arr idx) v)) (t (setf (aref arr idx) v))) (vm-reg-set state (vm-dst inst) v) (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-bit-access) state pc labels) (declare (ignore labels)) (let ((arr (%vm-cow-vector-materialize (vm-reg-get state (vm-arr inst)))) (idx (vm-reg-get state (vm-idx inst)))) (vm-reg-set state (vm-dst inst) (cond ((vm-bit-vector-p arr) (vm-bit-vector-ref arr idx)) ((bit-vector-p arr) (bit arr idx)) (t (aref arr idx))))) (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-set-symbol-plist) state pc labels)
  (declare (ignore labels))
  (let* ((sym (vm-reg-get state (vm-sym inst)))
         (new-plist (vm-reg-get state (vm-plist-reg inst))))
    (vm-reg-set state (vm-dst inst)
                (%vm-symbol-property-replace-plist state (vm-symbol-plists state)
                                                   sym new-plist :sync-host-p t))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-symbol-plist) state pc labels)
  (declare (ignore labels))
  (let* ((sym (vm-reg-get state (vm-src inst)))
         (plist (nth-value 0 (vm-symbol-plist-read-snapshot state sym))))
    (vm-reg-set state (vm-dst inst) plist)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-remprop) state pc labels)
  (declare (ignore labels))
  (let* ((sym (vm-reg-get state (vm-sym inst)))
         (indicator (vm-reg-get state (vm-indicator inst)))
         (found-p (%vm-symbol-property-remprop state (vm-symbol-plists state)
                                               sym indicator :sync-host-p t)))
    (vm-reg-set state (vm-dst inst) (if found-p t nil))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-symbol-set) state pc labels)
  (declare (ignore labels))
  (let* ((sym (vm-reg-get state (vm-sym inst)))
         (indicator (vm-reg-get state (vm-indicator inst)))
         (val (vm-reg-get state (vm-value inst))))
    (%vm-symbol-property-set state (vm-symbol-plists state)
                             sym indicator val :sync-host-p t)
    (vm-reg-set state (vm-dst inst) val)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-symbol-get) state pc labels)
  (declare (ignore labels))
  (let* ((sym (vm-reg-get state (vm-sym inst)))
         (indicator (vm-reg-get state (vm-indicator inst)))
         (default (vm-reg-get state (vm-default inst)))
         (result (%vm-symbol-property-get state (vm-symbol-plists state)
                                          sym indicator default)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-digit-char-p) state pc labels)
  (declare (ignore labels))
  (let* ((ch (vm-reg-get state (vm-src inst)))
         (result (vm-digit-char-p-value ch)))
    (vm-reg-set state (vm-dst inst) (or result nil))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-list-all-packages-inst) state pc labels)
  (declare (ignore labels))
  (let ((result (%vm-registry-result #'cl-cc/runtime::rt-list-all-packages
                                     #'list-all-packages)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-package-name-inst) state pc labels)
  (declare (ignore labels))
  (let* ((pkg (vm-reg-get state (vm-package-name-pkg inst)))
         (result (%vm-package-result pkg
                                     #'cl-cc/runtime::rt-package-name
                                     #'package-name
                                     pkg)))
    (vm-reg-set state (vm-dst inst)
                (%vm-maybe-sso-string result))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-unintern-inst) state pc labels)
  (declare (ignore labels))
  (let* ((sym (vm-reg-get state (vm-unintern-symbol inst)))
         (pkg (vm-reg-get state (vm-unintern-pkg inst)))
         (result (%vm-package-operation-result
                  pkg
                  (function cl-cc/runtime::rt-unintern)
                  (function unintern)
                  (list sym pkg))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-shadow-inst) state pc labels)
  (declare (ignore labels))
  (let* ((names (vm-reg-get state (vm-shadow-names inst)))
         (pkg (vm-reg-get state (vm-shadow-pkg inst)))
         (result (%vm-package-operation-result
                  pkg
                  (function cl-cc/runtime::rt-shadow)
                  (function shadow)
                  (list names pkg))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-unuse-package-inst) state pc labels)
  (declare (ignore labels))
  (let* ((pkgs (vm-reg-get state (vm-unuse-packages inst)))
         (pkg (vm-reg-get state (vm-unuse-pkg inst)))
         (result (%vm-package-operation-result
                  pkg
                  (function cl-cc/runtime::rt-unuse-package)
                  (function unuse-package)
                  (list pkgs pkg)
                  0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-use-package-inst) state pc labels)
  (declare (ignore labels))
  (let* ((pkgs (vm-reg-get state (vm-use-packages inst)))
         (pkg (vm-reg-get state (vm-use-pkg inst)))
         (result (%vm-package-operation-result
                  pkg
                  (function cl-cc/runtime::rt-use-package)
                  (function use-package)
                  (list pkgs pkg)
                  0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-import-inst) state pc labels)
  (declare (ignore labels))
  (let* ((symbols (vm-reg-get state (vm-import-symbols inst)))
         (pkg (vm-reg-get state (vm-import-pkg inst)))
         (result (%vm-package-operation-result
                  pkg
                  (function cl-cc/runtime::rt-import)
                  (function import)
                  (list symbols pkg))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-export-inst) state pc labels)
  (declare (ignore labels))
  (let* ((symbols (vm-reg-get state (vm-export-symbols inst)))
         (pkg (vm-reg-get state (vm-export-pkg inst)))
         (result (%vm-package-operation-result
                  pkg
                  (function cl-cc/runtime::rt-export)
                  (function export)
                  (list symbols pkg))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-gensym-inst) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (gensym))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-find-symbol) state pc labels)
  (declare (ignore labels))
  (let* ((name (string (%vm-host-string (vm-reg-get state (vm-src inst)))))
         (package-supplied-p (vm-find-symbol-pkg inst))
         (package-designator
           (when package-supplied-p
             (vm-reg-get state (vm-find-symbol-pkg inst)))))
    (multiple-value-bind (symbol status)
        (if package-supplied-p
            (multiple-value-bind (runtime-package host-designator)
                (%vm-resolve-package-designator package-designator)
              (if runtime-package
                  (let* ((table (gethash :symbols runtime-package))
                         (exports (gethash :exports runtime-package)))
                    (multiple-value-bind (found present-p)
                        (if table (gethash name table) (values nil nil))
                      (if present-p
                          (values found
                                  (if (member found exports :test (function eq))
                                      :external
                                      :internal))
                          (if (packagep host-designator)
                              (find-symbol name host-designator)
                              (values nil nil)))))
                  (find-symbol name host-designator)))
            (find-symbol name))
      (vm-reg-set state (vm-dst inst) symbol)
      (setf (vm-values-list state) (list symbol status))
      (values (1+ pc) nil nil))))

(defmethod execute-instruction ((inst vm-find-package) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (runtime-package host-designator)
      (%vm-resolve-package-designator (vm-reg-get state (vm-src inst)))
    (let ((result
            (or runtime-package
                (and *vm-self-host-mode*
                     (not (packagep host-designator))
                     (error "Package self-hosting required: package ~S not found in runtime registry"
                            (vm-reg-get state (vm-src inst))))
                (find-package host-designator))))
      (vm-reg-set state (vm-dst inst) result)
      (values (1+ pc) nil nil))))

(defmethod execute-instruction ((inst vm-remove-package-local-nickname)
                                  state pc labels)
  (declare (ignore labels))
  (let* ((pkg-designator (vm-reg-get state (vm-local-nickname-pkg inst)))
         (nickname (vm-reg-get state (vm-local-nickname-nick inst)))
         (fn (%vm-host-package-local-nickname-function
              "REMOVE-PACKAGE-LOCAL-NICKNAME")))
    (multiple-value-bind (runtime-package host-package)
        (%vm-resolve-package-designator pkg-designator)
      (declare (ignore runtime-package))
      (let ((result (funcall fn nickname host-package)))
        (vm-reg-set state (vm-dst inst) result)
        (values (1+ pc) nil nil)))))

(defmethod execute-instruction ((inst vm-add-package-local-nickname) state pc labels)
  (declare (ignore labels))
  (let* ((pkg-designator (vm-reg-get state (vm-local-nickname-pkg inst)))
         (nickname (vm-reg-get state (vm-local-nickname-nick inst)))
         (target (vm-reg-get state (vm-local-nickname-target inst)))
         (fn (%vm-host-package-local-nickname-function
              "ADD-PACKAGE-LOCAL-NICKNAME")))
    (multiple-value-bind (runtime-target host-target)
        (%vm-resolve-package-designator target)
      (declare (ignore runtime-target))
      (multiple-value-bind (runtime-package host-package)
          (%vm-resolve-package-designator pkg-designator)
        (declare (ignore runtime-package))
        (let ((result (funcall fn nickname host-target host-package)))
          (vm-reg-set state (vm-dst inst) result)
          (values (1+ pc) nil nil))))))

(defmethod execute-instruction ((inst vm-intern-symbol) state pc labels)
  (declare (ignore labels))
  (let* ((name (string (%vm-host-string (vm-reg-get state (vm-src inst)))))
         (package-supplied-p (vm-intern-pkg inst))
         (package-designator
           (when package-supplied-p
             (vm-reg-get state (vm-intern-pkg inst))))
         (result
           (cond
             ((not package-supplied-p)
              (cond
                (cl-cc/bootstrap:*runtime-intern-fn*
                 (funcall cl-cc/bootstrap:*runtime-intern-fn* name))
                (*vm-self-host-mode*
                 (error "Package self-hosting required: cannot intern ~S without runtime package registry"
                        name))
                (t
                 (intern name))))
             (t
              (multiple-value-bind (runtime-package host-designator)
                  (%vm-resolve-package-designator package-designator)
                (cond
                  (runtime-package
                   (cond
                     (cl-cc/bootstrap:*runtime-intern-fn*
                      (funcall cl-cc/bootstrap:*runtime-intern-fn*
                               name runtime-package))
                     (*vm-self-host-mode*
                      (error "Package self-hosting required: cannot intern ~S without runtime package registry"
                             name))
                     (t
                      (cl-cc/runtime:rt-intern name runtime-package))))
                  (*vm-self-host-mode*
                   (error "Package self-hosting required: cannot intern ~S without runtime package registry"
                          name))
                  (t
                   (intern name (find-package host-designator)))))))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-symbol-name) state pc labels)
  (declare (ignore labels))
  (let ((result (%vm-maybe-sso-string (symbol-name (vm-reg-get state (vm-src inst))))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

;;; Fast path: immediate symbols don't need the general symbol-name path.
(defmethod execute-instruction :around ((inst vm-symbol-name) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (if (vm-immediate-symbol-p value)
        (progn
          (vm-reg-set state (vm-dst inst)
                      (rt-string-intern (symbol-name (vm-decode-symbol value))))
          (values (1+ pc) nil nil))
        (call-next-method))))

(defmethod execute-instruction :around ((inst vm-keywordp) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (if (vm-immediate-symbol-p value)
        (progn
          (vm-reg-set state (vm-dst inst)
                      (if (keywordp (vm-decode-symbol value)) 1 0))
          (values (1+ pc) nil nil))
        (call-next-method))))

;;; Immediate-symbol fast paths: bypass the general execute-instruction method
;;; when the source value is a packed immediate symbol (vm-immediate-symbol-p).

(defmethod execute-instruction :around ((inst vm-intern-symbol) state pc labels)
  (multiple-value-bind (next-pc value-1 value-2) (call-next-method)
    (when (vm-immediate-intern-enabled-p)
      (let ((value (vm-reg-get state (vm-dst inst))))
        (when (symbolp value)
          (vm-reg-set state (vm-dst inst) (vm-encode-common-symbol value)))))
    (values next-pc value-1 value-2)))

(defmethod execute-instruction ((inst vm-string-set) state pc labels)
  (declare (ignore labels))
  (let ((v (vm-reg-get state (vm-val-reg inst))))
    (vm-string-set-char (vm-reg-get state (vm-str-reg inst))
                        (vm-reg-get state (vm-idx inst)) v)
    (vm-reg-set state (vm-dst inst) v)
    (values (1+ pc) nil nil)))

;;; Instruction Execution - String Search

(defmethod execute-instruction ((inst vm-search-string) state pc labels)
  (declare (ignore labels))
  (let* ((pattern (%vm-host-string (vm-reg-get state (vm-pattern inst))))
         (string (%vm-host-string (vm-reg-get state (vm-string-reg inst))))
         (start (vm-reg-get state (vm-start inst)))
         (result (or (search pattern string :start2 start) -1)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-concatenate) state pc labels)
  (declare (ignore labels))
  (let* ((parts (or (vm-parts inst)
                     (list (vm-str1 inst) (vm-str2 inst))))
          (result (%vm-maybe-sso-string
                   (apply #'concatenate 'string
                          (mapcar (lambda (reg)
                                    (%vm-host-string (vm-reg-get state reg)))
                                  parts)))))
     (vm-reg-set state (vm-dst inst) result)
     (values (1+ pc) nil nil)))

;;; Instruction Execution - String Manipulation

(defmethod execute-instruction ((inst vm-subseq) state pc labels)
  (declare (ignore labels))
  ;; SUBSEQ is a sequence operation: it must work on lists and vectors, not only
  ;; strings.  The previous version coerced the input to a string, so (subseq
  ;; '(1 2 3) 0 2) and (subseq #(1 2 3) 0 2) — and butlast/last, which expand to
  ;; subseq — raised a type error.
  (let* ((seq   (vm-reg-get state (vm-string-reg inst)))
         (start (vm-reg-get state (vm-start inst)))
         ;; nil end-slot means no upper bound (= end of sequence)
         (end   (if (vm-end inst) (vm-reg-get state (vm-end inst)) nil))
         (result
           (cond
             ((listp seq)
              (subseq seq start (or end (length seq))))
             ((or (stringp seq) (%vm-sso-string-p seq))
              (%vm-maybe-sso-string
               (vm-string-materialize (vm-string-subseq (%vm-host-string seq) start end))))
             (t
              (let ((v (%vm-cow-vector-materialize seq)))
                (subseq v start (or end (length v))))))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-char) state pc labels)
  (declare (ignore labels))
  (let* ((string (vm-reg-get state (vm-string-reg inst)))
          (index (vm-reg-get state (vm-index inst)))
          (result (%vm-string-char string index)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

;;; Instruction Execution - String Access and Query

(defmethod execute-instruction ((inst vm-string-length) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (%vm-string-length (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-make-string) state pc labels)
  (declare (ignore labels))
  (let* ((size      (vm-reg-get state (vm-src inst)))
         (char-reg  (vm-char inst))
         (init-char (if char-reg (vm-reg-get state char-reg) #\Space)))
    (vm-reg-set state (vm-dst inst) (make-string size :initial-element init-char))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction :around ((inst vm-rplacd) state pc labels)
  (declare (ignore labels))
  (let ((cons-val (vm-reg-get state (vm-cons-reg inst))))
    (if (%vm-managed-cons-pointer-p cons-val)
        (progn
          (%vm-managed-cons-set-slot state cons-val 2
                                     (vm-reg-get state (vm-val-reg inst)))
          (vm-reg-set state (vm-cons-reg inst) cons-val)
          (values (1+ pc) nil nil))
        (call-next-method))))

(defmethod execute-instruction :around ((inst vm-rplaca) state pc labels)
  (declare (ignore labels))
  (let ((cons-val (vm-reg-get state (vm-cons-reg inst))))
    (if (%vm-managed-cons-pointer-p cons-val)
        (progn
          (%vm-managed-cons-set-slot state cons-val 1
                                     (vm-reg-get state (vm-val-reg inst)))
          (vm-reg-set state (vm-cons-reg inst) cons-val)
          (values (1+ pc) nil nil))
        (call-next-method))))

(defmethod execute-instruction :around ((inst vm-cdr) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (if (%vm-managed-cons-pointer-p value)
        (progn
          (vm-reg-set state (vm-dst inst) (%vm-managed-cons-slot state value 2))
          (values (1+ pc) nil nil))
        (call-next-method))))

(defmethod execute-instruction :around ((inst vm-car) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (if (%vm-managed-cons-pointer-p value)
        (progn
          (vm-reg-set state (vm-dst inst) (%vm-managed-cons-slot state value 1))
          (values (1+ pc) nil nil))
        (call-next-method))))

(defmethod execute-instruction :around ((inst vm-cons) state pc labels)
  (declare (ignore labels))
  (if *vm-managed-cons-allocation-enabled*
      (let ((car-val (vm-reg-get state (vm-car-reg inst)))
            (cdr-val (vm-reg-get state (vm-cdr-reg inst))))
        (vm-reg-set state (vm-dst inst)
                    (%vm-managed-cons-alloc state (vm-dst inst) car-val cdr-val))
        (values (1+ pc) nil nil))
      (let ((car-val (vm-reg-get state (vm-car-reg inst)))
            (cdr-val (vm-reg-get state (vm-cdr-reg inst))))
        (vm-reg-set state (vm-dst inst) (cons car-val cdr-val))
        (values (1+ pc) nil nil))))

(defmethod execute-instruction ((inst vm-atom) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst)
                (if (consp (%vm-list-structure-materialize state value)) 0 1))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-listp) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst)
                (if (listp (%vm-list-structure-materialize state value))
                    1
                    0))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-subst) state pc labels)
  (declare (ignore labels))
  (let ((new-val (vm-reg-get state (vm-new-val inst)))
        (old-val (vm-reg-get state (vm-old-val inst)))
        (tree (vm-reg-get state (vm-tree inst))))
    (vm-reg-set state (vm-dst inst)
                (subst new-val old-val
                       (%vm-list-structure-materialize state tree)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-copy-tree) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst)
                (copy-tree (%vm-list-structure-materialize state value)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-copy-list) state pc labels)
  (declare (ignore labels))
  (let ((list-val (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst)
                (copy-list (%vm-list-structure-materialize state list-val)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-nconc) state pc labels)
  (declare (ignore labels))
  (let* ((lhs (vm-reg-get state (vm-lhs inst)))
         (rhs (vm-reg-get state (vm-rhs inst)))
         (lhs-writable (%vm-cow-list-ensure-writable lhs))
         (rhs-value (%vm-cow-list-materialize rhs)))
    (vm-reg-set state (vm-dst inst) (nconc lhs-writable rhs-value))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-equal) state pc labels)
  (declare (ignore labels))
  (let* ((lhs (vm-reg-get state (vm-lhs inst)))
         (rhs (vm-reg-get state (vm-rhs inst)))
         (lhs-value (%vm-list-structure-materialize state lhs))
         (rhs-value (%vm-list-structure-materialize state rhs)))
    (vm-reg-set state (vm-dst inst) (if (equal lhs-value rhs-value) 1 0))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-acons) state pc labels)
  (declare (ignore labels))
  (let* ((key (vm-reg-get state (vm-key inst)))
         (value (vm-reg-get state (vm-value inst)))
         (alist (vm-reg-get state (vm-alist inst))))
    (vm-reg-set state (vm-dst inst) (acons key value alist))
    (values (1+ pc) nil nil)))

;; Execute instructions
(defmethod execute-instruction ((inst vm-assoc) state pc labels)
  (declare (ignore labels))
  (let* ((key (vm-reg-get state (vm-key inst)))
         (alist (vm-reg-get state (vm-alist inst)))
         (alist-value (%vm-list-structure-materialize state alist))
         (result (assoc key alist-value)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-pop) state pc labels)
  (declare (ignore labels))
  (let ((list (vm-reg-get state (vm-list-reg inst))))
    (vm-reg-set state (vm-dst inst)
                (if (%vm-managed-cons-pointer-p list)
                    (%vm-managed-cons-car state list)
                    (car list)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-push) state pc labels)
  (declare (ignore labels))
  (let ((item (vm-reg-get state (vm-item-reg inst)))
        (list (vm-reg-get state (vm-list-reg inst))))
    (vm-reg-set state (vm-dst inst) (cons item list))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-null) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst)
                (if (%vm-managed-nil-p value) 1 0))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-endp) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst)
                (if (%vm-managed-nil-p value) 1 0))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-list-length) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst)
                (if (%vm-managed-cons-pointer-p value)
                    (%vm-managed-list-length state value)
                    (list-length value)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-rplacd) state pc labels)
  (declare (ignore labels))
  (let ((cons-val (vm-reg-get state (vm-cons-reg inst)))
        (new-val (vm-reg-get state (vm-val-reg inst))))
    (rplacd (%vm-cow-list-ensure-writable cons-val) new-val)
    (vm-reg-set state (vm-cons-reg inst) cons-val)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-rplaca) state pc labels)
  (declare (ignore labels))
  (let ((cons-val (vm-reg-get state (vm-cons-reg inst)))
        (new-val (vm-reg-get state (vm-val-reg inst))))
    (rplaca (%vm-cow-list-ensure-writable cons-val) new-val)
    (vm-reg-set state (vm-cons-reg inst) cons-val)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-nreverse) state pc labels)
  (declare (ignore labels))
  (let* ((value (vm-reg-get state (vm-src inst)))
         (writable (%vm-cow-list-ensure-writable value)))
    (vm-reg-set state (vm-dst inst) (nreverse writable))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction :around ((inst vm-butlast) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (if (%vm-managed-cons-pointer-p value)
        (progn
          (vm-reg-set state (vm-dst inst)
                      (butlast (%vm-managed-cons-materialize state value)))
          (values (1+ pc) nil nil))
        (call-next-method))))

(defmethod execute-instruction :around ((inst vm-last) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (if (%vm-managed-cons-pointer-p value)
        (progn
          (vm-reg-set state (vm-dst inst)
                      (last (%vm-managed-cons-materialize state value)))
          (values (1+ pc) nil nil))
        (call-next-method))))

(defmethod execute-instruction :around ((inst vm-rest) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (if (%vm-managed-cons-pointer-p value)
        (progn
          (vm-reg-set state (vm-dst inst) (%vm-managed-cons-cdr state value))
          (values (1+ pc) nil nil))
        (call-next-method))))

(defmethod execute-instruction ((inst vm-nthcdr) state pc labels)
  (declare (ignore labels))
  (let ((index (vm-reg-get state (vm-index-reg inst)))
        (list (vm-reg-get state (vm-list-reg inst))))
    (vm-reg-set state (vm-dst inst)
                (if (%vm-managed-cons-pointer-p list)
                    (%vm-managed-list-tail state list index)
                    (nthcdr index (%vm-cow-list-materialize list))))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-nth) state pc labels)
  (declare (ignore labels))
  (let ((index (vm-reg-get state (vm-index-reg inst)))
        (sequence (vm-reg-get state (vm-list-reg inst))))
    (vm-reg-set state (vm-dst inst)
                (let ((*vm-managed-cons-sequence-state* state))
                  (vm-sequence-elt sequence index)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-member) state pc labels)
  (declare (ignore labels))
  (let ((item (vm-reg-get state (vm-item-reg inst)))
        (list (vm-reg-get state (vm-list-reg inst))))
    (vm-reg-set state (vm-dst inst)
                (if (%vm-managed-cons-pointer-p list)
                    (%vm-managed-member state item list)
                    (member item (%vm-cow-list-materialize list))))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction :around ((inst vm-append) state pc labels)
  (declare (ignore labels))
  (let ((lhs (vm-reg-get state (vm-lhs inst)))
        (rhs (vm-reg-get state (vm-rhs inst))))
    (if (or (%vm-managed-cons-pointer-p lhs)
            (%vm-managed-cons-pointer-p rhs))
        (progn
          (vm-reg-set state (vm-dst inst)
                      (append (%vm-managed-cons-materialize state lhs)
                              (%vm-managed-cons-materialize state rhs)))
          (values (1+ pc) nil nil))
        (call-next-method))))

(defmethod execute-instruction :around ((inst vm-reverse) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (if (%vm-managed-cons-pointer-p value)
        (progn
          (vm-reg-set state (vm-dst inst)
                      (reverse (%vm-managed-cons-materialize state value)))
          (values (1+ pc) nil nil))
        (call-next-method))))

;;; Instruction Execution - List Accessors

(defmethod execute-instruction ((inst vm-length) state pc labels)
  (declare (ignore labels))
  (let ((sequence (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst)
                (let ((*vm-managed-cons-sequence-state* state))
                  (vm-sequence-length sequence)))
    (values (1+ pc) nil nil)))

;;; Instruction Execution - List Construction

(defmethod execute-instruction ((inst vm-make-list) state pc labels)
  (declare (ignore labels))
  (let ((n (vm-reg-get state (vm-size inst))))
    (vm-reg-set state (vm-dst inst) (make-list n))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-cdr) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst) (cdr (%vm-cow-list-materialize value)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-car) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst) (car (%vm-cow-list-materialize value)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-hash-cons) state pc labels)
  (declare (ignore labels))
  (let ((car-val (vm-reg-get state (vm-car-reg inst)))
        (cdr-val (vm-reg-get state (vm-cdr-reg inst))))
    (vm-reg-set state (vm-dst inst) (vm-hash-cons car-val cdr-val))
    (values (1+ pc) nil nil)))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — List Instruction Execution Methods
;;;
;;; Contains all execute-instruction methods for list VM instructions:
;;; cons/car/cdr, make-list, length/member/nth/nthcdr, named accessors
;;; (first-tenth), rest/last/butlast, nreverse/nbutlast, push/pop,
;;; assoc/acons/subst, equal, nconc, copy-list/copy-tree, listp/atom.
;;;
;;; Instruction struct definitions (define-vm-instruction forms) and the
;;; hash-cons table + sequence protocol are in list.lisp (loads before).
;;;
;;; Load order: after list.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Instruction Execution - Cons Cell Operations

(defmethod execute-instruction ((inst vm-cons) state pc labels)
  (declare (ignore labels))
  (let ((car-val (vm-reg-get state (vm-car-reg inst)))
        (cdr-val (vm-reg-get state (vm-cdr-reg inst))))
    ;; CL CONS must produce a fresh cell. Reusing globally interned cons cells
    ;; breaks destructive operators (RPLACA/RPLACD/NREVERSE family) and can
    ;; accidentally create cyclic/shared syntax structures in user programs.
    ;; Keep VM-HASH-CONS available as an explicit utility, but ordinary VM-CONS
    ;; follows ANSI CL allocation semantics.
    (vm-reg-set state (vm-dst inst) (cons car-val cdr-val))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-coerce-to-vector) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (coerce (vm-reg-get state (vm-src inst)) 'vector))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-coerce-to-list) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (coerce (vm-reg-get state (vm-src inst)) 'list))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-coerce-to-string) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (coerce (vm-reg-get state (vm-src inst)) 'string))
  (values (1+ pc) nil nil))
