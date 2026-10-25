(in-package :cl-cc/vm)

;;; Array/vector storage layer beneath the VM instructions in array.lisp:
;;; copy-on-write vector storage/sharing, specialized (element-type-tagged)
;;; array storage, bit-vector storage, and the dimension/rank/element-type
;;; metadata accessors array.lisp's instruction macros dispatch through.

(defstruct (vm-cow-storage (:constructor %make-vm-cow-storage)) backing (refcount 1 :type integer))

(defstruct (vm-cow-vector (:constructor %make-vm-cow-vector)) (storage (%make-vm-cow-storage :backing #()) :type vm-cow-storage) (adjustable-p nil :type boolean))

(defun vm-cow-vector-backing (value) (vm-cow-storage-backing (vm-cow-vector-storage value)))

(defparameter *vm-cow-vector-enabled* t)

(defparameter +vm-specialized-array-tag+ :specialized-array
  "Header tag for VM arrays whose elements are stored unboxed by the host.")

(defparameter +vm-bit-vector-tag+ :bit-vector
  "Header tag for VM bit vectors packed at 64 bits per word.")

(defstruct (vm-specialized-array (:constructor %make-vm-specialized-array))
  "VM specialized one-dimensional array.

HEADER records the array tag, normalized element type, and GC-skip flag.  STORAGE
is a host specialized vector; bit arrays use (UNSIGNED-BYTE 64) words and pack
64 logical bits per storage word."
  (header nil :type list)
  (element-type :any :type keyword)
  (length 0 :type fixnum)
  (storage #() :type vector)
  (gc-skip-p nil :type boolean))

(defun %vm-normalize-specialized-element-type (element-type)
  "Normalize ELEMENT-TYPE to the VM specialized-array type vocabulary."
  (case element-type
    ((:fixnum fixnum integer :integer) :fixnum)
    ((:double-float double-float :double) :double-float)
    ((:character character :char) :character)
    ((:bit bit) :bit)
    ((t :any nil) :any)
    (otherwise element-type)))

(defun vm-specialized-array-element-pointer-free-p (element-type)
  "Return T (boolean) when ELEMENT-TYPE contains no GC-visible pointers."
  (not (null (member (%vm-normalize-specialized-element-type element-type)
                      '(:fixnum :double-float :character :bit)
                      :test #'eq))))

(defun %make-vm-specialized-array-header (element-type length &key (gc-skip-p t))
  "Build a metadata header for a VM specialized array."
  (let* ((type (%vm-normalize-specialized-element-type element-type))
         (tag (if (eq type :bit) +vm-bit-vector-tag+ +vm-specialized-array-tag+)))
    (list :type-tag tag
          :element-type type
          :length length
          :gc-skip-p (and gc-skip-p
                          (vm-specialized-array-element-pointer-free-p type)))))

(defun %vm-specialized-storage (length element-type)
  "Allocate host storage for a specialized VM array."
  (case (%vm-normalize-specialized-element-type element-type)
    (:fixnum (make-array length :element-type 'fixnum :initial-element 0))
    (:double-float (make-array length :element-type 'double-float :initial-element 0.0d0))
    (:character (make-array length :element-type 'character :initial-element #\Nul))
    (:bit (make-array (ceiling length 64)
                      :element-type '(unsigned-byte 64)
                      :initial-element 0))
    (otherwise (make-array length :initial-element nil))))

(defun vm-make-specialized-array (length element-type)
  "Create a VM specialized array of LENGTH and ELEMENT-TYPE.

Supported pointer-free ELEMENT-TYPE values are :FIXNUM, :DOUBLE-FLOAT,
:CHARACTER, and :BIT.  :BIT arrays are VM bit vectors packed at 64 bits per
storage word.  :ANY/T arrays retain pointer-scannable element semantics."
  (check-type length (integer 0 *))
  (let* ((type (%vm-normalize-specialized-element-type element-type))
         (gc-skip-p (vm-specialized-array-element-pointer-free-p type)))
    (%make-vm-specialized-array
     :header (%make-vm-specialized-array-header type length :gc-skip-p gc-skip-p)
     :element-type type
     :length length
     :storage (%vm-specialized-storage length type)
     :gc-skip-p gc-skip-p)))

(defun vm-specialized-array-ref (array index)
  "Read INDEX from specialized ARRAY."
  (check-type index (integer 0 *))
  (unless (< index (vm-specialized-array-length array))
    (error "Specialized array index ~D out of bounds for length ~D"
           index (vm-specialized-array-length array)))
  (if (eq (vm-specialized-array-element-type array) :bit)
      (multiple-value-bind (word-index bit-index) (floor index 64)
        (if (logbitp bit-index (aref (vm-specialized-array-storage array)
                                     word-index))
            1
            0))
      (aref (vm-specialized-array-storage array) index)))

(defun (setf vm-specialized-array-ref) (value array index)
  "Write VALUE at INDEX in specialized ARRAY."
  (check-type index (integer 0 *))
  (unless (< index (vm-specialized-array-length array))
    (error "Specialized array index ~D out of bounds for length ~D"
           index (vm-specialized-array-length array)))
  (case (vm-specialized-array-element-type array)
    (:bit
     (multiple-value-bind (word-index bit-index) (floor index 64)
       (let* ((storage (vm-specialized-array-storage array))
              (word (aref storage word-index))
              (mask (ash 1 bit-index)))
         (setf (aref storage word-index)
               (if (zerop value)
                   (logand word (lognot mask))
                   (logior word mask)))
         (if (zerop value) 0 1))))
    (:double-float
     (setf (aref (vm-specialized-array-storage array) index)
           (coerce value 'double-float)))
    (:character
     (check-type value character)
     (setf (aref (vm-specialized-array-storage array) index) value))
    (:fixnum
     (check-type value fixnum)
     (setf (aref (vm-specialized-array-storage array) index) value))
    (otherwise
     (setf (aref (vm-specialized-array-storage array) index) value))))

(defun vm-bit-vector-p (value) "Return true when VALUE is a VM packed bit vector." (let ((array (%vm-cow-vector-materialize value))) (and (vm-specialized-array-p array) (eq (vm-specialized-array-element-type array) :bit))))

(defun vm-bit-vector-ref (bit-vector index)
  "Return bit at INDEX from packed VM BIT-VECTOR."
  (unless (vm-bit-vector-p bit-vector)
    (error "Expected VM bit vector, got ~S" bit-vector))
  (vm-specialized-array-ref bit-vector index))

(defun (setf vm-bit-vector-ref) (value bit-vector index)
  "Set bit at INDEX in packed VM BIT-VECTOR."
  (unless (vm-bit-vector-p bit-vector)
    (error "Expected VM bit vector, got ~S" bit-vector))
  (setf (vm-specialized-array-ref bit-vector index) value))

(defun %vm-copy-array-backing (array) (cond ((vm-specialized-array-p array) (%make-vm-specialized-array :header (copy-list (vm-specialized-array-header array)) :element-type (vm-specialized-array-element-type array) :length (vm-specialized-array-length array) :storage (copy-seq (vm-specialized-array-storage array)) :gc-skip-p (vm-specialized-array-gc-skip-p array))) ((arrayp array) (let* ((dimensions (array-dimensions array)) (fill-pointer-p (and (= (array-rank array) 1) (array-has-fill-pointer-p array))) (arguments (append (list dimensions :element-type (array-element-type array) :adjustable (adjustable-array-p array)) (when fill-pointer-p (list :fill-pointer (fill-pointer array))))) (copy (apply #'make-array arguments))) (dotimes (index (array-total-size array) copy) (setf (row-major-aref copy index) (row-major-aref array index))))) (t (copy-seq array))))

(defun %vm-cow-vector-materialize (value)
  (cond
    ((vm-cow-vector-p value) (vm-cow-vector-backing value))
    (t value)))

(defun %vm-cow-vector-share (value) (cond ((vm-cow-vector-p value) (let ((storage (vm-cow-vector-storage value))) (incf (vm-cow-storage-refcount storage)) (%make-vm-cow-vector :storage storage :adjustable-p (vm-cow-vector-adjustable-p value)))) ((or (arrayp value) (vm-specialized-array-p value)) (%make-vm-cow-vector :storage (%make-vm-cow-storage :backing value :refcount 2) :adjustable-p (and (arrayp value) (adjustable-array-p value)))) (t value)))

(defun %vm-cow-vector-ensure-writable (value) (if (vm-cow-vector-p value) (let ((storage (vm-cow-vector-storage value))) (when (> (vm-cow-storage-refcount storage) 1) (decf (vm-cow-storage-refcount storage)) (setf (vm-cow-vector-storage value) (%make-vm-cow-storage :backing (%vm-copy-array-backing (vm-cow-storage-backing storage))))) (vm-cow-vector-backing value)) value))

(defun %vm-array-dimensions-designator (size dimensions)
  "Return a MAKE-ARRAY dimensions designator from SIZE and optional DIMENSIONS."
  (let ((raw (or dimensions size)))
    (cond
      ((vectorp raw) (coerce raw 'list))
      ((listp raw) raw)
      (t raw))))

(defun %vm-array-total-size (dimensions-designator)
  "Return row-major total size for DIMENSIONS-DESIGNATOR."
  (if (listp dimensions-designator)
      (reduce #'* dimensions-designator :initial-value 1)
      dimensions-designator))

(defun %vm-array-vector-dimensions-p (dimensions-designator)
  "Return true when DIMENSIONS-DESIGNATOR denotes a one-dimensional vector."
  (or (integerp dimensions-designator)
      (and (consp dimensions-designator)
           (null (rest dimensions-designator)))))

(defun %vm-array-object (value)
  "Return the host array object for VALUE, materializing VM COW vectors."
  (%vm-cow-vector-materialize value))

(defun vm-array-rank-value (array) "VM-aware ARRAY-RANK supporting specialized one-dimensional arrays." (let ((object (%vm-array-object array))) (if (vm-specialized-array-p object) 1 (array-rank object))))

(defun vm-array-dimensions-value (array) "VM-aware ARRAY-DIMENSIONS supporting specialized one-dimensional arrays." (let ((object (%vm-array-object array))) (if (vm-specialized-array-p object) (list (vm-specialized-array-length object)) (array-dimensions object))))

(defun vm-array-dimension-value (array axis) "VM-aware ARRAY-DIMENSION supporting specialized one-dimensional arrays." (let ((object (%vm-array-object array))) (if (vm-specialized-array-p object) (if (zerop axis) (vm-specialized-array-length object) (error "Invalid specialized array dimension axis: ~D" axis)) (array-dimension object axis))))

(defun vm-array-total-size-value (array) "VM-aware ARRAY-TOTAL-SIZE supporting specialized one-dimensional arrays." (let ((object (%vm-array-object array))) (if (vm-specialized-array-p object) (vm-specialized-array-length object) (array-total-size object))))

(defun vm-array-element-type-value (array) "Return the element type for host or VM specialized ARRAY." (let ((object (%vm-array-object array))) (if (vm-specialized-array-p object) (case (vm-specialized-array-element-type object) (:fixnum 'fixnum) (:double-float 'double-float) (:character 'character) (:bit 'bit) (:any t) (otherwise (vm-specialized-array-element-type object))) (array-element-type object))))

(defun vm-adjustable-array-p-value (array) "VM-aware ADJUSTABLE-ARRAY-P for host, specialized, and COW arrays." (cond ((vm-cow-vector-p array) (vm-cow-vector-adjustable-p array)) ((vm-specialized-array-p array) nil) (t (adjustable-array-p array))))

(defun vm-array-has-fill-pointer-p-value (array) "VM-aware ARRAY-HAS-FILL-POINTER-P for COW arrays." (let ((object (%vm-array-object array))) (and (not (vm-specialized-array-p object)) (array-has-fill-pointer-p object))))

(defun vm-array-in-bounds-p-value (array subscripts)
  "Return true when SUBSCRIPTS are within ARRAY bounds."
  (let ((dimensions (vm-array-dimensions-value array))
        (subs (if (listp subscripts) subscripts (list subscripts))))
    (and (= (length dimensions) (length subs))
         (every (lambda (dimension subscript)
                  (and (integerp subscript)
                       (<= 0 subscript)
                       (< subscript dimension)))
                dimensions subs))))
