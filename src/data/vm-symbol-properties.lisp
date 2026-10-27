(in-package #:cl-cc/vm/data)

(defconstant +vm-symbol-plist-linear-limit+ 4
  "Maximum plist entry count kept on the short linear path before hash promotion.")

(defstruct (vm-symbol-property-hash-store
             (:constructor make-vm-symbol-property-hash-store (table)))
  "Internal promoted property store for symbols with many plist entries."
  (table (make-hash-table :test #'eq) :read-only t))

(defun %vm-symbol-property-entry-p (entry)
  (typep entry 'vm-symbol-property-hash-store))

(defun %vm-plist-entry-count (plist)
  (loop for tail on plist by #'cddr
        while tail
        count 1))

(defun %vm-small-plist-get (plist indicator default)
  (loop for tail on plist by #'cddr
        while tail
        when (eq (car tail) indicator)
          do (return (cadr tail))
        finally (return default)))

(defun %vm-small-plist-set (plist indicator value)
  (loop for tail on plist by #'cddr
        while tail
        when (eq (car tail) indicator)
          do (setf (cadr tail) value)
             (return plist)
        finally (return (list* indicator value plist))))

(defun %vm-small-plist-remprop (plist indicator)
  (cond
    ((null plist) (values nil nil))
    ((eq (car plist) indicator) (values (cddr plist) t))
    (t
     (multiple-value-bind (tail removed-p)
         (%vm-small-plist-remprop (cddr plist) indicator)
       (values (list* (car plist) (cadr plist) tail) removed-p)))))

(defun %vm-symbol-property-hash-table->plist (table)
  (let ((plist nil))
    (maphash (lambda (indicator value)
               (setf plist (list* indicator value plist)))
             table)
    plist))

(defun %vm-symbol-property-entry->plist (entry)
  (cond
    ((null entry) nil)
    ((%vm-symbol-property-entry-p entry)
     (%vm-symbol-property-hash-table->plist
      (vm-symbol-property-hash-store-table entry)))
    (t (copy-list entry))))

(defun vm-copy-symbol-property-entry (entry)
  (cond
    ((null entry) nil)
    ((%vm-symbol-property-entry-p entry)
     (let ((table (make-hash-table :test #'eq)))
       (maphash (lambda (indicator value)
                  (setf (gethash indicator table) value))
                (vm-symbol-property-hash-store-table entry))
       (make-vm-symbol-property-hash-store table)))
    (t (copy-list entry))))

(defun %vm-promote-symbol-property-plist (plist)
  (let ((table (make-hash-table :test #'eq)))
    (loop for tail on plist by #'cddr
          while tail
          do (setf (gethash (car tail) table) (cadr tail)))
    (make-vm-symbol-property-hash-store table)))

(defun %vm-normalize-symbol-property-entry (plist)
  (let ((plist-copy (copy-list plist)))
    (if (> (%vm-plist-entry-count plist-copy) +vm-symbol-plist-linear-limit+)
        (%vm-promote-symbol-property-plist plist-copy)
        plist-copy)))

(defun %vm-symbol-property-entry-get (entry indicator default)
  (cond
    ((null entry) default)
    ((%vm-symbol-property-entry-p entry)
     (gethash indicator (vm-symbol-property-hash-store-table entry) default))
    (t (%vm-small-plist-get entry indicator default))))

(defun %vm-symbol-property-entry-set (entry indicator value)
  (cond
    ((null entry)
     (list indicator value))
    ((%vm-symbol-property-entry-p entry)
     (setf (gethash indicator (vm-symbol-property-hash-store-table entry)) value)
     entry)
    (t
     (let ((updated (%vm-small-plist-set entry indicator value)))
       (if (> (%vm-plist-entry-count updated) +vm-symbol-plist-linear-limit+)
           (%vm-promote-symbol-property-plist updated)
           updated)))))

(defun %vm-symbol-property-entry-remprop (entry indicator)
  (cond
    ((null entry) (values nil nil))
    ((%vm-symbol-property-entry-p entry)
     (let ((table (vm-symbol-property-hash-store-table entry)))
       (multiple-value-bind (value found-p) (gethash indicator table)
         (declare (ignore value))
         (when found-p
           (remhash indicator table))
         (values (if (zerop (hash-table-count table)) nil entry)
                 found-p))))
    (t (%vm-small-plist-remprop entry indicator))))

(defun %vm-touch-symbol-property-barrier (state)
  (incf (vm-state-symbol-plist-read-barrier state)))

(defun %vm-sync-host-symbol-plist (sym entry)
  (setf (symbol-plist sym) (%vm-symbol-property-entry->plist entry)))

(defun %vm-symbol-property-get (state table sym indicator default)
  (cl-cc/runtime:rt-with-lock ((vm-state-symbol-plist-lock state))
    (%vm-symbol-property-entry-get (gethash sym table) indicator default)))

(defun %vm-symbol-property-set (state table sym indicator value &key (sync-host-p nil))
  (cl-cc/runtime:rt-with-lock ((vm-state-symbol-plist-lock state))
    (let* ((entry (gethash sym table))
           (updated (%vm-symbol-property-entry-set entry indicator value)))
      (setf (gethash sym table) updated)
      (%vm-touch-symbol-property-barrier state)
      (when sync-host-p
        (%vm-sync-host-symbol-plist sym updated))
      value)))

(defun %vm-symbol-property-remprop (state table sym indicator &key (sync-host-p nil))
  (cl-cc/runtime:rt-with-lock ((vm-state-symbol-plist-lock state))
    (multiple-value-bind (updated found-p)
        (%vm-symbol-property-entry-remprop (gethash sym table) indicator)
      (if updated
          (setf (gethash sym table) updated)
          (remhash sym table))
      (when found-p
        (%vm-touch-symbol-property-barrier state))
      (when sync-host-p
        (%vm-sync-host-symbol-plist sym updated))
      found-p)))

(defun %vm-symbol-property-replace-plist (state table sym plist &key (sync-host-p nil))
  (cl-cc/runtime:rt-with-lock ((vm-state-symbol-plist-lock state))
    (let ((normalized (and plist (%vm-normalize-symbol-property-entry plist))))
      (if normalized
          (setf (gethash sym table) normalized)
          (remhash sym table))
      (%vm-touch-symbol-property-barrier state)
      (when sync-host-p
        (%vm-sync-host-symbol-plist sym normalized))
      (%vm-symbol-property-entry->plist normalized))))

(defun vm-symbol-plist-read-snapshot (state sym)
  "Return a copy of SYM's user-visible plist and the current read barrier."
  (cl-cc/runtime:rt-with-lock ((vm-state-symbol-plist-lock state))
    (values (%vm-symbol-property-entry->plist
             (gethash sym (vm-state-symbol-plists state)))
            (vm-state-symbol-plist-read-barrier state))))

(define-vm-instruction vm-symbol-get (vm-instruction)
  "Get property INDICATOR from symbol SYM's plist. Returns default if absent."
  (dst nil :reader vm-dst) (sym nil :reader vm-sym)
  (indicator nil :reader vm-indicator) (default nil :reader vm-default)
  (:sexp-tag :symbol-get) (:sexp-slots dst sym indicator default))

(define-vm-instruction vm-remprop (vm-instruction)
  "Remove property INDICATOR from symbol SYM's plist."
  (dst nil :reader vm-dst) (sym nil :reader vm-sym)
  (indicator nil :reader vm-indicator)
  (:sexp-tag :remprop) (:sexp-slots dst sym indicator))

(define-vm-instruction vm-symbol-plist (vm-instruction)
  "Return entire property list of SYM."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :symbol-plist) (:sexp-slots dst src))

(define-vm-instruction vm-set-symbol-plist (vm-instruction)
  "Set the entire property list of SYM to PLIST."
  (dst nil :reader vm-dst) (sym nil :reader vm-sym) (plist-reg nil :reader vm-plist-reg)
  (:sexp-tag :set-symbol-plist) (:sexp-slots dst sym plist-reg))

(in-package #:cl-cc/vm/data)

(define-vm-instruction vm-symbol-set (vm-instruction)
  "Set property INDICATOR on symbol SYM's plist to VALUE."
  (dst nil :reader vm-dst) (sym nil :reader vm-sym)
  (indicator nil :reader vm-indicator) (value nil :reader vm-value)
  (:sexp-tag :symbol-set) (:sexp-slots dst sym indicator value))
