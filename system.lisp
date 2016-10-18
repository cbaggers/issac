(in-package :issac)

;;------------------------------------------------------------

(defvar *newton-version*
  (list newton-major-version newton-minor-version))

(defun newton-version ()
  (newtonworldgetversion))

;;------------------------------------------------------------

(defun %size-of-newton-float ()
  (newtonworldfloatsize))

(eval-when (:execute)
  (assert (= (%size-of-newton-float) (foreign-type-size :float))))

;;------------------------------------------------------------

(defun %newton-memory-used ()
  (newtongetmemoryused))

;; newtonsetmemorysystem
;; newtonalloc
;; newtonallocmemory
;; newtonfree
;; newtonfreememory
