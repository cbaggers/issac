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

;;------------------------------------------------------------

(declaim (type (array world (*)) *worlds**))
(defvar *worlds*
  (make-array 0 :element-type 'world :fill-pointer 0 :adjustable t
              :initial-element *null-world*))

(declaim (type (array body (*)) *bodies*))
(defvar *bodies*
  (make-array 0 :element-type 'body :fill-pointer 0 :adjustable t))

(declaim (type (array geometry (*)) *geometry*))
(defvar *geometry*
  (make-array 0 :element-type 'geometry :fill-pointer 0 :adjustable t))

;;------------------------------------------------------------

(defun %add-body-to-system (body)
  ;; returns the new index
  (vector-push-extend body *bodies*))

(defun %add-geom-to-system (geom)
  ;; returns the new index
  (vector-push-extend geom *geometry*))

(defun %body-id-to-body (body-id)
  (aref *bodies* body-id))

(defun %geom-id-to-geom (geom-id)
  (aref *geometry* geom-id))

;;------------------------------------------------------------
