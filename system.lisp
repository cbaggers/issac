(in-package :issac)

(deftype index () '(integer 0 #.most-positive-fixnum))
(deftype issac-id () 'index)

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

(declaim (type (array world (*)) *worlds*))
(defvar *worlds*
  (make-array 0 :element-type 'world :fill-pointer 0 :adjustable t
              :initial-element *null-world*))

(declaim (type (array body (*)) *bodies*))
(defvar *bodies*
  (make-array 0 :element-type 'body :fill-pointer 0 :adjustable t))

(declaim (type (array geometry (*)) *geometry*))
(defvar *geometry*
  (make-array 0 :element-type 'geometry :fill-pointer 0 :adjustable t))

(declaim (type (array material-pair (*)) *material-pairs*))
(defvar *material-pairs*
  (make-array 0 :element-type 'geometry :fill-pointer 0 :adjustable t))

(declaim (type (array joint (*)) *joints*))
(defvar *joints*
  (make-array 0 :element-type 'joint :fill-pointer 0 :adjustable t))

;;------------------------------------------------------------

(defn-inline %add-body-to-system ((body body)) issac-id
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (vector-push-extend body *bodies*))

(defn-inline %add-geom-to-system ((geom geometry)) issac-id
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (vector-push-extend geom *geometry*))

(defn-inline %add-material-pair-to-system ((material-pair material-pair))
    issac-id
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (vector-push-extend material-pair *material-pairs*))

(defn %add-joint-to-system ((joint joint)) issac-id
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (vector-push-extend joint *joints*))

;;------------------------------------------------------------

(defn %body-id-to-body ((body-id issac-id)) body
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (aref *bodies* body-id))

(defn %geom-id-to-geom ((geom-id issac-id)) geometry
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (aref *geometry* geom-id))

(defn %material-pair-id-to-material-pair ((material-pair-id issac-id))
    material-pair
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (aref *material-pairs* material-pair-id))

(defn %joint-id-to-joint ((joint-id issac-id)) joint
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (aref *joints* joint-id))

;;------------------------------------------------------------

(defn-inline %body-ptr->body ((ptr foreign-pointer)) body
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (%body-id-to-body
   (pointer-address
    (the foreign-pointer (newtonbodygetuserdata ptr)))))

(defn-inline %geom-ptr->geom ((ptr foreign-pointer)) geometry
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (%geom-id-to-geom (pointer-address (newtoncollisiongetuserdata ptr))))

(defn-inline %material-pair-ptr->material-pair ((ptr foreign-pointer))
    material-pair
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (%material-pair-id-to-material-pair
   (pointer-address
    (NewtonMaterialGetMaterialPairUserData ptr))))

(defn-inline %joint-ptr->joint ((ptr foreign-pointer)) joint
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (%joint-id-to-joint (pointer-address (newtonjointgetuserdata ptr))))

;;------------------------------------------------------------
