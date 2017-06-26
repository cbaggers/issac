(in-package :issac)

;;------------------------------------------------------------

(defun free-joint (world joint)
  (NewtonDestroyJoint (%world-ptr world) (%joint-ptr joint)))

;;------------------------------------------------------------

(defun %joint-user-data (geometry)
  (NewtonJointGetUserData (%geometry-ptr geometry)))

(defun (setf %joint-user-data) (ptr geometry)
  (NewtonJointSetUserData (%geometry-ptr geometry) ptr))

;;------------------------------------------------------------

(defun joint-linked-collide-p (joint)
  "Get the collision state of the two bodies linked by the
   joint. usually when two bodies are linked by a joint, the application
   wants collision between this two bodies to be disabled. This is the
   default behavior of joints when they are created, however when this
   behavior is not desired the application can change it by setting
   collision on. If the application decides to enable collision between
   jointed bodies, the application should make sure the collision
   geometry do not collide in the work space of the joint."
  (>= (NewtonJointGetCollisionState (%joint-ptr joint)) 0))

(defun (setf joint-linked-collide-p) (value joint)
  (NewtonJointSetCollisionState (%joint-ptr joint) (if value 1 0)))

;;------------------------------------------------------------

(defun joint-stiffness (joint)
  (NewtonJointGetStiffness (%joint-ptr joint)))

(defun (setf joint-stiffness) (value joint)
  (NewtonJointSetStiffness (%joint-ptr joint) (float value)))

;;------------------------------------------------------------

(defun joint-active-p (joint)
  (>= (newtonjointisactive (%joint-ptr joint)) 0))

;;------------------------------------------------------------

;; (CFFI:DEFCSTRUCT (NEWTONJOINTRECORD :SIZE 584)
;;   (M-ATTACHMENMATRIX-0 (:ARRAY (:ARRAY :FLOAT 4) 4) :OFFSET 0)
;;   (M-ATTACHMENMATRIX-1 (:ARRAY (:ARRAY :FLOAT 4) 4) :OFFSET 64)
;;   (M-MINLINEARDOF (:ARRAY :FLOAT 3) :OFFSET 128)
;;   (M-MAXLINEARDOF (:ARRAY :FLOAT 3) :OFFSET 140)
;;   (M-MINANGULARDOF (:ARRAY :FLOAT 3) :OFFSET 152)
;;   (M-MAXANGULARDOF (:ARRAY :FLOAT 3) :OFFSET 164)
;;   (M-ATTACHBODY-0 (:POINTER NEWTONBODY) :OFFSET 176)
;;   (M-ATTACHBODY-1 (:POINTER NEWTONBODY) :OFFSET 184)
;;   (M-EXTRAPARAMETERS (:ARRAY :FLOAT 64) :OFFSET 192)
;;   (M-BODIESCOLLISIONON :INT :OFFSET 448)
;;   (M-DESCRIPTIONTYPE (:ARRAY :CHAR 128) :OFFSET 452))

;; NewtonJointGetInfo

;;------------------------------------------------------------

;; NewtonJointGetBody0
;; NewtonJointGetBody1

;;------------------------------------------------------------

;; NewtonJointSetDestructor
