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

(defun joint-info (joint)
  (with-foreign-object (ptr 'NewtonJointRecord)
    (NewtonJointGetInfo (%joint-ptr joint) ptr)
    (with-foreign-slots ((m-attachmenmatrix-0
                          m-attachmenmatrix-1
                          m-minlineardof
                          m-maxlineardof
                          m-minangulardof
                          m-maxangulardof
                          m-attachbody-0
                          m-attachbody-1
                          m-extraparameters
                          m-bodiescollisionon
                          m-descriptiontype)
                         ptr NewtonJointRecord)
      (make-joint-info
       :attachment-0-body (%body-ptr->body m-attachbody-0)
       :attachment-1-body (%body-ptr->body m-attachbody-1)
       :attachment-0-mat4 (ptr->m4 m-attachbody-0)
       :attachment-1-mat4 (ptr->m4 m-attachbody-1)
       :min-linear-dof (ptr->v3 m-minlineardof)
       :max-linear-dof (ptr->v3 m-maxlineardof)
       :min-angular-dof (ptr->v3 m-minangulardof)
       :max-angular-dof (ptr->v3 m-maxangulardof)
       :extra-params (foreign-array-to-lisp
                      m-extraparameters '(:array :float 64))
       :bodies-collision-on (= 1 m-bodiescollisionon)
       :description (foreign-string-to-lisp m-descriptiontype :count 128)))))

;;------------------------------------------------------------

(defun joint-body-0 (joint)
  (NewtonJointGetBody0 (%joint-ptr joint)))

(defun joint-body-1 (joint)
  (NewtonJointGetBody1 (%joint-ptr joint)))

;;------------------------------------------------------------

(defun joint-destructor-callback (joint)
  (%joint-destructor-callback joint))

(defun (setf joint-destructor-callback) (callback joint)
  (let ((cb (if callback
                (get-callback '%joint-destructor-cb)
                (null-pointer))))
    (newtonjointsetdestructor (%joint-ptr joint) cb))
  (setf (%joint-destructor-callback joint) callback))
