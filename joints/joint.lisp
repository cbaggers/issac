(in-package :issac)

;;------------------------------------------------------------

(defn free-joint ((world world) (joint joint)) null
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (NewtonDestroyJoint (%world-ptr world) (%joint-ptr joint))
  nil)

;;------------------------------------------------------------

(defn %joint-user-data ((joint joint)) foreign-pointer
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (NewtonJointGetUserData (%joint-ptr joint)))

(defn (setf %joint-user-data) ((ptr foreign-pointer)
                               (joint joint))
    foreign-pointer
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (NewtonJointSetUserData (%joint-ptr joint) ptr)
  ptr)

;;------------------------------------------------------------

(defn joint-linked-collide-p ((joint joint)) boolean
  "Get the collision state of the two bodies linked by the
   joint. usually when two bodies are linked by a joint, the application
   wants collision between this two bodies to be disabled. This is the
   default behavior of joints when they are created, however when this
   behavior is not desired the application can change it by setting
   collision on. If the application decides to enable collision between
   jointed bodies, the application should make sure the collision
   geometry do not collide in the work space of the joint."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (>= (NewtonJointGetCollisionState (%joint-ptr joint)) 0))

(defn (setf joint-linked-collide-p) ((value t)
                                     (joint joint))
    t
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (NewtonJointSetCollisionState (%joint-ptr joint) (if value 1 0))
  value)

;;------------------------------------------------------------

(defn joint-stiffness ((joint joint)) single-float
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (NewtonJointGetStiffness (%joint-ptr joint)))

(defn (setf joint-stiffness) ((value single-float) (joint joint))
    single-float
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (NewtonJointSetStiffness (%joint-ptr joint) (float value)))

;;------------------------------------------------------------

(defn joint-active-p ((joint joint)) boolean
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (>= (newtonjointisactive (%joint-ptr joint)) 0))

;;------------------------------------------------------------

(defn joint-info ((joint joint)) joint-info
  (declare (optimize (speed 3) (safety 1) (debug 1)))
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

(defn joint-body-0 ((joint joint)) body
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (%body-ptr->body (NewtonJointGetBody0 (%joint-ptr joint))))

(defn joint-body-1 ((joint joint)) body
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (%body-ptr->body (NewtonJointGetBody1 (%joint-ptr joint))))

;;------------------------------------------------------------

(defn joint-destructor-callback ((joint joint))
    (or null (function (joint) t))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (%joint-destructor-callback joint))

(defn (setf joint-destructor-callback)
    ((callback (or null (function (joint) t)))
     (joint joint))
    (or null (function (joint) t))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((cb (if callback
                (get-callback '%joint-destructor-cb)
                (null-pointer))))
    (newtonjointsetdestructor (%joint-ptr joint) cb))
  (setf (%joint-destructor-callback joint) callback))
