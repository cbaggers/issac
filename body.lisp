(in-package :issac)

;;------------------------------------------------------------

(defun validate-body-kind (body-kind)
  (assert (member body-kind '(:kinematic :dynamic :deformable)) (body-kind))
  body-kind)

(defun make-body (world geometry
                  &key (kind :dynamic)
                    (mass 1s0)
                    (linear-damping 0.1)
                    (matrix4 (m4:identity)))
  (let ((kind (validate-body-kind kind))
        (wptr (%world-ptr world)))
    (with-foreign-array (m4 matrix4 '(:array :float 16))
      (let ((body
             (%make-body
              :ptr (ecase kind
                     (:kinematic (newtoncreatekinematicbody
                                  wptr (%geometry-ptr geometry) m4))
                     (:dynamic (newtoncreatedynamicbody
                                wptr (%geometry-ptr geometry) m4))
                     (:deformable (newtoncreatedeformablebody
                                   wptr (%mesh-ptr geometry) m4))))))
        (body-geometry-mass-set body geometry mass)
        (setf (body-linear-damping body) linear-damping)))))

;;------------------------------------------------------------

(defun free-body (body)
  (newtondestroybody (%body-ptr body)))

;;------------------------------------------------------------

(defun body-id (body)
  (newtonbodygetid (%body-ptr body)))

(defun body-world (body)
  (newtonbodygetworld (%body-ptr body)))

(defun %body-user-data (body)
  (newtonbodygetuserdata (%body-ptr body)))

;;------------------------------------------------------------

(defun body-group-id (body)
  (newtonbodygetmaterialgroupid (%body-ptr body)))

(defun (setf body-group-id) (id body)
  (assert (typep id '(unsigned-byte 8)))
  (newtonbodysetmaterialgroupid (%body-ptr body) id)
  id)

;;------------------------------------------------------------

(defun body-position (body)
  (with-foreign-object (v3 :float 3)
    (newtonbodygetposition (%body-ptr body) v3)
    (ptr->v3 v3)))

;;------------------------------------------------------------

(defun body-add-force (body force-vec3)
  (with-foreign-array (v3 force-vec3 '(:array :float 3))
    (newtonbodyaddforce (%body-ptr body) v3)))

(defun body-add-force (body delta-vec3 &optional (position-vec3 (v! 0 0 0)))
  (with-foreign-array (d3 delta-vec3 '(:array :float 3))
    (with-foreign-array (p3 position-vec3 '(:array :float 3))
      (newtonbodyaddimpulse (%body-ptr body) d3 p3))))

(defun body-add-torque (body torque-vec3)
  (with-foreign-array (v3 torque-vec3 '(:array :float 3))
    (newtonbodyaddtorque (%body-ptr body) v3)))

;; newtonbodyapplyimpulsearray
;; newtonbodyapplyimpulsepair

;;------------------------------------------------------------

(defun body-angular-damping (body)
  (with-foreign-object (v3 :float 3)
    (newtonbodygetangulardamping (%body-ptr body) v3)
    (ptr->v3 v3)))

(defun (setf body-angular-damping) (value body)
  (with-foreign-array (v3 value '(:array :float 3))
    (newtonbodysetangulardamping (%body-ptr body) v3))
  value)

;;------------------------------------------------------------

(defun body-auto-sleep-p (body)
  (>= (newtonbodygetautosleep (%body-ptr body)) 0))

(defun (setf body-auto-sleep-p) (value body)
  (newtonbodysetautosleep (%body-ptr body) (if value 1 0)))

;;------------------------------------------------------------

(defun body-centre-of-mass (body)
  (with-foreign-object (v3 :float 3)
    (newtonbodygetcentreofmass (%body-ptr body) v3)
    (ptr->v3 v3)))

(defun (setf body-centre-of-mass) (value body)
  (with-foreign-array (v3 value '(:array :float 3))
    (newtonbodysetcentreofmass (%body-ptr body) v3))
  value)

;;------------------------------------------------------------

(defun body-collidable-p (body)
  (>= (newtonbodygetcollidable (%body-ptr body)) 0))

(defun (setf body-collidable-p) (value body)
  (newtonbodysetcollidable (%body-ptr body) (if value 1 0)))

;;------------------------------------------------------------

;; newtonbodygetcollision
;; newtonbodysetcollision

;;------------------------------------------------------------

;; This feature is currently disabled: (By newton
;; newtonbodysetcontinuouscollisionmode
;; newtonbodygetcontinuouscollisionmode

;;------------------------------------------------------------

(defun body-recursive-collision-p (body)
  (>= (newtonbodygetjointrecursivecollision (%body-ptr body)) 0))

(defun (setf body-recursive-collision-p) (value body)
  (newtonbodysetjointrecursivecollision (%body-ptr body) (if value 1 0)))

;;------------------------------------------------------------

(defun body-linear-damping (body)
  (newtonbodygetlineardamping (%body-ptr body)))

(defun (setf body-linear-damping) (value body)
  (newtonbodysetlineardamping (%body-ptr body) (float value)))

;;------------------------------------------------------------

(defun body-matrix4 (body)
  (with-foreign-object (m4 :float 16)
    (newtonbodygetmatrix (%body-ptr body) m4)
    (ptr->m4 m4)))

(defun (setf body-matrix4) (mat4 body)
  (with-foreign-array (m4 mat4 '(:array :float 16))
    (newtonbodysetmatrix (%body-ptr body) m4)
    mat4))

;;------------------------------------------------------------

(defun body-angular-velocity (body)
  "Get the global angular velocity (omega) of the body"
  (with-foreign-object (v3 :float 3)
    (newtonbodygetomega (%body-ptr body) v3)
    (ptr->v3 v3)))

(defun (setf body-angular-velocity) (value body)
  "Get the global angular velocity (omega) of the body"
  (with-foreign-array (v3 value '(:array :float 3))
    (newtonbodysetomega (%body-ptr body) v3))
  value)

(defun body-omega (body)
  "The global angular velocity of the body"
  (body-angular-velocity body))

(defun (setf body-omega) (value body)
  "The global angular velocity of the body"
  (setf (body-angular-velocity body) value))

;;------------------------------------------------------------

(defun body-torque (body)
  "Get the global angular velocity (omega) of the body"
  (with-foreign-object (v3 :float 3)
    (newtonbodygettorque (%body-ptr body) v3)
    (ptr->v3 v3)))

(defun (setf body-torque) (value body)
  "Get the global angular velocity (omega) of the body"
  (with-foreign-array (v3 value '(:array :float 3))
    (newtonbodysettorque (%body-ptr body) v3))
  value)

;;------------------------------------------------------------

(defun body-mass (body)
  "Returns the mass and a vec3 containing the the moment of inertia of the object
   in each axis"
  (with-foreign-objects ((mass :float) (ixx :float) (iyy :float) (izz :float))
    (newtonbodygetmass (%body-ptr body) mass ixx iyy izz)
    (values mass (v! (mem-aref ixx :float)
                     (mem-aref iyy :float)
                     (mem-aref izz :float)))))

(defun body-mass-set (body mass ixx iyy izz)
  (newtonbodysetmassmatrix
   (%body-ptr body) (float mass) (float ixx) (float iyy) (float izz))
  body)

(defun body-mass-matrix-set (body mass interia-matrix)
  (with-foreign-array (m4 interia-matrix '(:array :float 16))
    (newtonbodysetfullmassmatrix (%body-ptr body) (float mass) m4))
  body)

(defun body-inverse-mass (body)
  "Returns the mass and a vec3 containing the the moment of inertia of the object
   in each axis"
  (with-foreign-objects ((mass :float) (ixx :float) (iyy :float) (izz :float))
    (newtonbodygetinvmass (%body-ptr body) mass ixx iyy izz)
    (values mass (v! (mem-aref ixx :float)
                     (mem-aref iyy :float)
                     (mem-aref izz :float)))))

(defun body-geometry-mass-set (body geometry mass)
  (newtonbodysetmassproperties
   (%body-ptr body) (float mass) (%geometry-ptr geometry)))

;;------------------------------------------------------------

(defun body-inertia-matrix4 (body)
  (with-foreign-object (m4 :float 16)
    (newtonbodygetinertiamatrix (%body-ptr body) m4)
    (ptr->m4 m4)))

(defun body-inverse-intertia-matrix4 (body)
  (with-foreign-object (m4 :float 16)
    (newtonbodygetinvinertiamatrix (%body-ptr body) m4)
    (ptr->m4 m4)))

;;------------------------------------------------------------

(defun body-velocity (body)
  "Get the global angular velocity (omega) of the body"
  (with-foreign-object (v3 :float 3)
    (newtonbodygetvelocity (%body-ptr body) v3)
    (ptr->v3 v3)))

(defun (setf body-velocity) (value body)
  "Get the global angular velocity (omega) of the body"
  (with-foreign-array (v3 value '(:array :float 3))
    (newtonbodysetvelocity (%body-ptr body) v3))
  value)

(defun body-point-velocity (body point)
  (with-foreign-array (p3 point '(:array :float 3))
    (with-foreign-object (r3 :float 3)
      (newtonbodygetpointvelocity (%body-ptr body) p3 r3)
      (ptr->v3 r3))))

(defun body-integrate-velocity (body timestep)
  (newtonbodyintegratevelocity (%body-ptr body) (float timestep)))

;;------------------------------------------------------------

(defun body-force (body)
  (with-foreign-object (v3 :float 3)
    (newtonbodygetforce  (%body-ptr body) v3)
    (ptr->v3 v3)))

(defun (setf body-force) (value body)
  (with-foreign-array (v3 value '(:array :float 3))
    (NewtonBodySetForce (%body-ptr body) v3))
  value)

(defun body-force-acc (body)
  "Get the force applied on the last call to apply force and torque
   callback. this function can be useful to modify force from joint
   callback"
  (with-foreign-object (v3 :float 3)
    (newtonbodygetforceacc (%body-ptr body) v3)
    (ptr->v3 v3)))

(defun body-torque-acc (body)
  (with-foreign-object (v3 :float 3)
    (newtonbodygettorqueacc (%body-ptr body) v3)
    (ptr->v3 v3)))

(defun body-calculate-inverse-dynamics-force (body timestep desired-velocity)
  (with-foreign-array (dv3 desired-velocity '(:array :float 3))
    (with-foreign-object (r3 :float 3)
      (newtonbodycalculateinversedynamicsforce
       (%body-ptr body) (float timestep) dv3 r3)
      (ptr->v3 r3))))

(defun %set-force-torque-callback (body callback)
  (newtonbodysetforceandtorquecallback (%body-ptr body) callback))

;;------------------------------------------------------------

(defun body-sleeping-p (body)
  (>= (newtonbodygetsleepstate (%body-ptr body)) 0))

(defun (setf body-sleeping-p) (value body)
  (newtonbodysetsleepstate (%body-ptr body) (if value 1 0)))

;;------------------------------------------------------------

(defun body-rotation (body)
  (with-foreign-object (v3 :float 3)
    (newtonbodygetrotation (%body-ptr body) v3)
    (ptr->v3 v3)))

(defun body-max-rotation-per-step (body)
  (newtonbodygetmaxrotationperstep (%body-ptr body)))

(defun (setf body-max-rotation-per-step) (value body)
  (newtonbodysetmaxrotationperstep (%body-ptr body) (float value)))

;;------------------------------------------------------------

;; newtonbodygetaabb

;;------------------------------------------------------------




;; newtonbodygetsimulationstate
;; newtonbodygetskeleton

;; newtonbodysetvelocitynosleep
;; newtonbodysetmatrixnosleep
;; newtonbodysetomeganosleep
;; newtonbodysetmatrixrecursive
;; newtonbodysetsimulationstate
;; newtonbodysetcollisionscale

;; newtonbodydestructor

;; newtonbodygetfirstjoint
;; newtonbodygetnextjoint
;; newtonbodygetnextcontactjoint

;; newtonbodyiterator
