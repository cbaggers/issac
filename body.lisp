(in-package :issac)

;;------------------------------------------------------------

;;
;; BODY

(deftclass (body (:constructor %make-body)
                  (:conc-name %body-))
  (ptr (error "") :type foreign-pointer))

;;------------------------------------------------------------

(defun validate-body-kind (body-kind)
  (assert (member body-kind '(:kinematic :dynamic :deformable)) (body-kind))
  body-kind)

(defun make-body (world collision &key (kind :dynamic) (matrix4 (m4:identity)))
  (let ((kind (validate-body-kind kind))
        (wptr (%world-ptr world)))
    (with-foreign-array (m4 matrix4 '(:array :float 16))
      (%make-body
       :ptr (ecase kind
              (:kinematic (newtoncreatekinematicbody
                           wptr (%geometry-ptr collision) m4))
              (:dynamic (newtoncreatedynamicbody
                         wptr (%geometry-ptr collision) m4))
              (:deformable (newtoncreatedeformablebody
                            wptr (%mesh-ptr collision) m4)))))))

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

newtonbodygetcollision
newtonbodysetcollision

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

(defun body-set-mass-matrix (body mass ixx iyy izz)
  (newtonbodysetmassmatrix
   (%body-ptr body) (float mass) (float ixx) (float iyy) (float izz)))

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

;;------------------------------------------------------------

(defun body-sleeping-p (body)
  (>= (newtonbodygetsleepstate (%body-ptr body)) 0))

(defun (setf body-sleeping-p) (value body)
  (newtonbodysetsleepstate (%body-ptr body) (if value 1 0)))

;;------------------------------------------------------------

(defun body-max-rotation-per-step (body)
  (newtonbodygetmaxrotationperstep (%body-ptr body)))

(defun (setf body-max-rotation-per-step) (value body)
  (newtonbodysetmaxrotationperstep (%body-ptr body) (float value)))

;;------------------------------------------------------------
;;------------------------------------------------------------

;; newtonbodygetaabb

;;------------------------------------------------------------

;; newtonbodygetmass
;; newtonbodygetinvmass

;; newtonbodysetmassproperties
;; newtonbodysetfullmassmatrix

;;------------------------------------------------------------

;; newtonbodygetforce
;; newtonbodygetforceacc
;; newtonbodycalculateinversedynamicsforce

;;------------------------------------------------------------

;; newtonbodygetposition
;; newtonbodygetrotation
;; newtonbodygetpointvelocity



;; newtonbodygetinertiamatrix
;; newtonbodygetinvinertiamatrix



;; newtonbodygetsimulationstate
;; newtonbodygetskeleton
;; newtonbodygettorqueacc



;; newtonbodysetvelocitynosleep
;; newtonbodysetmatrixnosleep
;; newtonbodysetomeganosleep
;; newtonbodysetmatrixrecursive
;; newtonbodysetsimulationstate
;; newtonbodysetcollisionscale


;; newtonbodyapplyimpulsearray
;; newtonbodyapplyimpulsepair

;; newtonbodydestructor



;; newtonbodygetfirstjoint
;; newtonbodygetnextjoint
;; newtonbodygetnextcontactjoint




;; newtonbodyintegratevelocity
;; newtonbodyiterator
