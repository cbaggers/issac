(in-package :issac)

;;------------------------------------------------------------

(defn validate-body-kind ((body-kind symbol)) symbol
  (assert (member body-kind '(:kinematic :dynamic :deformable)) (body-kind))
  body-kind)

(defn make-body ((world world)
                 (geometry geometry)
                 &key (kind symbol :dynamic)
                 (mass single-float 1f0)
                 (linear-damping single-float 0.1)
                 (matrix4 rtg-math.types:mat4 (m4:identity)))
    body
  (declare (profile t))
  (let ((kind (validate-body-kind kind))
        (wptr (%world-ptr world)))
    (with-foreign-array (m4 matrix4 '(:array :float 16))
      (let* ((body
              (%make-body
               :ptr (ecase kind
                      (:kinematic (newtoncreatekinematicbody
                                   wptr (%geometry-ptr geometry) m4))
                      (:dynamic (newtoncreatedynamicbody
                                 wptr (%geometry-ptr geometry) m4))
                      (:deformable (newtoncreatedeformablebody
                                    wptr (%geometry-ptr geometry) m4))))))
        (setf (%body-user-data body) (make-pointer (%add-body-to-system body)))
        (body-geometry-mass-set body geometry mass)
        (setf (body-linear-damping body) linear-damping)
        body))))

;;------------------------------------------------------------

(defn free-body ((body body)) null
  (declare (profile t))
  (newtondestroybody (%body-ptr body))
  nil)

(defn body-destructor-callback ((body body))
    (or null (function (body) t))
  (declare (profile t))
  (%body-destructor-callback body))

(defn (setf body-destructor-callback)
    ((callback (or null (function (body) t)))
     (body body))
    (or null (function (body) t))
  (declare (profile t))
  (let ((cb (if callback
                (get-callback '%body-destructor)
                (null-pointer))))
    (newtonbodysetdestructorcallback (%body-ptr body) cb))
  (setf (%body-destructor-callback body) callback))

;;------------------------------------------------------------

(defn body-id ((body body)) (signed-byte 32)
  (newtonbodygetid (%body-ptr body)))

(defn body-world ((body body)) world
  (declare (profile t))
  (%world-from-world-ptr (newtonbodygetworld (%body-ptr body))))

(defn %body-user-data ((body body)) cffi:foreign-pointer
  (declare (optimize (speed 3) (safety 1)))
  (newtonbodygetuserdata (%body-ptr body)))

(defn (setf %body-user-data) ((ptr cffi:foreign-pointer)
                              (body body))
    cffi:foreign-pointer
  (declare (optimize (speed 3) (safety 1)))
  (newtonbodysetuserdata (%body-ptr body) ptr))

;;------------------------------------------------------------

(defn body-material ((body body)) (signed-byte 32)
  (newtonbodygetmaterialgroupid (%body-ptr body)))

(defn (setf body-material) ((id (signed-byte 32))
                            (body body))
    (signed-byte 32)
  (assert (typep id '(unsigned-byte 8)))
  (newtonbodysetmaterialgroupid (%body-ptr body) id)
  id)

;;------------------------------------------------------------

(defn body-position ((body body)) (simple-array single-float (3))
  (with-foreign-object (v3 :float 3)
    (newtonbodygetposition (%body-ptr body) v3)
    (ptr->v3 v3)))

;;------------------------------------------------------------

(defn body-add-force ((body body)
                      (force-vec3 (simple-array single-float (3))))
    null
  (with-foreign-array (v3 force-vec3 '(:array :float 3))
    (newtonbodyaddforce (%body-ptr body) v3))
  nil)

(defn body-add-impluse
    ((body body)
     (delta-vec3 (simple-array single-float (3)))
     &optional
     (position-vec3 (simple-array single-float (3)) (v! 0 0 0)))
    null
  (with-foreign-array (d3 delta-vec3 '(:array :float 3))
    (with-foreign-array (p3 position-vec3 '(:array :float 3))
      (newtonbodyaddimpulse (%body-ptr body) d3 p3)))
  nil)

(defn body-add-torque ((body body)
                       (torque-vec3 (simple-array single-float (3))))
    null
  (with-foreign-array (v3 torque-vec3 '(:array :float 3))
    (newtonbodyaddtorque (%body-ptr body) v3))
  nil)

(defn body-apply-impulse-pair ((body body)
                               (linear-impulse3 (simple-array single-float (3)))
                               (angular-impulse3 (simple-array single-float (3))))
    null
  (with-foreign-array (l3 linear-impulse3 '(:array :float 3))
    (with-foreign-array (a3 angular-impulse3 '(:array :float 3))
      (newtonbodyapplyimpulsepair (%body-ptr body) l3 a3)))
  nil)

;;newtonbodyapplyimpulsearray

;;------------------------------------------------------------

(defn body-angular-damping ((body body)) (simple-array single-float (3))
  (with-foreign-object (v3 :float 3)
    (newtonbodygetangulardamping (%body-ptr body) v3)
    (ptr->v3 v3)))

(defn (setf body-angular-damping) ((value (simple-array single-float (3)))
                                   (body body))
    (simple-array single-float (3))
  (with-foreign-array (v3 value '(:array :float 3))
    (newtonbodysetangulardamping (%body-ptr body) v3))
  value)

;;------------------------------------------------------------

(defn body-auto-sleep-p ((body body)) boolean
  (>= (newtonbodygetautosleep (%body-ptr body)) 0))

(defn (setf body-auto-sleep-p) ((value t) (body body)) t
  (newtonbodysetautosleep (%body-ptr body) (if value 1 0))
  value)

;;------------------------------------------------------------

(defn body-centre-of-mass ((body body)) (simple-array single-float (3))
  (with-foreign-object (v3 :float 3)
    (newtonbodygetcentreofmass (%body-ptr body) v3)
    (ptr->v3 v3)))

(defn (setf body-centre-of-mass) ((value (simple-array single-float (3)))
                                  (body body))
    (simple-array single-float (3))
  (with-foreign-array (v3 value '(:array :float 3))
    (newtonbodysetcentreofmass (%body-ptr body) v3))
  value)

;;------------------------------------------------------------

(defn body-collidable-p ((body body)) boolean
  (>= (newtonbodygetcollidable (%body-ptr body)) 0))

(defn (setf body-collidable-p) ((value t) (body body)) t
  (newtonbodysetcollidable (%body-ptr body) (if value 1 0)))

;;------------------------------------------------------------

(defn body-geometry ((body body)) geometry
  "Get the geometry for the given body"
  (%geom-ptr->geom
   (newtonbodygetcollision
    (%body-ptr body))))

(defn (setf body-geometry) ((geometry geometry)
                            (body body))
    geometry
  "Get the geometry for the given body"
  (newtonbodysetcollision (%body-ptr body) (%geometry-ptr geometry))
  geometry)

;;------------------------------------------------------------

(defn body-continuous-collision-mode ((body body)) (signed-byte 32)
  ;; {TODO} translate the int returned from this
  (newtonbodygetcontinuouscollisionmode body))

(defn (setf body-continuous-collision-mode) ((state (unsigned-byte 32))
                                              (body body))
    (unsigned-byte 32)
  ;; {TODO} translate the int returned from this
  (newtonbodysetcontinuouscollisionmode body state)
  state)

;;------------------------------------------------------------

(defn body-recursive-collision-p ((body body)) boolean
  (>= (newtonbodygetjointrecursivecollision (%body-ptr body)) 0))

(defn (setf body-recursive-collision-p) ((value t) (body body)) t
  (newtonbodysetjointrecursivecollision (%body-ptr body) (if value 1 0))
  value)

;;------------------------------------------------------------

(defn body-linear-damping ((body body)) single-float
  (newtonbodygetlineardamping (%body-ptr body)))

(defn (setf body-linear-damping) ((value single-float)
                                  (body body))
    single-float
  (newtonbodysetlineardamping (%body-ptr body) value)
  value)

;;------------------------------------------------------------

(defn body-matrix4 ((body body)) (simple-array single-float (16))
  (with-foreign-object (m4 :float 16)
    (newtonbodygetmatrix (%body-ptr body) m4)
    (ptr->m4 m4)))

(defn (setf body-matrix4) ((mat4 (simple-array single-float (16)))
                           (body body))
    (simple-array single-float (16))
  (with-foreign-array (m4 mat4 '(:array :float 16))
    (newtonbodysetmatrix (%body-ptr body) m4)
    mat4))

(defn body-transform-callback ((body body)) (or null body-transform-function)
  (%body-transform-callback body))

(defn (setf body-transform-callback)
    ((callback (or null body-transform-function))
     (body body))
    (or null body-transform-function)
  (let ((cb (if callback
                (get-callback '%body-transform)
                (null-pointer))))
    (newtonbodysettransformcallback (%body-ptr body) cb)
    (setf (%body-transform-callback body) callback)))

;;------------------------------------------------------------

(defn body-angular-velocity ((body body)) (simple-array single-float (3))
  "Get the global angular velocity (omega) of the body"
  (with-foreign-object (v3 :float 3)
    (newtonbodygetomega (%body-ptr body) v3)
    (ptr->v3 v3)))

(defn (setf body-angular-velocity) ((value (simple-array single-float (3)))
                                     (body body))
    (simple-array single-float (3))
  "Get the global angular velocity (omega) of the body"
  (with-foreign-array (v3 value '(:array :float 3))
    (newtonbodysetomega (%body-ptr body) v3))
  value)

(defn body-omega ((body body)) (simple-array single-float (3))
  "The global angular velocity of the body"
  (body-angular-velocity body))

(defn (setf body-omega) ((value (simple-array single-float (3)))
                         (body body))
    (simple-array single-float (3))
  "The global angular velocity of the body"
  (setf (body-angular-velocity body) value))

;;------------------------------------------------------------

(defn body-torque ((body body)) (simple-array single-float (3))
  "Get the global angular velocity (omega) of the body"
  (with-foreign-object (v3 :float 3)
    (newtonbodygettorque (%body-ptr body) v3)
    (ptr->v3 v3)))

(defn (setf body-torque) ((value (simple-array single-float (3)))
                          (body body))
    (simple-array single-float (3))
  "Get the global angular velocity (omega) of the body"
  (with-foreign-array (v3 value '(:array :float 3))
    (newtonbodysettorque (%body-ptr body) v3))
  value)

;;------------------------------------------------------------

(defn body-mass ((body body))
    (values single-float (simple-array single-float (3)))
  "Returns the mass and a vec3 containing the the moment of inertia of the object
   in each axis"
  (with-foreign-objects ((mass :float) (ixx :float) (iyy :float) (izz :float))
    (newtonbodygetmass (%body-ptr body) mass ixx iyy izz)
    (values (mem-aref mass :float)
            (v! (mem-aref ixx :float)
                (mem-aref iyy :float)
                (mem-aref izz :float)))))

(defn body-mass-set ((body body)
                     (mass single-float)
                     (ixx single-float)
                     (iyy single-float)
                     (izz single-float))
    body
  (newtonbodysetmassmatrix (%body-ptr body) mass ixx iyy izz)
  body)

(defn body-mass-matrix-set ((body body)
                            (mass single-float)
                            (interia-matrix (simple-array single-float (16))))
    body
  (with-foreign-array (m4 interia-matrix '(:array :float 16))
    (newtonbodysetfullmassmatrix (%body-ptr body) (float mass) m4))
  body)

(defn body-inverse-mass ((body body))
    (values single-float (simple-array single-float (3)))
  "Returns the mass and a vec3 containing the the moment of inertia of the object
   in each axis"
  (with-foreign-objects ((mass :float) (ixx :float) (iyy :float) (izz :float))
    (newtonbodygetinvmass (%body-ptr body) mass ixx iyy izz)
    (values (mem-aref mass :float)
            (v! (mem-aref ixx :float)
                (mem-aref iyy :float)
                (mem-aref izz :float)))))

(defn body-geometry-mass-set ((body body)
                              (geometry geometry)
                              (mass single-float))
    null
  (newtonbodysetmassproperties (%body-ptr body) mass (%geometry-ptr geometry))
  nil)

;;------------------------------------------------------------

(defn body-inertia-matrix4 ((body body)) (simple-array single-float (16))
  (with-foreign-object (m4 :float 16)
    (newtonbodygetinertiamatrix (%body-ptr body) m4)
    (ptr->m4 m4)))

(defn body-inverse-intertia-matrix4 ((body body))
    (simple-array single-float (16))
  (with-foreign-object (m4 :float 16)
    (newtonbodygetinvinertiamatrix (%body-ptr body) m4)
    (ptr->m4 m4)))

;;------------------------------------------------------------

(defn body-velocity ((body body)) (simple-array single-float (3))
  "Get the global angular velocity (omega) of the body"
  (with-foreign-object (v3 :float 3)
    (newtonbodygetvelocity (%body-ptr body) v3)
    (ptr->v3 v3)))

(defn (setf body-velocity) ((value (simple-array single-float (3)))
                            (body body))
    (simple-array single-float (3))
  "Get the global angular velocity (omega) of the body"
  (with-foreign-array (v3 value '(:array :float 3))
    (newtonbodysetvelocity (%body-ptr body) v3))
  value)

(defn body-point-velocity ((body body)
                           (point (simple-array single-float (3))))
    (simple-array single-float (3))
  (with-foreign-array (p3 point '(:array :float 3))
    (with-foreign-object (r3 :float 3)
      (newtonbodygetpointvelocity (%body-ptr body) p3 r3)
      (ptr->v3 r3))))

(defn body-integrate-velocity ((body body)
                               (timestep single-float))
    null
  (newtonbodyintegratevelocity (%body-ptr body) timestep)
  nil)

;;------------------------------------------------------------

(defn body-force ((body body)) (simple-array single-float (3))
  (with-foreign-object (v3 :float 3)
    (newtonbodygetforce  (%body-ptr body) v3)
    (ptr->v3 v3)))

(defn (setf body-force) ((value (simple-array single-float (3)))
                         (body body))
    (simple-array single-float (3))
  (with-foreign-array (v3 value '(:array :float 3))
    (NewtonBodySetForce (%body-ptr body) v3))
  value)

(defn body-force-acc ((body body)) (simple-array single-float (3))
  "Get the force applied on the last call to apply force and torque
   callback. this function can be useful to modify force from joint
   callback"
  (with-foreign-object (v3 :float 3)
    (newtonbodygetforceacc (%body-ptr body) v3)
    (ptr->v3 v3)))

(defn body-torque-acc ((body body)) (simple-array single-float (3))
  (with-foreign-object (v3 :float 3)
    (newtonbodygettorqueacc (%body-ptr body) v3)
    (ptr->v3 v3)))

(defn body-calculate-inverse-dynamics-force
    ((body body)
     (timestep single-float)
     (desired-velocity (simple-array single-float (3))))
    (simple-array single-float (3))
  (with-foreign-array (dv3 desired-velocity '(:array :float 3))
    (with-foreign-object (r3 :float 3)
      (newtonbodycalculateinversedynamicsforce
       (%body-ptr body) (float timestep) dv3 r3)
      (ptr->v3 r3))))

(defn body-force-torque-callback ((body body))
    (or null body-torque-function)
  (%body-force-torque-callback body))

(defn (setf body-force-torque-callback)
    ((callback (or null body-torque-function))
     (body body))
    (or null body-torque-function)
  (let ((cb (if callback
                (get-callback '%body-apply-force-and-torque)
                (null-pointer))))
    (newtonbodysetforceandtorquecallback (%body-ptr body) cb))
  (setf (%body-force-torque-callback body) callback))

;;------------------------------------------------------------

(defn body-sleeping-p ((body body)) boolean
  (>= (newtonbodygetsleepstate (%body-ptr body)) 0))

(defn (setf body-sleeping-p) ((value t) (body body)) t
  (newtonbodysetsleepstate (%body-ptr body) (if value 1 0))
  value)

(defn body-set-velocity-no-sleep ((body body)
                                  (vec3 (simple-array single-float (3))))
    (simple-array single-float (3))
  (with-foreign-array (v3 vec3 '(:array :float 3))
    (newtonbodysetvelocitynosleep (%body-ptr body) v3))
  vec3)

(defn body-set-matrix-no-sleep ((body body)
                                (mat4 (simple-array single-float (16))))
    (simple-array single-float (16))
  (with-foreign-array (m4 mat4 '(:array :float 16))
    (newtonbodysetmatrixnosleep (%body-ptr body) m4))
  mat4)

(defn body-set-omega-no-sleep ((body body)
                               (vec3 (simple-array single-float (3))))
    (simple-array single-float (3))
  (with-foreign-array (v3 vec3 '(:array :float 3))
    (newtonbodysetomeganosleep (%body-ptr body) v3))
  vec3)

;;------------------------------------------------------------

(defn body-rotation ((body body)) (simple-array single-float (3))
  (with-foreign-object (v3 :float 3)
    (newtonbodygetrotation (%body-ptr body) v3)
    (ptr->v3 v3)))

(defn body-max-rotation-per-step ((body body)) single-float
  (newtonbodygetmaxrotationperstep (%body-ptr body)))

(defn (setf body-max-rotation-per-step) ((value single-float)
                                         (body body))
    single-float
  (newtonbodysetmaxrotationperstep (%body-ptr body) value)
  value)

;;------------------------------------------------------------

(defn body-aabb ((body body))
    (values (simple-array single-float (3))
            (simple-array single-float (3)))
  (with-foreign-objects ((p0 :float 3)
                         (p1 :float 3))
    (newtonbodygetaabb (%body-ptr body) p0 p1)
    (values (ptr->v3 p0) (ptr->v3 p1))))

;;------------------------------------------------------------

(defn body-simulation-state ((body body)) (signed-byte 32)
  ;; {TODO} what does state mean
  (newtonbodygetsimulationstate (%body-ptr body)))

(defn (setf body-simulation-state) ((state (signed-byte 32))
                                    (body body))
    (signed-byte 32)
  ;; {TODO} what does state mean
  (newtonbodysetsimulationstate (%body-ptr body) state)
  state)

(defn body-get-skeleton ((body body)) foreign-pointer
  ;; {TODO} how are we meant to handle skeletons?
  (newtonbodygetskeleton (%body-ptr body)))

(defn body-set-matrix-recursive ((body body)
                                 (mat4 (simple-array single-float (16))))
    (simple-array single-float (16))
  (with-foreign-array (m4 mat4 '(:array :float 16))
    (newtonbodysetmatrixrecursive (%body-ptr body) m4))
  mat4)

(defn body-set-collision-scale ((body body)
                                (scale3 (simple-array single-float (3))))
    (simple-array single-float (3))
  (newtonbodysetcollisionscale body (v:x scale3) (v:y scale3) (v:z scale3))
  scale3)

(defn-inline %body-first-joint ((body body)) foreign-pointer
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;; {TODO} need an index of joints
  (newtonbodygetfirstjoint (%body-ptr body)))

(defn-inline %body-next-joint ((body body) (joint foreign-pointer))
    foreign-pointer
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;; {TODO} need an index of joints
  (newtonbodygetnextjoint
   (%body-ptr body)
   joint))

(defmacro do-body-joints ((var-name body-obj) &body body)
  ;; hidden is just so the user can't fuck the process by setting
  ;; var-name to something else
  (with-gensyms (gbody hidden)
    `(let* ((,gbody ,body-obj)
            (,hidden (%body-first-joint ,gbody)))
       (loop :until (null-pointer-p ,hidden) :do
          (let ((,var-name (%body-ptr->body ,hidden)))
            ,@body
            (setf ,hidden (%body-next-joint ,gbody ,hidden)))))))

(defn-inline %body-first-contact-joint ((body body)) foreign-pointer
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;; {TODO} need an index of contact-joints
  (newtonbodygetfirstcontactjoint
   (%body-ptr body)))

(defn %body-next-contact-joint ((body body)
                                (contact-joint foreign-pointer))
    foreign-pointer
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;; {TODO} need an index of contact-joints
  (newtonbodygetnextcontactjoint
   (%body-ptr body)
   contact-joint))

(defmacro do-body-contact-joints ((var-name body-obj) &body body)
  ;; hidden is just so the user can't fuck the process by setting
  ;; var-name to something else
  (with-gensyms (gbody hidden)
    `(let* ((,gbody ,body-obj)
            (,hidden (%body-first-contact-joint ,gbody)))
       (loop :until (null-pointer-p ,hidden) :do
          (let ((,var-name (%joint-ptr->joint ,hidden)))
            ,@body
            (setf ,hidden (%body-next-contact-joint ,gbody ,hidden)))))))
