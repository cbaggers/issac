(in-package :issac)

;;------------------------------------------------------------
;;
;; NewtonMaterial* is a pair so we call it material-pair
;;
;; MaterialGroupID is what we call a material (it is represented as an int)
;;
;;------------------------------------------------------------

(defn world-default-material ((world world)) (signed-byte 32)
  "Gets the defaul material for this world.

   A Material can be interpreted as the nodes of a dense graph. The edges of
   the graph are called material-pairs and we describe the relationships
   between pairs of materials.

   When the Newton world is created, the default material is created by the
   engine. When bodies are created the application assigns a material to the
   body."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (NewtonMaterialGetDefaultGroupID (%world-ptr world)))


;; NewtonMaterialCreateGroupID


(defn create-material ((world world)) (signed-byte 32)
  "Create a new material.

   A Material can be interpreted as the nodes of a dense graph. The edges of
   the graph are called material-pairs and we describe the relationships
   between pairs of materials.

   When the Newton world is created, the default material is created by the
   engine. When bodies are created the application assigns a material to the
   body.

   Note: The only way to destroy a material after its creation is by
   destroying all the bodies and calling the function `destroy-all-materials`"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (NewtonMaterialCreateGroupID (%world-ptr world)))

(defn destroy-all-materials ((world world)) null
  "Remove all materials from the Newton world. This function must be called
   after there are no more rigid bodies in the word."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (NewtonMaterialDestroyAllGroupID (%world-ptr world))
  nil)

(defn material-pair-body-colliding-shape ((material-pair material-pair)
                                          (body body))
    geometry
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (%geom-ptr->geom
   (newtonmaterialgetbodycollidingshape
    (%material-pair-ptr material-pair)
    (%body-ptr body))))

(defn material-pair-contact-face-attribute ((material-pair material-pair))
    (unsigned-byte 32)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtonmaterialgetcontactfaceattribute
   (%material-pair-ptr material-pair)))

(defun material-pair-contact-force (material-pair body)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-foreign-object (force :float)
    (newtonmaterialgetcontactforce
     (%material-pair-ptr material-pair)
     (%body-ptr body)
     force)
    (mem-aref force :float)))

(defun material-pair-contact-max-normal-impact (material-pair)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtonmaterialgetcontactmaxnormalimpact
   (%material-pair-ptr material-pair)))

(defun material-pair-contact-max-tangent-impact (material-pair index)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtonmaterialgetcontactmaxtangentimpact
   (%material-pair-ptr material-pair)
   index))

(defun material-pair-contact-normal-speed (material-pair)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtonmaterialgetcontactnormalspeed
   (%material-pair-ptr material-pair)))

(defun material-pair-contact-penetration (material-pair)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtonmaterialgetcontactpenetration
   (%material-pair-ptr material-pair)))

(defun material-pair-contact-tangent-directions (material-pair body)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-foreign-objects ((t0 :float 3)
                         (t1 :float 3))
    (newtonmaterialgetcontacttangentdirections
     (%material-pair-ptr material-pair)
     (%body-ptr body) t0 t1)
    (values (ptr->v3 t0) (ptr->v3 t1))))

(defun material-pair-contact-tangent-speed (material-pair index)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtonmaterialgetcontacttangentspeed
   (%material-pair-ptr material-pair)
   index))

(defun material-pair-set-callback-userdata (material-pair
                                            material-a
                                            material-b
                                            ptr)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtonmaterialsetcallbackuserdata
   (%material-pair-ptr material-pair)
   material-a
   material-b
   ptr)
  (values))

(defun material-pair-set-contact-elasticity (material-pair restitution)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtonmaterialsetcontactelasticity
   (%material-pair-ptr material-pair)
   restitution)
  (values))

(defun material-pair-set-contact-friction-coef (material-pair
                                                static-friction-coefficient
                                                kinetic-friction-coefficient
                                                index)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtonmaterialsetcontactfrictioncoef
   (%material-pair-ptr material-pair)
   static-friction-coefficient
   kinetic-friction-coefficient
   index)
  (values))

(defun material-pair-set-contact-friction-state (material-pair state index)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  ;; {TODO} what is the state in lisp? (its an int in C)
  (newtonmaterialsetcontactfrictionstate
   (%material-pair-ptr material-pair)
   state
   index)
  (values))

(defun material-pair-set-contact-softness (material-pair softness)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtonmaterialsetcontactsoftness
   (%material-pair-ptr material-pair)
   softness)
  (values))

(defun material-pair-set-contact-tangent-acceleration (material-pair
                                                       acceleration
                                                       index)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtonmaterialsetcontacttangentacceleration
   (%material-pair-ptr material-pair)
   acceleration
   index)
  (values))

(defun material-pair-set-contact-tangent-friction (material-pair
                                                   friction
                                                   index)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtonmaterialsetcontacttangentfriction
   (%material-pair-ptr material-pair)
   friction
   index)
  (values))

(defun world-set-default-material-collidable (world
                                              material-a
                                              material-b
                                              state)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  ;; {TODO} how do we represent state?
  (newtonmaterialsetdefaultcollidable
   (%world-ptr world)
   material-a
   material-b
   state)
  (values))

(defun world-set-default-material-elasticity (world
                                              material-a
                                              material-b
                                              elasticity)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtonmaterialsetdefaultelasticity
   (%world-ptr world)
   material-a
   material-b
   elasticity)
  (values))

(defun world-set-default-material-friction (world
                                            material-a
                                            material-b
                                            static-friction-coefficient
                                            kinetic-friction-coefficient)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtonmaterialsetdefaultfriction
   (%world-ptr world)
   material-a
   material-b
   static-friction-coefficient
   kinetic-friction-coefficient)
  (values))

(defun world-set-default-material-softness (world
                                            material-a
                                            material-b
                                            softness)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtonmaterialsetdefaultsoftness
   (%world-ptr world)
   material-a
   material-b
   softness)
  (values))

(defun world-set-material-pair-surface-thickness (world
                                                  material-a
                                                  material-b
                                                  thickness)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtonmaterialsetsurfacethickness
   (%world-ptr world)
   material-a
   material-b
   thickness)
  (values))

(defun material-contact-rotate-tangent-directions (material-pair vec3)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-foreign-array (v3 vec3 '(:array :float 3))
    (newtonmaterialcontactrotatetangentdirections
     (%material-pair-ptr material-pair)
     v3)))

(defun material-pair-contact-position-and-normal (material-pair body)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-foreign-objects ((pos3 :float 3)
                         (norm3 :float 3))
    (newtonmaterialgetcontactpositionandnormal
     (%material-pair-ptr material-pair)
     (%body-ptr body) pos3 norm3)
    (values (ptr->v3 pos3) (ptr->v3 norm3))))

(defun material-pair-contact-position (material-pair body)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-foreign-objects ((pos3 :float 3)
                         (norm3 :float 3))
    (newtonmaterialgetcontactpositionandnormal
     (%material-pair-ptr material-pair)
     (%body-ptr body) pos3 norm3)
    (ptr->v3 pos3)))

(defun material-pair-contact-normal (material-pair body)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-foreign-objects ((pos3 :float 3)
                         (norm3 :float 3))
    (newtonmaterialgetcontactpositionandnormal
     (%material-pair-ptr material-pair)
     (%body-ptr body) pos3 norm3)
    (ptr->v3 norm3)))

(defun (setf material-pair-contact-position) (pos3 material-pair)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-foreign-array (p3 pos3 '(:array :float 3))
    (newtonmaterialsetcontactposition
     (%material-pair-ptr material-pair)
     p3))
  pos3)

(defun (setf material-pair-contact-normal) (vec3 material-pair)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-foreign-array (n3 vec3 '(:array :float 3))
    (newtonmaterialsetcontactnormaldirection
     (%material-pair-ptr material-pair)
     n3))
  vec3)

;; NewtonMaterialSetCollisionCallback
;; {TODO} need a bit more info on contacts process before doing this
