(in-package :issac)

;;------------------------------------------------------------
;;
;; NewtonMaterial* is a pair so we call it material-pair
;;
;; MaterialGroupID is what we call a material (it is represented as an int)
;;
;;------------------------------------------------------------

(defun world-default-material (world)
  "Gets the defaul material for this world.

   A Material can be interpreted as the nodes of a dense graph. The edges of
   the graph are called material-pairs and we describe the relationships
   between pairs of materials.

   When the Newton world is created, the default material is created by the
   engine. When bodies are created the application assigns a material to the
   body."
  (NewtonMaterialGetDefaultGroupID (%world-ptr world)))


;; NewtonMaterialCreateGroupID


(defun create-material (world)
  "Create a new material.

   A Material can be interpreted as the nodes of a dense graph. The edges of
   the graph are called material-pairs and we describe the relationships
   between pairs of materials.

   When the Newton world is created, the default material is created by the
   engine. When bodies are created the application assigns a material to the
   body.

   Note: The only way to destroy a material after its creation is by
   destroying all the bodies and calling the function `destroy-all-materials`"
  (NewtonMaterialCreateGroupID (%world-ptr world)))

(defun destroy-all-materials (world)
  "Remove all materials from the Newton world. This function must be called
   after there are no more rigid bodies in the word."
  (NewtonMaterialDestroyAllGroupID (%world-ptr world)))


(defun material-pair-body-colliding-shape (material-pair body)
  (%geom-ptr->geom
   (newtonmaterialgetbodycollidingshape
    (%material-pair-ptr material-pair)
    (%body-ptr body))))

(defun material-pair-contact-face-attribute (material-pair)
  (newtonmaterialgetcontactfaceattribute
   (%material-pair-ptr material-pair)))

(defun material-pair-contact-force (material-pair body)
  (with-foreign-object (force :float)
    (newtonmaterialgetcontactforce
     (%material-pair-ptr material-pair)
     (%body-ptr body)
     force)
    (mem-aref force :float)))

(defun material-pair-contact-max-normal-impact (material-pair)
  (newtonmaterialgetcontactmaxnormalimpact
   (%material-pair-ptr material-pair)))

(defun material-pair-contact-max-tangent-impact (material-pair index)
  (newtonmaterialgetcontactmaxtangentimpact
   (%material-pair-ptr material-pair)
   index))

(defun material-pair-contact-normal-speed (material-pair)
  (newtonmaterialgetcontactnormalspeed
   (%material-pair-ptr material-pair)))

(defun material-pair-contact-penetration (material-pair)
  (newtonmaterialgetcontactpenetration
   (%material-pair-ptr material-pair)))

(defun material-pair-contact-tangent-directions (material-pair body)
  (with-foreign-objects ((t0 :float 3)
                         (t1 :float 3))
    (newtonmaterialgetcontacttangentdirections
     (%material-pair-ptr material-pair)
     (%body-ptr body) t0 t1)
    (values (ptr->v3 t0) (ptr->v3 t1))))

(defun material-pair-contact-tangent-speed (material-pair index)
  (newtonmaterialgetcontacttangentspeed
   (%material-pair-ptr material-pair)
   index))

(defun material-pair-set-callback-userdata (material-pair
                                            material-a
                                            material-b
                                            ptr)
  (newtonmaterialsetcallbackuserdata
   (%material-pair-ptr material-pair)
   material-a
   material-b
   ptr)
  (values))

(defun material-pair-set-contact-elasticity (material-pair restitution)
  (newtonmaterialsetcontactelasticity
   (%material-pair-ptr material-pair)
   restitution)
  (values))

(defun material-pair-set-contact-friction-coef (material-pair
                                                static-friction-coefficient
                                                kinetic-friction-coefficient
                                                index)
  (newtonmaterialsetcontactfrictioncoef
   (%material-pair-ptr material-pair)
   static-friction-coefficient
   kinetic-friction-coefficient
   index)
  (values))

(defun material-pair-set-contact-friction-state (material-pair state index)
  ;; {TODO} what is the state in lisp? (its an int in C)
  (newtonmaterialsetcontactfrictionstate
   (%material-pair-ptr material-pair)
   state
   index)
  (values))

(defun material-pair-set-contact-softness (material-pair softness)
  (newtonmaterialsetcontactsoftness
   (%material-pair-ptr material-pair)
   softness)
  (values))

(defun material-pair-set-contact-tangent-acceleration (material-pair
                                                       acceleration
                                                       index)
  (newtonmaterialsetcontacttangentacceleration
   (%material-pair-ptr material-pair)
   acceleration
   index)
  (values))

(defun material-pair-set-contact-tangent-friction (material-pair
                                                   friction
                                                   index)
  (newtonmaterialsetcontacttangentfriction
   (%material-pair-ptr material-pair)
   friction
   index)
  (values))

(defun world-set-default-material-collidable (world
                                              material-a
                                              material-b
                                              state)
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
  (newtonmaterialsetsurfacethickness
   (%world-ptr world)
   material-a
   material-b
   thickness)
  (values))

(defun material-contact-rotate-tangent-directions (material-pair vec3)
  (with-foreign-array (v3 vec3 '(:array :float 3))
    (newtonmaterialcontactrotatetangentdirections
     (%material-pair-ptr material-pair)
     v3)))

(defun material-pair-contact-position-and-normal (material-pair body)
  (with-foreign-objects ((pos3 :float 3)
                         (norm3 :float 3))
    (newtonmaterialgetcontactpositionandnormal
     (%material-pair-ptr material-pair)
     (%body-ptr body) pos3 norm3)
    (values (ptr->v3 pos3) (ptr->v3 norm3))))

(defun material-pair-contact-position (material-pair body)
  (with-foreign-objects ((pos3 :float 3)
                         (norm3 :float 3))
    (newtonmaterialgetcontactpositionandnormal
     (%material-pair-ptr material-pair)
     (%body-ptr body) pos3 norm3)
    (ptr->v3 pos3)))

(defun material-pair-contact-normal (material-pair body)
  (with-foreign-objects ((pos3 :float 3)
                         (norm3 :float 3))
    (newtonmaterialgetcontactpositionandnormal
     (%material-pair-ptr material-pair)
     (%body-ptr body) pos3 norm3)
    (ptr->v3 norm3)))

(defun (setf material-pair-contact-position) (pos3 material-pair)
  (with-foreign-array (p3 pos3 '(:array :float 3))
    (newtonmaterialsetcontactposition
     (%material-pair-ptr material-pair)
     p3))
  pos3)

(defun (setf material-pair-contact-normal) (vec3 material-pair)
  (with-foreign-array (n3 vec3 '(:array :float 3))
    (newtonmaterialsetcontactnormaldirection
     (%material-pair-ptr material-pair)
     n3))
  vec3)

;; NewtonMaterialSetCollisionCallback
;; {TODO} need a bit more info on contacts process before doing this
