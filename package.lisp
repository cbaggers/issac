;;;; package.lisp

(uiop:define-package #:issac
    (:use #:cl #:raw-bindings-newton :cffi :%rtg-math)
  (:import-from #:alexandria :with-gensyms)
  (:import-from #:rtg-math :v!)
  (:shadow :callback)
  (:export

   ;; types
   :ball-&-socket
   :bilateral-joint
   :body
   :box-geometry
   :capsule-geometry
   :chamfer-cylinder-geometry
   :compound-geometry
   :cone-geometry
   :convex-hull-geometry
   :corkscrew
   :cylinder-geometry
   :geometry
   :geometry-tree
   :height-field-geometry
   :hinge
   :joint
   :material-pair
   :mesh
   :null-geometry
   :scene-geometry
   :skeleton
   :slider
   :sphere-geometry
   :universal-joint
   :up-vector
   :world
   :joint-info
   :ll-collision

   ;;
   :calculate-spring-damper-acceleration

   :ball-&-socket-callback
   :ball-&-socket-force
   :ball-&-socket-omega
   :ball-&-socket-set-cone-limits

   :bilateral-callback
   :bilateral-info-callback

   :body-aabb
   :body-add-force
   :body-add-impluse
   :body-add-torque
   :body-angular-damping
   :body-angular-velocity
   :body-apply-impulse-pair
   :body-auto-sleep-p
   :body-calculate-inverse-dynamics-force
   :body-centre-of-mass
   :body-collidable-p
   :body-continuous-collision-mode
   :body-destructor-callback
   :body-force
   :body-force-acc
   :body-force-torque-callback
   :body-geometry
   :body-geometry-mass-set
   :body-get-skeleton
   :body-id
   :body-inertia-matrix4
   :body-integrate-velocity
   :body-inverse-intertia-matrix4
   :body-inverse-mass
   :body-linear-damping
   :body-mass
   :body-mass-matrix-set
   :body-mass-set
   :body-material
   :body-matrix4
   :body-max-rotation-per-step
   :body-omega
   :body-point-velocity
   :body-position
   :body-recursive-collision-p
   :body-rotation
   :body-set-collision-scale
   :body-set-matrix-no-sleep
   :body-set-matrix-recursive
   :body-set-omega-no-sleep
   :body-set-velocity-no-sleep
   :body-simulation-state
   :body-sleeping-p
   :body-torque
   :body-torque-acc
   :body-transform-callback
   :body-velocity
   :body-world
   :do-body-contact-joints
   :do-body-joints

   :compound-add-sub-geometry
   :compound-from-node-get-geometry
   :compound-node
   :compound-node-index
   :compound-remove-sub-geometry
   :compound-remove-sub-geometry-by-index
   :compound-set-sub-geometry-matrix4
   :do-compound-nodes
   :with-compound-add-remove

   :contact-geometry-0
   :contact-geometry-1
   :contact-geometry-id-0
   :contact-geometry-id-1
   :contact-material-pair

   :convex-geometry-calculate-buoyancy-acceleration
   :convex-geometry-calculate-inertia
   :convex-geometry-calculate-volume
   :convex-shape-p
   :convex-static-p

   :corkscrew-calc-stop-acceleration
   :corkscrew-calc-stop-alpha
   :corkscrew-calc-stop-alpha
   :corkscrew-callback
   :corkscrew-force
   :corkscrew-omega
   :corkscrew-position
   :corkscrew-velocity

   :create-material
   :destroy-all-materials

   :create-scene-geometry

   :ensure-world-pool-size

   :free-body
   :free-geometry
   :free-joint
   :free-world

   :gen-world-id

   :geometry-calculate-aabb
   :geometry-collide
   :geometry-get-parent
   :geometry-mode
   :geometry-most-extreme-vertex
   :geometry-offset-matrix4
   :geometry-point-distance
   :geometry-scale
   :geometry-skin-thickness
   :geometry-tree-debug-callback
   :geometry-tree-ray-cast-callback
   :geometry-type
   :with-geometry

   :height-field-ray-cast-callback

   :hinge-callback
   :hinge-force
   :hinge-omega

   :initialize
   :uninitialize

   :joint-active-p
   :joint-body-0
   :joint-body-1
   :joint-closest-distance
   :joint-destructor-callback
   :joint-info
   :joint-linked-collide-p
   :joint-remove-contact
   :joint-stiffness
   :do-joint-contacts

   :make-ball-&-socket
   :make-body
   :make-box-geometry
   :make-capsule-geometry
   :make-chamfer-cylinder-geometry
   :make-compound-geometry
   :make-cone-geometry
   :make-convex-hull-geometry
   :make-corkscrew
   :make-cylinder-geometry
   :make-geometry-tree
   :make-height-field-geometry
   :make-hinge
   :make-null-geometry
   :make-slider
   :make-sphere-geometry
   :make-universal
   :make-up-vector
   :make-world

   :map-bodies-in-aabb

   :map-polygons-in-geometry

   :material-contact-rotate-tangent-directions
   :material-pair-body-colliding-shape
   :material-pair-contact-face-attribute
   :material-pair-contact-force
   :material-pair-contact-max-normal-impact
   :material-pair-contact-max-tangent-impact
   :material-pair-contact-normal
   :material-pair-contact-normal-speed
   :material-pair-contact-penetration
   :material-pair-contact-position
   :material-pair-contact-position-and-normal
   :material-pair-contact-tangent-directions
   :material-pair-contact-tangent-speed
   :material-pair-set-callback-userdata
   :material-pair-set-contact-elasticity
   :material-pair-set-contact-friction-coef
   :material-pair-set-contact-friction-state
   :material-pair-set-contact-softness
   :material-pair-set-contact-tangent-acceleration
   :material-pair-set-contact-tangent-friction
   :do-world-material-pairs

   :newton-version

   :scene-add-sub-geometry
   :scene-from-node-get-geometry
   :scene-node
   :scene-node-index
   :scene-remove-sub-geometry
   :scene-remove-sub-geometry-by-index
   :scene-set-sub-geometry-matrix4
   :do-scene-nodes
   :with-scene-add-remove

   :slider-calc-stop-acceleration
   :slider-callback
   :slider-force
   :slider-position
   :slider-velocity

   :sub-geometry-handle

   :tree-geometry-face-attribute


   :universal-joint-angle
   :universal-joint-calc-stop-alpha-0
   :universal-joint-calc-stop-alpha-1
   :universal-joint-callback
   :universal-joint-force
   :universal-joint-omega
   :universal-joint-omega-0
   :universal-joint-omega-1

   :up-vector-pin

   :valid-solver-model-p
   :validate-body-kind
   :validate-friction-model
   :validate-min-frame-rate
   :validate-solver-model

   :world-body-count
   :world-body-destructor
   :world-broadphase-algorithm
   :world-constraint-count
   :world-contact-merge-tolerance
   :world-current-device
   :world-default-material
   :world-destroy-all-bodies
   :world-destructor
   :world-devices
   :world-dispatch-thread-job
   :world-friction-model
   :world-geometry-constructor-callback
   :world-geometry-destructor-callback
   :world-invalidate-cache
   :world-last-update-time
   :world-max-thread-count
   :world-minimum-frame-rate
   :world-post-destroy-listener
   :world-post-update-listener
   :world-pre-destroy-listener
   :world-pre-update-listener
   :world-ray-cast
   :world-set-default-material-collidable
   :world-set-default-material-elasticity
   :world-set-default-material-friction
   :world-set-default-material-softness
   :world-set-material-pair-surface-thickness
   :world-solver-model
   :world-step
   :world-step-async
   :world-substep-count
   :world-sync-thread-jobs
   :world-thread-count
   :world-wait-for-update-to-finish
   :do-world-bodys))
