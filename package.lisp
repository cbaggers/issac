;;;; package.lisp

(uiop:define-package #:issac
  (:use #:cl #:raw-bindings-newton #:structy-defclass :cffi)
  (:import-from #:alexandria :with-gensyms)
  (:import-from #:rtg-math :v!)
  (:shadow :callback)
  (:export
   ;; system
   :newton-version

   ;; world
   :world
   :make-world
   :free-world
   :world-destroy-all-bodies
   :world-step
   :world-step-async
   :world-wait-for-update-to-finish
   :world-last-update-time
   :world-substep-count
   :world-minimum-frame-rate
   :world-invalidate-cache
   :world-current-device
   :world-devices
   :world-broadphase-algorithm
   :world-solver-model
   :world-friction-model
   :world-contact-merge-tolerance
   :world-thread-count
   :world-max-thread-count
   :world-sync-thread-jobs
   :world-body-count
   :world-constraint-count

   ;; geometry
   :geometry
   :null-geometry
   :box-geometry
   :sphere-geometry
   :cone-geometry
   :capsule-geometry
   :cylinder-geometry
   :chamfer-cylinder-geometry
   :convex-hull-geometry
   :geometry-tree
   :make-null-geometry
   :make-box-geometry
   :make-sphere-geometry
   :make-cone-geometry
   :make-capsule-geometry
   :make-cylinder-geometry
   :make-chamfer-cylinder-geometry
   :make-convex-hull-geometry
   :make-geometry-tree
   :free-geometry
   :geometry-mode
   :geometry-scale
   :geometry-offset-matrix4
   :convex-shape-p
   :convex-static-p

   ;; bodies
   :body
   :make-body
   :free-body
   :body-id
   :body-world
   :body-group-id
   :body-position
   :body-add-force
   :body-add-force
   :body-add-torque
   :body-angular-damping
   :body-auto-sleep-p
   :body-centre-of-mass
   :body-collidable-p
   :body-recursive-collision-p
   :body-linear-damping
   :body-matrix4
   :body-angular-velocity
   :body-omega
   :body-torque
   :body-mass
   :body-mass-set
   :body-mass-matrix-set
   :body-inverse-mass
   :body-geometry-mass-set
   :body-inertia-matrix4
   :body-inverse-intertia-matrix4
   :body-velocity
   :body-point-velocity
   :body-integrate-velocity
   :body-force
   :body-force-acc
   :body-torque-acc
   :body-calculate-inverse-dynamics-force
   :body-set-force-torque-callback
   :body-sleeping-p
   :body-rotation
   :body-max-rotation-per-step))
