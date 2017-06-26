(in-package :issac)

;;------------------------------------------------------------

(deftclass (geometry (:constructor %make-geometry)
                     (:conc-name %geometry-))
  (ptr (error "") :type foreign-pointer))

(deftclass (null-geometry
            (:constructor %make-null)
            (:include geometry))
  (iterator-callback  ))

(deftclass (box-geometry
            (:constructor %make-box)
            (:include geometry)))

(deftclass (sphere-geometry
            (:constructor %make-sphere)
            (:include geometry)))

(deftclass (cone-geometry
            (:constructor %make-cone)
            (:include geometry)))

(deftclass (capsule-geometry
            (:constructor %make-capsule)
            (:include geometry)))

(deftclass (cylinder-geometry
            (:constructor %make-cylinder)
            (:include geometry)))

(deftclass (chamfer-cylinder-geometry
            (:constructor %make-chamfer-cylinder)
            (:include geometry)))

(deftclass (convex-hull-geometry
            (:constructor %make-convex-hull-geometry)
            (:include geometry)))

(deftclass (geometry-tree
            (:constructor %make-geometry-tree)
            (:include geometry)))

(deftclass (compound-geometry
            (:constructor %make-compound-geometry)
            (:include geometry)))

(deftclass (height-field-geometry
            (:constructor %make-height-field)
            (:include geometry)))

(deftclass (scene-geometry
            (:constructor %make-scene)
            (:include geometry)))

;;------------------------------------------------------------
;; NewtonBody* - newtonbody - body

(defstruct (body (:constructor %make-body)
                 (:conc-name %body-))
  (ptr (error "") :type foreign-pointer)
  (force-torque-callback
   nil
   :type (or null (function (body integer) t)))
  (destructor-callback
   nil
   :type (or null (function (body) t)))
  (transform-callback
   nil
   :type (or null (function (body rtg-math.types:mat4) t))))

;;------------------------------------------------------------
;; NewtonSkeletonContainer* - newtonskeletoncontainer - skeleton

(deftclass (skeleton (:constructor %make-skeleton)
                  (:conc-name %skeleton-))
  (ptr (error "") :type foreign-pointer))

;;------------------------------------------------------------

(deftclass (mesh (:constructor %make-mesh)
                     (:conc-name %mesh-))
  (ptr (error "") :type foreign-pointer))

;;------------------------------------------------------------

(deftclass (joint (:constructor %make-joint)
                  (:conc-name %joint-))
  (ptr (error "") :type foreign-pointer))

(deftclass (ball-&-socket (:constructor %make-ball-&-socket) (:include joint)))

(deftclass (corkscrew (:constructor %make-corkscrew) (:include joint)))

(deftclass (hinge (:constructor %make-hinge) (:include joint)))

(deftclass (slider (:constructor %make-slider) (:include joint)))

(deftclass (universal-joint (:constructor %make-universal-joint)
                            (:include joint)))

(deftclass (up-vector (:constructor %make-up-vector) (:include joint)))

;; An advanced feature we dont need yet
;; (deftclass (bilateral-joint (:constructor %make-bilateral-joint)
;;                             (:include joint)))

;;------------------------------------------------------------

(deftclass (material-pair (:constructor %make-material-pair)
                     (:conc-name %material-pair-))
  (ptr (error "") :type foreign-pointer))

;;------------------------------------------------------------
;; NewtonWorld* - newtonworld - world

(defvar *world-id* -1)

(defun gen-world-id ()
  (incf *world-id*))

(deftclass (world (:constructor %make-world)
                  (:conc-name %world-))
  (ptr (error "") :type foreign-pointer)
  (id (gen-world-id) :type (unsigned-byte 16))
  (solve-model (error "") :type t)
  (friction-model (error "") :type keyword)
  (min-frame-rate 60 :type (unsigned-byte 16)))

(defvar *null-world*
  (%make-world :ptr (null-pointer)
               :solve-model nil
               :friction-model :null
               :min-frame-rate 0))

;;------------------------------------------------------------
