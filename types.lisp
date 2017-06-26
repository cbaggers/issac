(in-package :issac)

;;------------------------------------------------------------

(defstruct (geometry (:constructor %make-geometry)
                     (:conc-name %geometry-))
  (ptr (error "") :type foreign-pointer))

(defstruct (null-geometry
            (:constructor %make-null)
            (:include geometry))
  (iterator-callback  ))

(defstruct (box-geometry
            (:constructor %make-box)
            (:include geometry)))

(defstruct (sphere-geometry
            (:constructor %make-sphere)
            (:include geometry)))

(defstruct (cone-geometry
            (:constructor %make-cone)
            (:include geometry)))

(defstruct (capsule-geometry
            (:constructor %make-capsule)
            (:include geometry)))

(defstruct (cylinder-geometry
            (:constructor %make-cylinder)
            (:include geometry)))

(defstruct (chamfer-cylinder-geometry
            (:constructor %make-chamfer-cylinder)
            (:include geometry)))

(defstruct (convex-hull-geometry
            (:constructor %make-convex-hull-geometry)
            (:include geometry)))

(defstruct (geometry-tree
            (:constructor %make-geometry-tree)
            (:include geometry)))

(defstruct (compound-geometry
            (:constructor %make-compound-geometry)
            (:include geometry)))

(defstruct (height-field-geometry
            (:constructor %make-height-field)
            (:include geometry)))

(defstruct (scene-geometry
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

(defstruct (skeleton (:constructor %make-skeleton)
                  (:conc-name %skeleton-))
  (ptr (error "") :type foreign-pointer))

;;------------------------------------------------------------

(defstruct (mesh (:constructor %make-mesh)
                     (:conc-name %mesh-))
  (ptr (error "") :type foreign-pointer))

;;------------------------------------------------------------

(defstruct (joint (:constructor %make-joint)
                  (:conc-name %joint-))
  (ptr (error "") :type foreign-pointer))

(defstruct (ball-&-socket (:constructor %make-ball-&-socket) (:include joint)))

(defstruct (corkscrew (:constructor %make-corkscrew) (:include joint)))

(defstruct (hinge (:constructor %make-hinge) (:include joint)))

(defstruct (slider (:constructor %make-slider) (:include joint)))

(defstruct (universal-joint (:constructor %make-universal-joint)
                            (:include joint)))

(defstruct (up-vector (:constructor %make-up-vector) (:include joint)))

;; An advanced feature we dont need yet
;; (defstruct (bilateral-joint (:constructor %make-bilateral-joint)
;;                             (:include joint)))

;;------------------------------------------------------------

(defstruct (material-pair (:constructor %make-material-pair)
                     (:conc-name %material-pair-))
  (ptr (error "") :type foreign-pointer))

;;------------------------------------------------------------
;; NewtonWorld* - newtonworld - world

(defvar *world-id* -1)

(defun gen-world-id ()
  (incf *world-id*))

(defstruct (world (:constructor %make-world)
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
