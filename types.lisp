(in-package :issac)

;;------------------------------------------------------------

(defstruct (geometry (:constructor %make-geometry)
                     (:conc-name %geometry-))
  (ptr (error "") :type foreign-pointer)
  (iterator-callback
   nil
   :type (or null (function (geometry
                             (array single-float (*))
                             (signed-byte 32))
                            single-float))))

(defstruct (null-geometry
            (:constructor %make-null)
            (:include geometry)))

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

(deftype geometry-tree-raycast-function ()
  '(function (geometry-tree
              body
              single-float
              rtg-math.types:vec3
              (signed-byte 32))
    t))

(deftype geometry-tree-debug-function ()
  '(function (geometry-tree
              body
              (signed-byte 32)
              (array single-float (*))
              (signed-byte 32))
    t))

(defstruct (geometry-tree
             (:constructor %make-geometry-tree)
             (:conc-name %geometry-tree-)
             (:include geometry))
  (raycast-callback
   nil
   :type (or null geometry-tree-raycast-function))
  (debug-callback
   nil
   :type (or null geometry-tree-debug-function)))

(defstruct (compound-geometry
            (:constructor %make-compound-geometry)
            (:include geometry)))

(defstruct (height-field-geometry
             (:constructor %make-height-field)
             (:conc-name %height-field-)
             (:include geometry))
  (ray-cast-callback
   nil
   :type (or null (function (height-field-geometry
                             body
                             single-float
                             (signed-byte 32)
                             (signed-byte 32)
                             rtg-math.types:vec3
                             (signed-byte 32))
                            single-float))))

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
   :type (or null (function (body single-float) t)))
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
  (ptr (error "") :type foreign-pointer)
  (destructor-callback nil :type (or null (function (joint) t))))

(defstruct joint-info
  (attachment-0-body
   (error "joint-info missing required data attachment-0-body")
   :type body)
  (attachment-1-body
   (error "joint-info missing required data attachment-1-body")
   :type body)
  (attachment-0-mat4
   (error "joint-info missing required data attachment-0-mat4")
   :type rtg-math.types:mat4)
  (attachment-1-mat4
   (error "joint-info missing required data attachment-1-mat4")
   :type rtg-math.types:mat4)
  (min-linear-dof
   (error "joint-info missing required data min-linear-dof")
   :type rtg-math.types:vec3)
  (max-linear-dof
   (error "joint-info missing required data max-linear-dof")
   :type rtg-math.types:vec3)
  (min-angular-dof
   (error "joint-info missing required data min-angular-dof")
   :type rtg-math.types:vec3)
  (max-angular-dof
   (error "joint-info missing required data max-angular-dof")
   :type rtg-math.types:vec3)
  (extra-params
   (error "joint-info missing required data extra-params")
   :type (array single-float (64)))
  (bodies-collision-on
   (error "joint-info missing required data bodies-collision-on")
   :type boolean)
  (description
   (error "joint-info missing required data description")
   :type string))

(defstruct (ball-&-socket (:constructor %make-ball-&-socket)
                          (:conc-name %ball-&-socket-)
                          (:include joint))
  (callback
   nil
   :type (or null (function (ball-&-socket single-float) t))))

(defstruct (corkscrew (:constructor %make-corkscrew)
                      (:conc-name %corkscrew-)
                      (:include joint))
  (callback
   nil
   :type (or null (function (corkscrew
                             single-float
                             single-float
                             single-float
                             single-float)
                            (unsigned-byte 32)))))

(defstruct (hinge (:constructor %make-hinge)
                  (:conc-name %hinge-)
                  (:include joint))
  (callback
   nil
   :type (or null (function (hinge
                             single-float
                             single-float
                             single-float
                             single-float)
                            (unsigned-byte 32)))))

(defstruct (slider (:constructor %make-slider)
                   (:conc-name %slider-)
                   (:include joint))
  (callback
   nil
   :type (or null (function (slider
                             single-float
                             single-float
                             single-float
                             single-float)
                            (unsigned-byte 32)))))

(defstruct (universal-joint (:constructor %make-universal-joint)
                            (:conc-name %universal-)
                            (:include joint))
  (callback
   nil
   :type (or null (function (universal-joint
                             single-float
                             single-float
                             single-float
                             single-float)
                            (unsigned-byte 32)))))

(defstruct (up-vector (:constructor %make-up-vector)
                      (:conc-name %up-vector-)
                      (:include joint)))

;; An advanced feature we dont need yet
(defstruct (bilateral-joint (:constructor %make-bilateral-joint)
                            (:conc-name %bilateral-)
                            (:include joint))
  (callback
   nil
   :type (or null (function (bilateral-joint single-float) t)))
  (info-callback
   nil
   :type (or null (function (bilateral-joint joint-info) t))))

;;------------------------------------------------------------

(defstruct (material-pair (:constructor %make-material-pair)
                          (:conc-name %material-pair-))
  (ptr (error "") :type foreign-pointer)
  (aabb-overlap-callback
   nil
   :type (or null (function (material-pair body body)
                            (signed-byte 32)))))

;;------------------------------------------------------------
;; NewtonWorld* - newtonworld - world

(defvar *world-id* -1)

(defun gen-world-id ()
  (incf *world-id*))

(deftype ray-filter-function ()
  '(function (body
              geometry
              rtg-math.types:vec3
              rtg-math.types:vec3
              (signed-byte 64)
              single-float)
    single-float))

(deftype ray-prefilter-function ()
  '(function (body geometry) (unsigned-byte 32)))

(deftype body-iterator-function ()
  '(function (body) (signed-byte 32)))

(defstruct (world (:constructor %make-world)
                  (:conc-name %world-))
  (ptr (error "") :type foreign-pointer)
  (id (gen-world-id) :type (unsigned-byte 16))
  (solve-model (error "") :type t)
  (friction-model (error "") :type keyword)
  (min-frame-rate 60 :type (unsigned-byte 16))
  ;;
  (body-iterator-callback
   nil :type (or null body-iterator-function))
  (geom-constructor-callback
   nil :type (or null (function (world body body) t)))
  (geom-destructor-callback
   nil :type (or null (function (world body) t)))
  (pre-update-listener-callback
   nil :type (or null (function (world single-float) t)))
  (post-update-listener-callback
   nil :type (or null (function (world single-float) t)))
  (pre-destroy-listener-callback
   nil :type (or null (function (world) t)))
  (post-destroy-listener-callback
   nil :type (or null (function (world) t)))
  (destructor-callback
   nil :type (or null (function (world) t)))
  (ray-filter-callback
   nil :type (or null ray-filter-function))
  (ray-prefilter-callback
   nil :type (or null ray-prefilter-function))
  (update-listener-callback
   nil :type (or null (function (world foreign-pointer single-float)
                                t))))

(declaim (type world *null-world*))
(defvar *null-world*
  (%make-world :ptr (null-pointer)
               :solve-model nil
               :friction-model :null
               :min-frame-rate 0))

;;------------------------------------------------------------
