(in-package :issac)

;;------------------------------------------------------------

(deftclass (geometry (:constructor %make-geometry)
                     (:conc-name %geometry-))
  (ptr (error "") :type foreign-pointer))

(deftclass (null-geometry
            (:constructor %make-null)
            (:include geometry)))

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

;;------------------------------------------------------------

(deftclass (body (:constructor %make-body)
                  (:conc-name %body-))
  (ptr (error "") :type foreign-pointer))

;;------------------------------------------------------------

(deftclass (mesh (:constructor %make-mesh)
                     (:conc-name %mesh-))
  (ptr (error "") :type foreign-pointer))

;;------------------------------------------------------------

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