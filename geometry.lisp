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

(defun make-null-geometry (world)
  "Create a transparent collision primitive. Some times the
   application needs to create helper rigid bodies that will never
   collide with other bodies, for example the neck of a rag doll, or an
   internal part of an articulated structure. This can be done by using
   the material system but it too much work and it will increase
   unnecessarily the material count, and therefore the project
   complexity. The Null collision is a collision object that satisfy all
   this conditions without having to change the engine philosophy."
  (let ((wptr (%world-ptr world)))
    (%make-null
     :ptr (newtoncreatenull wptr))))

(defun make-box-geometry (world &key (dimensions (v! 1s0 1s0 1s0))
                                  (offset-matrix4 (m4:identity)))
  "Create a box primitive for collision."
  (let ((wptr (%world-ptr world)))
    (with-foreign-array (m4 offset-matrix4 '(:array :float 16))
      (%make-box
       :ptr (newtoncreatebox
             wptr (v:x dimensions) (v:y dimensions) (v:z dimensions) 0 m4)))))

(defun make-sphere-geometry (world &key (radius 1s0)
                                     (offset-matrix4 (m4:identity)))
  "Create a generalized ellipsoid primitive.. Sphere collision are
   generalized ellipsoids, the application can create many different kind
   of objects by just playing with dimensions of the radius. for example
   to make a sphere set all tree radius to the same value, to make a
   ellipse of revolution just set two of the tree radius to the same
   value."
  (let ((wptr (%world-ptr world)))
    (with-foreign-array (m4 offset-matrix4 '(:array :float 16))
      (%make-sphere
       :ptr (newtoncreatesphere wptr 0 (float radius) m4)))))

(defun make-cone-geometry (world &key (radius 1s0) (height 1s0)
                                   (offset-matrix4 (m4:identity)))
  "Create a cone primitive for collision."
  (let ((wptr (%world-ptr world)))
    (with-foreign-array (m4 offset-matrix4 '(:array :float 16))
      (%make-cone
       :ptr (newtoncreatecone wptr (float radius) (float height) 0  m4)))))

(defun make-capsule-geometry (world &key (radius-0 1s0) (radius-1 1s0)
                                      (height (* 2 (max radius-1 radius-0)))
                                      (offset-matrix4 (m4:identity)))
  "Create a capsule primitive for collision. the capsule height must
   equal of larger than the sum of the cap radius. If this is not the
   case the height will be clamped the 2 * radius."
  (let ((wptr (%world-ptr world)))
    (with-foreign-array (m4 offset-matrix4 '(:array :float 16))
      (%make-capsule
       :ptr (newtoncreatecapsule
             wptr (float radius-0) (float radius-1) (float height) 0 m4)))))

(defun make-cylinder-geometry (world &key (radius-0 1s0) (radius-1 1s0)
                                       (height 4s0)
                                       (offset-matrix4 (m4:identity)))
  "Create a cylinder primitive for collision."
  (let ((wptr (%world-ptr world)))
    (with-foreign-array (m4 offset-matrix4 '(:array :float 16))
      (%make-cylinder
       :ptr (newtoncreatecylinder
             wptr (float radius-0) (float radius-1) (float height) 0 m4)))))

(defun make-chamfer-cylinder-geometry (world &key (radius 1s0) (height 1s0)
                                               (offset-matrix4 (m4:identity)))
  "Create a ChamferCylinder primitive for collision."
  (let ((wptr (%world-ptr world)))
    (with-foreign-array (m4 offset-matrix4 '(:array :float 16))
      (%make-chamfer-cylinder
       :ptr (newtoncreatechamfercylinder
             wptr (float radius) (float height) 0 m4)))))


(defun make-convex-hull-geometry (world points/mesh
                                  &key (tolerance 1s0)
                                    (offset-matrix4 (m4:identity)))
  "Create a ConvexHull primitive from collision from a cloud of
   points. Convex hulls are the solution to collision primitive that can
   not be easily represented by an implicit solid. The implicit solid
   primitives (spheres, cubes, cylinders, capsules, cones, etc.), have
   constant time complexity for contact calculation and are also
   extremely efficient on memory usage, therefore the application get
   perfect smooth behavior. However for cases where the shape is too
   difficult or a polygonal representation is desired, convex hulls come
   closest to the to the model shape. For example it is a mistake to
   model a 10000 point sphere as a convex hull when the perfect sphere is
   available, but it is better to represent a pyramid by a convex hull
   than with a sphere or a box.

   There is not upper limit as to how many vertex the application can
   pass to make a hull shape, however for performance and memory usage
   concern it is the application responsibility to keep the max vertex
   at the possible minimum. The minimum number of vertex should be
   equal or larger than 4 and it is the application responsibility
   that the points are part of a solid geometry. Unpredictable results
   will occur if all points happen to be collinear or coplanar.The
   performance of collision with convex hull proxies is sensitive to
   the vertex count of the hull. Since a the convex hull of a visual
   geometry is already an approximation of the mesh, for visual
   purpose there is not significant difference between the appeal of a
   exact hull and one close to the exact hull but with but with a
   smaller vertex count. It just happens that sometime complex meshes
   lead to generation of convex hulls with lots of small detail that
   play not roll of the quality of the simulation but that have a
   significant impact on the performance because of a large vertex
   count. For this reason the application have the option to set a
   tolerance parameter. tolerance* is use to post process the final
   geometry in the following faction, a point on the surface of the
   hull can be removed if the distance of all of the surrounding vertex
   immediately adjacent to the average plane equation formed the faces
   adjacent to that point, is smaller than the tolerance. A value of
   zero in tolerance will generate an exact hull and a value langer
   that zero will generate a loosely fitting hull and it willbe faster
   to generate."
  (let ((wptr (%world-ptr world)))
    (with-foreign-array (m4 offset-matrix4 '(:array :float 16))
      (etypecase points/mesh
        (mesh
         (%make-convex-hull-from-points wptr points/mesh tolerance m4))
        (sequence
         (%make-convex-hull-from-points wptr points/mesh tolerance m4))))))

(defun %make-convex-hull-from-points (wptr mesh tolerance m4)
  (declare (ignore wptr mesh tolerance m4))
  (warn "Not implemented"))

(defun %make-convex-hull-from-points (wptr points tolerance m4)
  (assert (>= (length points) 4))
  (let ((len (length points)))
    (with-foreign-object (p-ptr :float (* 3 len))
      (loop :for i :below len :do
         (let ((vec (elt points i)))
           (setf (mem-aref p-ptr :float (+ (* i 3) 0)) (v:x vec)
                 (mem-aref p-ptr :float (+ (* i 3) 1)) (v:y vec)
                 (mem-aref p-ptr :float (+ (* i 3) 2)) (v:z vec))))
      (%make-convex-hull-geometry
       :ptr (newtoncreateconvexhull
             wptr len p-ptr 12 (float tolerance) 0 m4)))))
