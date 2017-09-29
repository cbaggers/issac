(in-package :issac)

;;------------------------------------------------------------

(defn-inline %init-data ((geometry geometry)) geometry
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (setf (%geometry-user-data geometry)
        (make-pointer (%add-geom-to-system geometry)))
  geometry)

(defn make-null-geometry ((world world)) null-geometry
  "Create a transparent collision primitive. Some times the
   application needs to create helper rigid bodies that will never
   collide with other bodies, for example the neck of a rag doll, or an
   internal part of an articulated structure. This can be done by using
   the material system but it too much work and it will increase
   unnecessarily the material count, and therefore the project
   complexity. The Null collision is a collision object that satisfy all
   this conditions without having to change the engine philosophy."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((wptr (%world-ptr world)))
    (%init-data
     (%make-null
      :ptr (newtoncreatenull wptr)))))

(defn make-box-geometry
    ((world world)
     &key
     (dimensions (simple-array single-float (3)) (v! 1f0 1f0 1f0))
     (offset-matrix4 (simple-array single-float (16)) (m4:identity)))
    box-geometry
  "Create a box primitive for collision."
  (let ((wptr (%world-ptr world)))
    (with-foreign-array (m4 offset-matrix4 '(:array :float 16))
      (%init-data
       (%make-box
        :ptr (newtoncreatebox
              wptr (v:x dimensions) (v:y dimensions) (v:z dimensions) 0 m4))))))

(defn make-sphere-geometry
    ((world world)
     &key
     (radius single-float 1f0)
     (offset-matrix4 (simple-array single-float (16)) (m4:identity)))
    sphere-geometry
  "Create a generalized ellipsoid primitive.. Sphere collision are
   generalized ellipsoids, the application can create many different kind
   of objects by just playing with dimensions of the radius. for example
   to make a sphere set all tree radius to the same value, to make a
   ellipse of revolution just set two of the tree radius to the same
   value."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((wptr (%world-ptr world)))
    (with-foreign-array (m4 offset-matrix4 '(:array :float 16))
      (%init-data
       (%make-sphere
        :ptr (newtoncreatesphere wptr (float radius) 0 m4))))))

(defn make-cone-geometry ((world world)
                          &key
                          (radius single-float 1f0)
                          (height single-float 1f0)
                          (offset-matrix4 (simple-array single-float (16)) (m4:identity)))
    cone-geometry
  "Create a cone primitive for collision."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((wptr (%world-ptr world)))
    (with-foreign-array (m4 offset-matrix4 '(:array :float 16))
      (%init-data
       (%make-cone
        :ptr (newtoncreatecone wptr (float radius) (float height) 0  m4))))))

(defn make-capsule-geometry ((world world)
                             &key
                             (radius-0 single-float 1f0)
                             (radius-1 single-float 1f0)
                             (height single-float (* 2 (max radius-1 radius-0)))
                             (offset-matrix4 (simple-array single-float (16)) (m4:identity)))
    capsule-geometry
  "Create a capsule primitive for collision. the capsule height must
   equal of larger than the sum of the cap radius. If this is not the
   case the height will be clamped the 2 * radius."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((wptr (%world-ptr world)))
    (with-foreign-array (m4 offset-matrix4 '(:array :float 16))
      (%init-data
       (%make-capsule
        :ptr (newtoncreatecapsule
              wptr (float radius-0) (float radius-1) (float height) 0 m4))))))

(defn make-cylinder-geometry ((world world)
                              &key
                              (radius-0 single-float 1f0)
                              (radius-1 single-float 1f0)
                              (height single-float 4f0)
                              (offset-matrix4 (simple-array single-float (16)) (m4:identity)))
    cylinder-geometry
  "Create a cylinder primitive for collision."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((wptr (%world-ptr world)))
    (with-foreign-array (m4 offset-matrix4 '(:array :float 16))
      (%init-data
       (%make-cylinder
        :ptr (newtoncreatecylinder
              wptr (float radius-0) (float radius-1) (float height) 0 m4))))))

(defn make-chamfer-cylinder-geometry ((world world)
                                      &key
                                      (radius single-float 1f0)
                                      (height single-float 1f0)
                                      (offset-matrix4 (simple-array single-float (16)) (m4:identity)))
    chamfer-cylinder-geometry
  "Create a ChamferCylinder primitive for collision."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((wptr (%world-ptr world)))
    (with-foreign-array (m4 offset-matrix4 '(:array :float 16))
      (%init-data
       (%make-chamfer-cylinder
        :ptr (newtoncreatechamfercylinder
              wptr (float radius) (float height) 0 m4))))))


(defn make-convex-hull-geometry ((world world)
                                 (points/mesh (or mesh sequence))
                                 &key
                                 (tolerance single-float 1f0)
                                 (offset-matrix4 (simple-array single-float (16)) (m4:identity)))
    convex-hull-geometry
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
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((wptr (%world-ptr world)))
    (with-foreign-array (m4 offset-matrix4 '(:array :float 16))
      (%init-data
       (etypecase points/mesh
         (mesh
          (%make-convex-hull-from-mesh wptr points/mesh tolerance m4))
         (sequence
          (%make-convex-hull-from-points wptr points/mesh tolerance m4)))))))

(defun %make-convex-hull-from-mesh (wptr mesh tolerance m4)
  (declare (ignore wptr mesh tolerance m4))
  (error "Not implemented"))

(defn %make-convex-hull-from-points ((wptr foreign-pointer)
                                     (points sequence)
                                     (tolerance number)
                                     (m4 foreign-pointer))
    convex-hull-geometry
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

(defn convex-geometry-calculate-inertia ((convex convex-hull-geometry))
    (values (simple-array single-float (3))
            (simple-array single-float (3)))
  "Calculate the three principal axis and the the values of the inertia matrix
   of a convex collision objects.
   Returns two vec3s:
   - the principal inertia.
   - the center of mass for the principal inertia."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-foreign-objects ((inertia3 :float 3)
                         (origin3 :float 3))
    (newtonconvexcollisioncalculateinertialmatrix
     (%geometry-ptr convex) inertia3 origin3)
    (values (ptr->v3 inertia3)
            (ptr->v3 origin3))))


(defun convex-geometry-calculate-volume (convex)
  (newtonconvexcollisioncalculatevolume (%geometry-ptr convex)))

;; (defun convex-geometry-calculate-buoyancy-acceleration (convex
;;                                                         matrix4
;;                                                         gravity-vec3
;;                                                         fluid-density
;;                                                         fluid-linear-viscosity
;;                                                         )
;;   (newtonconvexcollisioncalculatebuoyancyacceleration
;;    (%geometry-ptr convex)
;;     ))

;; (newtonconvexhullgetfaceindices)

;;------------------------------------------------------------

;; newtoncollisionaggregateaddbody
;; newtoncollisionaggregatecreate
;; newtoncollisionaggregatedestroy
;; newtoncollisionaggregategetselfcollision
;; newtoncollisionaggregateremovebody
;; newtoncollisionaggregatesetselfcollision

;;------------------------------------------------------------

(defn make-geometry-tree ((world world)
                          (faces/mesh/filename (or sequence
                                                   mesh
                                                   string
                                                   pathname))
                          &key
                          (optimize t t))
    geometry-tree
  "Create an empty complex collision geometry tree. Tree-geometry is
   the preferred geometry within Newton for collision with polygonal meshes
   of arbitrary complexity. The mesh must be made of flat
   non-intersecting polygons, but they do not explicitly need to be
   triangles."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((tree-ptr (newtoncreatetreecollision (%world-ptr world) 0))
        (src faces/mesh/filename))
    (etypecase src
      (sequence (%make-geom-tree-from-seq tree-ptr src (not (null optimize))))
      (mesh (%make-geom-tree-from-mesh tree-ptr src))
      ((or string pathname) (%deserialize-geom-tree tree-ptr src)))
    (%make-geometry-tree :ptr tree-ptr)))

(defn %make-geom-tree-from-seq ((tree-ptr foreign-pointer)
                                (src sequence)
                                (optimize boolean))
    null
  ;; Add an individual polygon to a TreeCollision. After the call to
  ;; NewtonTreeCollisionBeginBuild the TreeCollision is ready to accept
  ;; polygons. The application should iterate through the application's
  ;; mesh, adding the mesh polygons to the TreeCollision one at a time. The
  ;; polygons must be flat and non-self intersecting.
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtontreecollisionbeginbuild tree-ptr)
  (labels ((add-face (f)
             (let ((len (length f)))
               (with-foreign-object (d :float (* 3 len))
                 (loop :for v :in f :for i :from 0 :do
                    (setf (mem-aref d :float (+ (* i 3) 0)) (v:x v)
                          (mem-aref d :float (+ (* i 3) 1)) (v:y v)
                          (mem-aref d :float (+ (* i 3) 2)) (v:z v)))
                 (newtontreecollisionaddface
                  tree-ptr len d +vec3-size+ 0)))))
    (map nil #'add-face src))
  (newtontreecollisionendbuild tree-ptr (if optimize 1 0))
  nil)

(defn %make-geom-tree-from-mesh ((tree-ptr foreign-pointer)
                                 (src mesh))
    null
  (declare (ignore tree-ptr src))
  (error "Not implemented")
  nil)

(defn %deserialize-geom-tree ((tree-ptr foreign-pointer)
                              (src (or string pathname)))
    null
  (declare (ignore tree-ptr src))
  (error "Not implemented")
  nil)

;; (defun tree-geometry-face-attribute (tree index)
;;   (newtontreecollisiongetfaceattribute ))
;; (newtontreecollisionsetfaceattribute)
;; (newtontreecollisiongetvertexlisttrianglelistinaabb)


(defn geometry-tree-ray-cast-callback ((tree geometry-tree))
    (or null geometry-tree-raycast-function)
  (%geometry-tree-raycast-callback tree))

(defn (setf geometry-tree-ray-cast-callback)
    ((callback (or null geometry-tree-raycast-function))
     (tree geometry-tree))
    (or geometry-tree-raycast-function null)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((cb (if callback
                (get-callback '%geom-tree-raycast-cb)
                (null-pointer))))
    (NewtonTreeCollisionSetUserRayCastCallback (%geometry-ptr tree) cb)
    (setf (%geometry-tree-raycast-callback tree) callback)))

(defn geometry-tree-debug-callback ((tree geometry-tree))
    (or null geometry-tree-raycast-function)
  (%geometry-tree-debug-callback tree))

(defn (setf geometry-tree-debug-callback)
    ((callback (or null geometry-tree-raycast-function))
     (tree geometry-tree))
    (or null geometry-tree-raycast-function)
  (let ((cb (if callback
                (get-callback '%geom-tree-debug-cb)
                (null-pointer))))
    (NewtonStaticCollisionSetDebugCallback (%geometry-ptr tree) cb)
    (setf (%geometry-tree-debug-callback tree) callback)))

;;------------------------------------------------------------

(defn free-geometry ((geometry geometry)) null
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtondestroycollision (%geometry-ptr geometry))
  nil)

;;------------------------------------------------------------

(defmacro with-geometry ((var geometry) &body body)
  (assert (symbolp var))
  `(let ((,var ,geometry))
     (assert (typep ,var 'geometry))
     (unwind-protect (progn ,@body)
       (free-geometry ,var))))

;;------------------------------------------------------------

(defn geometry-mode ((geometry geometry)) (signed-byte 32)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtoncollisiongetmode (%geometry-ptr geometry)))

(defn (setf geometry-mode) ((mode (signed-byte 32))
                            (geometry geometry))
    (signed-byte 32)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (assert (integerp mode))
  (newtoncollisionsetmode (%geometry-ptr geometry) mode)
  mode)


;;------------------------------------------------------------

(defn geometry-scale ((geometry geometry))
    (simple-array single-float (3))
  (with-foreign-objects ((x :float) (y :float) (z :float))
    (newtoncollisiongetscale (%geometry-ptr geometry) x y z)
    (v! (mem-aref x :float) (mem-aref y :float) (mem-aref z :float))))

(defn (setf geometry-scale) ((value (simple-array single-float (3)))
                             (geometry geometry))
    (simple-array single-float (3))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtoncollisionsetscale
   (%geometry-ptr geometry) (v:x value) (v:y value) (v:z value))
  value)

;;------------------------------------------------------------

(defn geometry-offset-matrix4 ((geometry geometry))
    (simple-array single-float (16))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-foreign-object (m4 :float 16)
    (newtoncollisiongetmatrix (%geometry-ptr geometry) m4)
    (ptr->m4 m4)))

(defn (setf geometry-offset-matrix4)
    ((mat4 (simple-array single-float (16)))
     (geometry geometry))
    (simple-array single-float (16))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-foreign-array (m4 mat4 '(:array :float 16))
    (newtoncollisionsetmatrix (%geometry-ptr geometry) m4)
    mat4))

;;------------------------------------------------------------

(defn %geometry-user-data ((geometry geometry)) foreign-pointer
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtoncollisiongetuserdata (%geometry-ptr geometry)))

(defn (setf %geometry-user-data) (ptr geometry) foreign-pointer
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtoncollisionsetuserdata (%geometry-ptr geometry) ptr)
  ptr)

(defn %geometry-user-data-1 ((geometry geometry)) foreign-pointer
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtoncollisiongetuserdata1 (%geometry-ptr geometry)))

(defn (setf %geometry-user-data-1) ((ptr foreign-pointer)
                                    (geometry geometry))
    foreign-pointer
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtoncollisionsetuserdata1 (%geometry-ptr geometry) ptr)
  ptr)

(defn %geometry-data-pointer ((geometry geometry)) foreign-pointer
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtoncollisiondatapointer (%geometry-ptr geometry)))

;;------------------------------------------------------------

(defn %geometry-user-id ((geometry geometry)) (unsigned-byte 32)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtoncollisiongetuserid (%geometry-ptr geometry)))

(defn (setf %geometry-user-id) ((id (unsigned-byte 32))
                                (geometry geometry))
    null
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtoncollisionsetuserid (%geometry-ptr geometry) id)
  nil)

;;------------------------------------------------------------

(defn convex-shape-p ((geometry geometry)) boolean
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (>= (newtoncollisionisconvexshape (%geometry-ptr geometry)) 0))

(defn convex-static-p ((geometry geometry)) boolean
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (>= (newtoncollisionisstaticshape (%geometry-ptr geometry)) 0))

;;------------------------------------------------------------

(defn %geometry-info ((geometry geometry)) t
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-foreign-object (info '(:struct newtoncollisioninforecord))
    (newtoncollisiongetinfo (%geometry-ptr geometry) info)
    (mem-aref info '(:struct newtoncollisioninforecord))))

;;------------------------------------------------------------

(defn make-height-field-geometry ((world world)
                                  (heights-2d-array)
                                  &optional
                                  (vertical-scale single-float 1f0)
                                  (horizontal-scale single-float 1f0)
                                  (diagonals symbol :lower-left->upper-right))
    height-field-geometry
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  ;; {TODO} what are shape-ids?
  (assert (and (typep heights-2d-array 'array)
               (= 2 (array-rank heights-2d-array))))
  (let* ((width (array-dimension heights-2d-array 0))
         (height (array-dimension heights-2d-array 1)))
    (with-foreign-array (arr heights-2d-array :float)
      (with-foreign-object (attributes :char (* width height))
        (%make-height-field
         :ptr (newtoncreateheightfieldcollision
               (%world-ptr world)
               width height
               (ecase diagonals
                 (:lower-left->upper-right 0)
                 (:lower-right->upper-left 1))
               32
               arr
               attributes
               (float vertical-scale)
               (float horizontal-scale)
               0))))))

(defn height-field-ray-cast-callback ((height-field height-field-geometry))
    (or null height-field-ray-cast-function)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (%height-field-ray-cast-callback height-field))

(defn (setf height-field-ray-cast-callback)
    ((callback (or null height-field-ray-cast-function))
     (height-field height-field-geometry))
    (or null height-field-ray-cast-function)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((cb (if callback
                (get-callback '%geom-height-field-raycast-cb)
                (null-pointer))))
    (NewtonHeightFieldSetUserRayCastCallback (%geometry-ptr height-field) cb)
    (setf (%height-field-ray-cast-callback height-field) callback)))

;;------------------------------------------------------------

(defn create-scene-geometry ((world world)
                             (shape-id (signed-byte 32)))
    scene-geometry
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  ;; {TODO} what are shape-ids?
  (%make-scene
   :ptr (newtoncreatescenecollision (%world-ptr world) shape-id)))

(defn-inline %scene-begin-add-remove ((scene scene-geometry)) null
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (newtonscenecollisionbeginaddremove (%geometry-ptr scene))
  nil)

(defn-inline %scene-end-add-remove ((scene scene-geometry)) null
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (newtonscenecollisionendaddremove (%geometry-ptr scene))
  nil)

(defmacro with-scene-add-remove (scene &body body)
  (with-gensyms (gscene)
    `(let ((,gscene ,scene))
       (%scene-begin-add-remove ,gscene)
       (unwind-protect (progn ,@body)
         (%scene-end-add-remove ,gscene)))))

(defn scene-add-sub-geometry ((scene scene-geometry)
                              (geometry geometry))
    foreign-pointer
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtonscenecollisionaddsubcollision
   (%geometry-ptr scene)
   (%geometry-ptr geometry)))

(defn %scene-first-node ((scene scene-geometry)) foreign-pointer
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;; What is a collision-node? look like just a ptr
  (newtonscenecollisiongetfirstnode (%geometry-ptr scene)))

(defn %scene-next-node ((scene scene-geometry)
                        (node foreign-pointer))
    foreign-pointer
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  ;; What is a collision-node? look like just a ptr
  (newtonscenecollisiongetnextnode
   (%geometry-ptr scene)
   node))

(defmacro do-scene-nodes ((var-name scene) &body body)
  ;; hidden is just so the user can't fuck the process by setting
  ;; var-name to something else
  (with-gensyms (gscene hidden)
    `(let* ((,gscene ,scene)
            (,hidden (%scene-first-node ,gscene)))
       (loop :until (null-pointer-p ,hidden) :do
          (let ((,var-name ,hidden))
            ,@body
            (setf ,hidden (%scene-next-node ,gscene ,hidden)))))))

(defn scene-from-node-get-geometry ((scene scene-geometry)
                                    (node foreign-pointer))
    geometry
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (%geom-ptr->geom
   (newtonscenecollisiongetcollisionfromnode
    (%geometry-ptr scene)
    node)))

(defn scene-node-index ((scene scene-geometry)
                        (node foreign-pointer))
    (signed-byte 32)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtonscenecollisiongetnodeindex
   (%geometry-ptr scene)
   node))

(defn scene-node ((scene scene-geometry)
                  (index (signed-byte 32)))
    foreign-pointer
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtonscenecollisiongetnodebyindex
   (%geometry-ptr scene)
   index))

(defn scene-remove-sub-geometry-by-index ((scene scene-geometry)
                                          (index (signed-byte 32)))
    null
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtonscenecollisionremovesubcollisionbyindex
   (%geometry-ptr scene)
   index)
  nil)

(defn scene-remove-sub-geometry ((scene scene-geometry)
                                 (node foreign-pointer))
    null
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtonscenecollisionremovesubcollision
   (%geometry-ptr scene)
   node)
  nil)

(defn scene-set-sub-geometry-matrix4 ((scene scene-geometry)
                                      (node foreign-pointer)
                                      (mat4 (simple-array single-float (16))))
    null
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-foreign-array (m4 mat4 '(:array :float 16))
    (newtonscenecollisionsetsubcollisionmatrix
     (%geometry-ptr scene)
     node
     m4))
  nil)

;;------------------------------------------------------------
;; COMPOUND COLLISION

(defn make-compound-geometry ((world world)
                              (shape-id (signed-byte 32)))
    compound-geometry
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  "Create a container to hold an array of convex collision
   primitives. Compound collision primitives can only be made of convex
   collision primitives and they can not contain compound
   collision. Therefore they are treated as convex primitives.

   Compound collision primitives are treated as instance collision
   objects that can not shared by multiples rigid bodies."
  ;; {TODO} What is a shape-id
  (%make-compound-geometry
   :ptr (newtoncreatecompoundcollision
         (%world-ptr world) shape-id)))

(defn compound-add-sub-geometry ((compound compound-geometry)
                                 (convex convex-hull-geometry))
    foreign-pointer
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (NewtonCompoundCollisionAddSubCollision
   (%geometry-ptr compound)
   (%geometry-ptr convex)))

(defn compound-node-index ((compound compound-geometry)
                           (node foreign-pointer))
    (signed-byte 32)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (NewtonCompoundCollisionGetNodeIndex
   (%geometry-ptr compound)
   node))

(defn compound-remove-sub-geometry-by-index ((compound compound-geometry)
                                             (index (signed-byte 32)))
    null
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (NewtonCompoundCollisionRemoveSubCollisionByIndex
   (%geometry-ptr compound)
   index)
  nil)

(defn compound-remove-sub-geometry ((compound compound-geometry)
                                    (node foreign-pointer))
    null
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (NewtonCompoundCollisionRemoveSubCollision
   (%geometry-ptr compound)
   node)
  nil)

(defn-inline %compound-begin-add-remove ((compound compound-geometry))
    null
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (newtoncompoundcollisionbeginaddremove (%geometry-ptr compound))
  nil)

(defn-inline %compound-end-add-remove ((compound compound-geometry))
    null
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (newtoncompoundcollisionendaddremove (%geometry-ptr compound))
  nil)

(defmacro with-compound-add-remove (compound &body body)
  (with-gensyms (gcompound)
    `(let ((,gcompound ,compound))
       (%compound-begin-add-remove ,gcompound)
       (unwind-protect (progn ,@body)
         (%compound-end-add-remove ,gcompound)))))

(defn compound-from-node-get-geometry ((compound compound-geometry)
                                       (node foreign-pointer))
    geometry
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (%geom-ptr->geom
   (newtoncompoundcollisiongetcollisionfromnode
    (%geometry-ptr compound)
    node)))

(defn %compound-first-node ((compound compound-geometry))
    foreign-pointer
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;; What is a collision-node? look like just a ptr
  (newtoncompoundcollisiongetfirstnode (%geometry-ptr compound)))

(defn %compound-next-node ((compound compound-geometry)
                           (node foreign-pointer))
    foreign-pointer
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  ;; What is a collision-node? look like just a ptr
  (newtoncompoundcollisiongetnextnode
   (%geometry-ptr compound)
   node))

(defmacro do-compound-nodes ((var-name compound) &body body)
  ;; hidden is just so the user can't fuck the process by setting
  ;; var-name to something else
  (with-gensyms (gcompound hidden)
    `(let* ((,gcompound ,compound)
            (,hidden (%compound-first-node ,gcompound)))
       (loop :until (null-pointer-p ,hidden) :do
          (let ((,var-name ,hidden))
            ,@body
            (setf ,hidden (%compound-next-node ,gcompound ,hidden)))))))


(defn compound-node ((compound compound-geometry)
                     (index (signed-byte 32)))
    foreign-pointer
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtoncompoundcollisiongetnodebyindex
   (%geometry-ptr compound)
   index))

(defn compound-set-sub-geometry-matrix4
    ((compound compound-geometry)
     (node foreign-pointer)
     (mat4 (simple-array single-float (16))))
    null
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-foreign-array (m4 mat4 '(:array :float 16))
    (newtoncompoundcollisionsetsubcollisionmatrix
     (%geometry-ptr compound)
     node
     m4))
  nil)

;; newtonfracturedcompoundcollisiongetvertexnormals
;; newtonfracturedcompoundcollisiongetvertexpositions
;; newtonfracturedcompoundcollisiongetvertexuvs
;; newtonfracturedcompoundgetfirstsubmesh
;; newtonfracturedcompoundgetmainmesh
;; newtonfracturedcompoundgetnextsubmesh
;; newtonfracturedcompoundisnodefreetodetach
;; newtonfracturedcompoundmeshpart
;; newtonfracturedcompoundmeshpartgetfirstsegment
;; newtonfracturedcompoundmeshpartgetindexcount
;; newtonfracturedcompoundmeshpartgetindexstream
;; newtonfracturedcompoundmeshpartgetmaterial
;; newtonfracturedcompoundmeshpartgetnextsegment
;; newtonfracturedcompoundneighbornodelist
;; newtonfracturedcompoundplaneclip
;; newtonfracturedcompoundsetcallbacks

;; types/callbacks
;; newtoncompoundcollisionparam
;; newtonfracturecompoundcollisiononemitchunk
;; newtonfracturecompoundcollisiononemitcompoundfractured

;;------------------------------------------------------------

(defn map-polygons-in-geometry
    ((function geometry-iterator-function)
     (geometry geometry)
     &optional
     (mat4 (simple-array single-float (16)) (m4:identity)))
    null
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (assert function)
  (with-foreign-array (m4 mat4 '(:array :float 16))
    (let ((cb (get-callback '%geometry-iterator-cb))
          (geom-ptr (%geometry-ptr geometry)))
      (setf (%geometry-iterator-callback geometry) function)
      (unwind-protect
           (NewtonCollisionForEachPolygonDo geom-ptr m4 cb geom-ptr)
        (setf (%geometry-iterator-callback geometry) nil))))
  nil)

;;------------------------------------------------------------

(defn geometry-calculate-aabb
    ((geometry geometry)
     &optional
     (offset-matrix4 (simple-array single-float (16)) (m4:identity)))
    null
  "Calculate an axis-aligned bounding box for this collision,
   the box is calculated relative to `offset-matrix4`"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-foreign-objects ((p0 :float 3)
                         (p1 :float 3))
    (with-foreign-array (m4 offset-matrix4 '(:array :float 16))
      (newtoncollisioncalculateaabb
       (%geometry-ptr geometry) m4 p0 p1)
      (values (ptr->v3 p0) (ptr->v3 p1))))
  nil)

(defn geometry-get-parent ((geometry geometry)) geometry
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (%geom-ptr->geom
   (newtoncollisiongetparentinstance
    (%geometry-ptr geometry))))

(defn geometry-skin-thickness ((geometry geometry)) single-float
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (NewtonCollisionGetSkinThickness (%geometry-ptr geometry)))

(defn sub-geometry-handle ((geometry geometry)) geometry
  (%geom-ptr->geom
   (newtoncollisiongetsubcollisionhandle
    (%geometry-ptr geometry))))

(defn geometry-type ((geometry geometry)) (signed-byte 32)
  ;; {TODO} what is type?
  (newtoncollisiongettype (%geometry-ptr geometry)))


(defn geometry-most-extreme-vertex ((geometry geometry)
                                    (dir3 (simple-array single-float (3))))
    (simple-array single-float (3))
  "Calculate the most extreme point of a convex collision shape along the given
   direction"
  (with-foreign-array (d3 dir3 '(:array :float 3))
    (with-foreign-object (vpos3 :float 3)
      (newtoncollisionsupportvertex
       (%geometry-ptr geometry) d3 vpos3)
      (ptr->v3 vpos3))))

;; types/callbacks
;; newtonusermeshcollisioncontinuousoverlaptest
;; newtoncollisioninforecord
;; newtoncollisiontreeparam
