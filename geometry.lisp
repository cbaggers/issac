(in-package :issac)

;;------------------------------------------------------------

(defun %init-data (body)
  (setf (%geometry-user-data body) (make-pointer (%add-geom-to-system body)))
  body)

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
    (%init-data
     (%make-null
      :ptr (newtoncreatenull wptr)))))

(defun make-box-geometry (world &key (dimensions (v! 1s0 1s0 1s0))
                                  (offset-matrix4 (m4:identity)))
  "Create a box primitive for collision."
  (let ((wptr (%world-ptr world)))
    (with-foreign-array (m4 offset-matrix4 '(:array :float 16))
      (%init-data
       (%make-box
        :ptr (newtoncreatebox
              wptr (v:x dimensions) (v:y dimensions) (v:z dimensions) 0 m4))))))

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
      (%init-data
       (%make-sphere
        :ptr (newtoncreatesphere wptr (float radius) 0 m4))))))

(defun make-cone-geometry (world &key (radius 1s0) (height 1s0)
                                   (offset-matrix4 (m4:identity)))
  "Create a cone primitive for collision."
  (let ((wptr (%world-ptr world)))
    (with-foreign-array (m4 offset-matrix4 '(:array :float 16))
      (%init-data
       (%make-cone
        :ptr (newtoncreatecone wptr (float radius) (float height) 0  m4))))))

(defun make-capsule-geometry (world &key (radius-0 1s0) (radius-1 1s0)
                                      (height (* 2 (max radius-1 radius-0)))
                                      (offset-matrix4 (m4:identity)))
  "Create a capsule primitive for collision. the capsule height must
   equal of larger than the sum of the cap radius. If this is not the
   case the height will be clamped the 2 * radius."
  (let ((wptr (%world-ptr world)))
    (with-foreign-array (m4 offset-matrix4 '(:array :float 16))
      (%init-data
       (%make-capsule
        :ptr (newtoncreatecapsule
              wptr (float radius-0) (float radius-1) (float height) 0 m4))))))

(defun make-cylinder-geometry (world &key (radius-0 1s0) (radius-1 1s0)
                                       (height 4s0)
                                       (offset-matrix4 (m4:identity)))
  "Create a cylinder primitive for collision."
  (let ((wptr (%world-ptr world)))
    (with-foreign-array (m4 offset-matrix4 '(:array :float 16))
      (%init-data
       (%make-cylinder
        :ptr (newtoncreatecylinder
              wptr (float radius-0) (float radius-1) (float height) 0 m4))))))

(defun make-chamfer-cylinder-geometry (world &key (radius 1s0) (height 1s0)
                                               (offset-matrix4 (m4:identity)))
  "Create a ChamferCylinder primitive for collision."
  (let ((wptr (%world-ptr world)))
    (with-foreign-array (m4 offset-matrix4 '(:array :float 16))
      (%init-data
       (%make-chamfer-cylinder
        :ptr (newtoncreatechamfercylinder
              wptr (float radius) (float height) 0 m4))))))


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
      (%init-data
       (etypecase points/mesh
         (mesh
          (%make-convex-hull-from-mesh wptr points/mesh tolerance m4))
         (sequence
          (%make-convex-hull-from-points wptr points/mesh tolerance m4)))))))

(defun %make-convex-hull-from-mesh (wptr mesh tolerance m4)
  (declare (ignore wptr mesh tolerance m4))
  (error "Not implemented"))

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

(defun convex-geometry-calculate-inertia (convex)
  "Calculate the three principal axis and the the values of the inertia matrix
   of a convex collision objects.
   Returns two vec3s:
   - the principal inertia.
   - the center of mass for the principal inertia."
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

(defun make-geometry-tree (world faces/mesh/filename
                           &key (optimize t))
  "Create an empty complex collision geometry tree. TreeCollision* is
   the preferred method within Newton for collision with polygonal meshes
   of arbitrary complexity. The mesh must be made of flat
   non-intersecting polygons, but they do not explicitly need to be
   triangles. TreeCollision* can be serialized by the application to/from
   an arbitrary storage device."
  (let ((tree-ptr (newtoncreatetreecollision (%world-ptr world) 0))
        (src faces/mesh/filename))
    (etypecase src
      (sequence (%make-geom-tree-from-seq tree-ptr src optimize))
      (mesh (%make-geom-tree-from-mesh tree-ptr src))
      ((or string pathname) (%deserialize-geom-tree tree-ptr src)))
    (%make-geometry-tree :ptr tree-ptr)))

(defun %make-geom-tree-from-seq (tree-ptr src optimize)
  ;; Add an individual polygon to a TreeCollision. After the call to
  ;; NewtonTreeCollisionBeginBuild the TreeCollision is ready to accept
  ;; polygons. The application should iterate through the application's
  ;; mesh, adding the mesh polygons to the TreeCollision one at a time. The
  ;; polygons must be flat and non-self intersecting.
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
  (newtontreecollisionendbuild tree-ptr (if optimize 1 0)))

(defun %make-geom-tree-from-mesh (tree-ptr src)
  (declare (ignore tree-ptr src))
  (error "Not implemented"))

(defun %deserialize-geom-tree (tree-ptr src)
  (declare (ignore tree-ptr src))
  (error "Not implemented"))

;; (defun tree-geometry-face-attribute (tree index)
;;   (newtontreecollisiongetfaceattribute ))
;; (newtontreecollisionsetfaceattribute)
;; (newtontreecollisiongetvertexlisttrianglelistinaabb)

;;------------------------------------------------------------

(defun free-geometry (geometry)
  (newtondestroycollision (%geometry-ptr geometry))
  t)

;;------------------------------------------------------------

(defmacro with-geometry ((var geometry) &body body)
  (assert (symbolp var))
  `(let ((,var ,geometry))
     (assert (typep ,var 'geometry))
     (unwind-protect (progn ,@body)
       (free-geometry ,var))))

;;------------------------------------------------------------

(defun geometry-mode (geometry)
  (newtoncollisiongetmode (%geometry-ptr geometry)))

(defun (setf geometry-mode) (mode geometry)
  (assert (integerp mode))
  (newtoncollisionsetmode (%geometry-ptr geometry) mode))


;;------------------------------------------------------------

(defun geometry-scale (geometry)
  (with-foreign-objects ((x :float) (y :float) (z :float))
    (newtoncollisiongetscale (%geometry-ptr geometry) x y z)
    (v! (mem-aref x :float) (mem-aref y :float) (mem-aref z :float))))

(defun (setf geometry-scale) (value geometry)
  (newtoncollisionsetscale
   (%geometry-ptr geometry) (v:x value) (v:y value) (v:z value)))

;;------------------------------------------------------------

(defun geometry-offset-matrix4 (geometry)
  (with-foreign-object (m4 :float 16)
    (newtoncollisiongetmatrix (%geometry-ptr geometry) m4)
    (ptr->m4 m4)))

(defun (setf geometry-offset-matrix4) (mat4 geometry)
  (with-foreign-array (m4 mat4 '(:array :float 16))
    (newtoncollisionsetmatrix (%geometry-ptr geometry) m4)
    mat4))

;;------------------------------------------------------------

(defun %geometry-user-data (geometry)
  (newtoncollisiongetuserdata (%geometry-ptr geometry)))

(defun (setf %geometry-user-data) (ptr geometry)
  (newtoncollisionsetuserdata (%geometry-ptr geometry) ptr))

(defun %geometry-user-data-1 (geometry)
  (newtoncollisiongetuserdata1 (%geometry-ptr geometry)))

(defun (setf %geometry-user-data-1) (ptr geometry)
  (newtoncollisionsetuserdata1 (%geometry-ptr geometry) ptr))


(defun %geometry-data-pointer (geometry)
  (newtoncollisiondatapointer (%geometry-ptr geometry)))

;;------------------------------------------------------------

(defun %geometry-user-id (geometry)
  (newtoncollisiongetuserid (%geometry-ptr geometry)))

(defun (setf %geometry-user-id) (id geometry)
  (assert (typep id '(unsigned-byte 32)))
  (newtoncollisionsetuserid (%geometry-ptr geometry) id))

;;------------------------------------------------------------

(defun convex-shape-p (geometry)
  (>= (newtoncollisionisconvexshape (%geometry-ptr geometry)) 0))

(defun convex-static-p (geometry)
  (>= (newtoncollisionisstaticshape (%geometry-ptr geometry)) 0))

;;------------------------------------------------------------

(defun %geometry-info (geometry)
  (with-foreign-object (info '(:struct newtoncollisioninforecord))
    (newtoncollisiongetinfo (%geometry-ptr geometry) info)
    (mem-aref info '(:struct newtoncollisioninforecord))))

;;------------------------------------------------------------

(defun make-height-field-geometry (world heights-2d-array
                                   &optional
                                     (vertical-scale 1f0)
                                     (horizontal-scale 1f0)
                                     (diagonals :lower-left->upper-right))
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

;;------------------------------------------------------------

(defun create-scene-geometry (world shape-id)
  ;; {TODO} what are shape-ids?
  (%make-scene
   :ptr (newtoncreatescenecollision (%world-ptr world) shape-id)))

(defun %scene-begin-add-remove (scene)
  (newtonscenecollisionbeginaddremove (%geometry-ptr scene)))

(defun %scene-end-add-remove (scene)
  (newtonscenecollisionendaddremove (%geometry-ptr scene)))

(defmacro with-scene-add-remove (scene &body body)
  (with-gensyms (gscene)
    `(let ((,gscene ,scene))
       (%scene-begin-add-remove ,gscene)
       (unwind-protect (progn ,@body)
         (%scene-end-add-remove ,gscene)))))

(defun scene-add-sub-geometry (scene geometry)
  (newtonscenecollisionaddsubcollision
   (%geometry-ptr scene)
   (%geometry-ptr geometry)))

(defun %scene-first-node (scene)
  ;; What is a collision-node? look like just a ptr
  (newtonscenecollisiongetfirstnode (%geometry-ptr scene)))

(defun %scene-next-node (scene node)
  ;; What is a collision-node? look like just a ptr
  (newtonscenecollisiongetnextnode
   (%geometry-ptr scene)
   node))

(defmacro do-scene-nodes ((var-name scene) &body body)
  ;; hidden is just so the user can't fuck the process by setting
  ;; var-name to something else
  (with-gensyms (gscene hidden)
    `(let* ((,gscene ,scene)
            (,hidden (%scene-first-node ,gscene))
            (,var-name ,hidden))
       (loop :until (null-pointer-p ,hidden) :do
          (setf ,var-name ,hidden)
          (progn ,@body)
          (setf ,hidden (%scene-next-node ,gscene ,hidden)
                ,var-name ,hidden)))))

(defun scene-from-node-get-geometry (scene node)
  (%geom-ptr->geom
   (newtonscenecollisiongetcollisionfromnode
    (%geometry-ptr scene)
    node)))

(defun scene-node-index (scene node)
  (newtonscenecollisiongetnodeindex
   (%geometry-ptr scene)
   node))

(defun scene-node (scene index)
  (newtonscenecollisiongetnodebyindex
   (%geometry-ptr scene)
   index))

(defun scene-remove-sub-geometry-by-index (scene index)
  (newtonscenecollisionremovesubcollisionbyindex
   (%geometry-ptr scene)
   index))

(defun scene-remove-sub-geometry (scene node)
  (newtonscenecollisionremovesubcollision
   (%geometry-ptr scene)
   node))

(defun scene-set-sub-geometry-matrix4 (scene node mat4)
  (with-foreign-array (m4 mat4 '(:array :float 16))
    (newtonscenecollisionsetsubcollisionmatrix
     (%geometry-ptr scene)
     node
     m4)))

;;------------------------------------------------------------
;; COMPOUND COLLISION


(defun make-compound-geometry (world shape-id)
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

(defun compound-add-sub-geometry (compound convex)
  (NewtonCompoundCollisionAddSubCollision
   (%geometry-ptr compound)
   (%geometry-ptr convex)))

(defun compound-node-index (compound node)
  (NewtonCompoundCollisionGetNodeIndex
   (%geometry-ptr compound)
   node))

(defun compound-remove-sub-geometry-by-index (compound index)
  (NewtonCompoundCollisionRemoveSubCollisionByIndex
   (%geometry-ptr compound)
   index))

(defun compound-remove-sub-geometry (compound node)
  (NewtonCompoundCollisionRemoveSubCollision
   (%geometry-ptr compound)
   node))

(defun %compound-begin-add-remove (compound)
  (newtoncompoundcollisionbeginaddremove (%geometry-ptr compound)))

(defun %compound-end-add-remove (compound)
  (newtoncompoundcollisionendaddremove (%geometry-ptr compound)))

(defmacro with-compound-add-remove (compound &body body)
  (with-gensyms (gcompound)
    `(let ((,gcompound ,compound))
       (%compound-begin-add-remove ,gcompound)
       (unwind-protect (progn ,@body)
         (%compound-end-add-remove ,gcompound)))))

(defun compound-from-node-get-geometry (compound node)
  (%geom-ptr->geom
   (newtoncompoundcollisiongetcollisionfromnode
    (%geometry-ptr compound)
    node)))

(defun %compound-first-node (compound)
  ;; What is a collision-node? look like just a ptr
  (newtoncompoundcollisiongetfirstnode (%geometry-ptr compound)))

(defun %compound-next-node (compound node)
  ;; What is a collision-node? look like just a ptr
  (newtoncompoundcollisiongetnextnode
   (%geometry-ptr compound)
   node))

(defmacro do-compound-nodes ((var-name compound) &body body)
  ;; hidden is just so the user can't fuck the process by setting
  ;; var-name to something else
  (with-gensyms (gcompound hidden)
    `(let* ((,gcompound ,compound)
            (,hidden (%compound-first-node ,gcompound))
            (,var-name ,hidden))
       (loop :until (null-pointer-p ,hidden) :do
          (setf ,var-name ,hidden)
          (progn ,@body)
          (setf ,hidden (%compound-next-node ,gcompound ,hidden)
                ,var-name ,hidden)))))


(defun compound-node (compound index)
  (newtoncompoundcollisiongetnodebyindex
   (%geometry-ptr compound)
   index))

(defun compound-set-sub-geometry-matrix4 (compound node mat4)
  (with-foreign-array (m4 mat4 '(:array :float 16))
    (newtoncompoundcollisionsetsubcollisionmatrix
     (%geometry-ptr compound)
     node
     m4)))

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
;; newtononcompoundsubcollisionaabboverlap

;;------------------------------------------------------------

(defun geometry-calculate-aabb (geometry
                                &optional (offset-matrix4 (m4:identity)))
  "Calculate an axis-aligned bounding box for this collision,
   the box is calculated relative to `offset-matrix4`"
  (with-foreign-objects ((p0 :float 3)
                         (p1 :float 3))
    (with-foreign-array (m4 offset-matrix4 '(:array :float 16))
      (newtoncollisioncalculateaabb
       (%geometry-ptr geometry) m4 p0 p1)
      (values (ptr->v3 p0) (ptr->v3 p1)))))

(defun geometry-get-parent (geometry)
  (newtoncollisiongetparentinstance (%geometry-ptr geometry)))

(defun geometry-skin-thickness (geometry)
  (NewtonCollisionGetSkinThickness (%geometry-ptr geometry)))

(defun sub-geometry-handle (geometry)
  (%geom-ptr->geom
   (newtoncollisiongetsubcollisionhandle (%geometry-ptr geometry))))

(defun geometry-type (geometry)
  ;; {TODO} what is type?
  (newtoncollisiongettype (%geometry-ptr geometry)))


(defun geometry-most-extreme-vertex (geometry dir3)
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
;; newtoncollisioniterator
;; newtoncollisiontreeparam
