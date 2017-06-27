(in-package :issac)

;;------------------------------------------------------------
;; Body

;; NewtonApplyForceAndTorque
(defcallback %body-apply-force-and-torque
    :void ((body-ptr :pointer) (timestep :float) (thread-index :int))
  (declare (ignore thread-index))
  (let ((body (%body-ptr->body body-ptr)))
    (funcall (%body-force-torque-callback body)
             body
             timestep)
    (values)))

;; NewtonBodyDestructor
(defcallback %body-destructor :void ((body-ptr :pointer))
  (let ((body (%body-ptr->body body-ptr)))
    (funcall (%body-destructor-callback body) body)
    (values)))

;; NewtonSetTransform
(defcallback %body-transform
    :void ((body-ptr :pointer) (mat4 (:pointer :float)) (thread-index :int))
  (declare (ignore thread-index))
  (let ((body (%body-ptr->body body-ptr)))
    (funcall (%body-transform-callback body) body (ptr->m4 mat4))
    (values)))

;;------------------------------------------------------------
;; newtonballcallback

(defcallback %ball-cb :void ((joint-ptr :pointer))
  (let ((joint (%joint-ptr->joint joint-ptr)))
    (funcall (%ball-&-socket-callback joint) joint)
    (values)))

;;------------------------------------------------------------
;; newtoncorkscrewcallback

(defcallback %corkscrew-cb :uint
    ((joint-ptr :pointer)
     (data (:pointer NewtonHingeSliderUpdateDesc)))
  (with-foreign-slots ((m-accel m-minFriction m-maxFriction m-timestep)
                       data NewtonHingeSliderUpdateDesc)
    (let ((joint (%joint-ptr->joint joint-ptr)))
      (funcall (%corkscrew-callback joint)
               joint m-accel m-maxFriction m-maxFriction m-timestep))))

;;------------------------------------------------------------
;; newtonslidercallback

(defcallback %slider-cb :uint
    ((joint-ptr :pointer)
     (data (:pointer NewtonHingeSliderUpdateDesc)))
  (with-foreign-slots ((m-accel m-minFriction m-maxFriction m-timestep)
                       data NewtonHingeSliderUpdateDesc)
    (let ((joint (%joint-ptr->joint joint-ptr)))
      (funcall (%slider-callback joint)
               joint m-accel m-maxFriction m-maxFriction m-timestep))))

;;------------------------------------------------------------
;; newtonuniversalcallback

(defcallback %universal-cb :uint
    ((joint-ptr :pointer)
     (data (:pointer NewtonHingeSliderUpdateDesc)))
  (with-foreign-slots ((m-accel m-minFriction m-maxFriction m-timestep)
                       data NewtonHingeSliderUpdateDesc)
    (let ((joint (%joint-ptr->joint joint-ptr)))
      (funcall (%universal-callback joint)
               joint m-accel m-maxFriction m-maxFriction m-timestep))))

;;------------------------------------------------------------
;; newtonhingecallback

(defcallback %hinge-cb :uint
    ((joint-ptr :pointer)
     (data (:pointer NewtonHingeSliderUpdateDesc)))
  (with-foreign-slots ((m-accel m-minFriction m-maxFriction m-timestep)
                       data NewtonHingeSliderUpdateDesc)
    (let ((joint (%joint-ptr->joint joint-ptr)))
      (funcall (%hinge-callback joint)
               joint m-accel m-maxFriction m-maxFriction m-timestep))))

;;------------------------------------------------------------
;; newtonuserbilateralcallback

(defcallback %bilateral-cb
    :void ((joint-ptr :pointer) (timestep :float) (thread-index :int))
  (declare (ignore thread-index))
  (let ((joint (%joint-ptr->joint joint-ptr)))
    (funcall (%bilateral-callback joint)
             joint
             timestep)
    (values)))

;; newtonuserbilateralgetinfocallback
(defcallback %bilateral-info-cb :void
    ((joint-ptr :pointer) (data (:pointer NewtonJointRecord)))
  (with-foreign-slots ((m-attachmenmatrix-0
                        m-attachmenmatrix-1
                        m-minlineardof
                        m-maxlineardof
                        m-minangulardof
                        m-maxangulardof
                        m-attachbody-0
                        m-attachbody-1
                        m-extraparameters
                        m-bodiescollisionon
                        m-descriptiontype)
                       data NewtonJointRecord)
    (let ((joint (%joint-ptr->joint joint-ptr)))
      (funcall (%hinge-callback joint)
               joint
               (ptr->m4 m-attachbody-0)
               (ptr->m4 m-attachbody-1)
               (ptr->v3 m-minlineardof)
               (ptr->v3 m-maxlineardof)
               (ptr->v3 m-minangulardof)
               (ptr->v3 m-maxangulardof)
               (%body-ptr->body m-attachbody-0)
               (%body-ptr->body m-attachbody-1)
               (foreign-array-to-lisp m-extraparameters '(:array :float 64))
               m-bodiescollisionon
               (foreign-string-to-lisp m-descriptiontype :count 128))
      (values))))

;;------------------------------------------------------------
;; world

;; newtonbodyiterator
;; we use the userdata to store the world-ptr
(defcallback %body-iterator-cb :int ((body-ptr :pointer) (user-data :pointer))
  (let ((body (%body-ptr->body body-ptr))
        (world (%world-from-world-ptr user-data)))
    (funcall (%world-body-iterator-callback world) body)))

;; newtoncollisioncopyconstructioncallback
(defcallback %geom-copy-construction-cb :void ((world-ptr :pointer)
                                               (geom :pointer)
                                               (src-geom :pointer))
  (let ((world (%world-from-world-ptr world-ptr))
        (geometry (%geom-ptr->geom geom))
        (src-geometry (%geom-ptr->geom src-geom)))
    (format t "{TODO} geometry copy construction ~s ~s ~s"
            world geometry src-geometry)
    (values)))

;; newtoncollisiondestructorcallback
(defcallback %geom-copy-destruction-cb :void ((world-ptr :pointer)
                                              (geom :pointer))
  (let ((world (%world-from-world-ptr world-ptr))
        (geometry (%geom-ptr->geom geom)))
    (format t "{TODO} geometry destruction ~s ~s"
            world geometry)
    (values)))


;; newtonworlddestroylistenercallback
(defcallback %world-destroy-listener-cb :void ((world-ptr :pointer)
                                               (user-data :pointer))
  (let ((world (%world-from-world-ptr world-ptr)))
    (format t "{TODO} geometry destruction ~s ~s"
            world user-data)
    (values)))

;; newtonworlddestructorcallback
(defcallback %world-destructor-cb :void ((world-ptr :pointer))
  (let ((world (%world-from-world-ptr world-ptr)))
    (format t "{TODO} world destruction ~s" world)
    (values)))

;; newtonworldlistenerbodydestroycallback
(defcallback %world-destroy-listener-cb :void ((world-ptr :pointer)
                                               (user-data :pointer)
                                               (body-ptr :pointer))
  (let ((world (%world-from-world-ptr world-ptr))
        (body (%body-ptr->body body-ptr)))
    (format t "{TODO} listener body destruction ~s ~s ~s"
            world user-data body)
    (values)))


;; newtonworldrayfiltercallback
;; we use the userdata to store the world-id
(defcallback %world-ray-filter-cb :float ((body-ptr :pointer)
                                          (geom-ptr :pointer)
                                          (hit-contact :pointer)
                                          (hit-normal :pointer)
                                          (collision-id :long)
                                          (user-data :pointer)
                                          (intersect-param :float))
  (let ((world (%world-from-world-ptr user-data))
        (body (%body-ptr->body body-ptr))
        (shape-hit (%geom-ptr->geom geom-ptr))
        (hit-contact (ptr->v3 hit-contact))
        (hit-normal (ptr->v3 hit-normal)))
    (funcall (%world-ray-filter-callback world)
             body
             shape-hit
             hit-contact
             hit-normal
             collision-id
             intersect-param)))

;; newtonworldrayprefiltercallback
(defcallback %world-ray-prefilter-cb :uint ((body-ptr :pointer)
                                            (geom-ptr :pointer)
                                            (user-data :pointer))
  (let ((world (%world-by-id (pointer-address user-data)))
        (body (%body-ptr->body body-ptr))
        (shape-hit (%geom-ptr->geom geom-ptr)))
    (funcall (%world-ray-prefilter-callback world) body shape-hit)))

;; newtonworldupdatelistenercallback
(defcallback %world-update-listener-cb :void ((world-ptr :pointer)
                                              (listener-user-data :pointer)
                                              (timestep :float))
  (let ((world (%world-from-world-ptr world-ptr)))
    (funcall (%world-update-listener-callback world)
             world
             listener-user-data
             timestep)
    (values)))

;;------------------------------------------------------------
;; Geometry

;; newtoncollisioniterator
;; we us the user-data to store the geom-ptr
(defcallback %geometry-iterator-cb :void ((user-ptr :pointer)
                                          (vertex-count :int)
                                          (face-arr :pointer)
                                          (face-id :int))
  (let ((geom (%geom-ptr->geom user-ptr))
        (arr (make-array vertex-count :element-type 'single-float)))
    (loop :for i :below vertex-count :do
       (setf (aref arr i) (mem-aref face-arr :float i)))
    (funcall (%geometry-iterator-callback geom)
             geom
             arr
             face-id)
    (values)))


;; newtoncollisiontreeraycastcallback
(defcallback %geom-tree-raycast-cb :float ((body-ptr :pointer)
                                           (tree-ptr :pointer)
                                           (intersection :float)
                                           (normal :pointer)
                                           (face-id :int)
                                           (used-data :pointer))
  (declare (ignore used-data))
  (let ((body (%body-ptr->body body-ptr))
        (geom (%geom-ptr->geom tree-ptr))
        (normal (ptr->v3 normal)))
    (funcall (%geometry-iterator-callback geom)
             geom
             body
             intersection
             normal
             face-id)))

;; newtontreecollisionfacecallback
;; I need more info to finish this one
;; (defcallback %geom-tree-face-cb :float ((context :pointer)
;;                                         (polygon :pointer)
;;                                         (stride-in-bytes :int)
;;                                         (index-arr :pointer)
;;                                         (index-count :int))
;;   (declare (ignore used-data))
;;   (let ((body (%body-ptr->body body-ptr))
;;         (geom (%geom-ptr->geom tree-ptr))
;;         (normal (ptr->v3 normal)))
;;     (funcall (%geometry-iterator-callback geom)
;;              geom
;;              body
;;              intersection
;;              normal
;;              face-id)))
;; (void* const context,
;;  const dFloat* const polygon,
;;  int strideInBytes,
;;  const int* const indexArray,
;;  int indexCount)

;; newtontreecollisioncallback

(defcallback %geom-tree-face-cb :void ((tree-ptr :pointer)
                                       (body-ptr :pointer)
                                       (face-id :int)
                                       (vertex-count :int)
                                       (vertex-arr :pointer)
                                        (stride-in-bytes :int))
  (let ((tree (%geom-ptr->geom tree-ptr))
        (body (%body-ptr->body body-ptr))
        (arr (make-array vertex-count :element-type 'single-float)))
    (loop :for i :below vertex-count :do
       (setf (aref arr i) (mem-aref vertex-arr :float i)))
    (funcall (%geometry-tree-face-callback tree)
             tree
             body
             face-id
             arr
             stride-in-bytes)
    (values)))

;; newtonfracturecompoundcollisionreconstructmainmeshcallback
;; need to work out NewtonFracturedCompoundMeshPart first
;; (NewtonBody* const body,
;;  NewtonFracturedCompoundMeshPart* mainMesh,
;;  NewtonCollision* fracturedCompountCollision)


;; newtonheightfieldraycastcallback
(defcallback %geom-height-field-raycat-cb :float
    ((body-ptr :pointer)
     (height-field-ptr :pointer)
     (intersection :float)
     (row :int)
     (col :int)
     (normal :pointer)
     (face-id :int)
     (user-data :pointer))
  (declare (ignore user-data))
  (let ((hfield (%geom-ptr->geom height-field-ptr))
        (body (%body-ptr->body body-ptr)))
    (funcall (%height-field-ray-cast-callback hfield)
             hfield
             body
             intersection
             row
             col
             (ptr->v3 normal)
             face-id)))

;;------------------------------------------------------------
;; material

;; newtononaabboverlap
(defcallback %geom-height-field-raycat-cb :int
    ((material-ptr :pointer)
     (body0-ptr :pointer)
     (body1-ptr :pointer)
     (thread-index :int))
  (declare (ignore thread-index))
  (let ((material (%material-pair-ptr->material-pair material-ptr))
        (body0 (%body-ptr->body body0-ptr))
        (body1 (%body-ptr->body body1-ptr)))
    (funcall (%material-pair-aabb-overlap-callback material)
             material
             body0
             body1)))

;; ;; newtononcompoundsubcollisionaabboverlap
;; seems to belong to world but not sure how to get the callback
;; (defcallback %geom-height-field-raycat-cb :float
;;     ((material-ptr :pointer)
;;      (node0-ptr :pointer)
;;      (body0-ptr :pointer)
;;      (node1-ptr :pointer)
;;      (body1-ptr :pointer)
;;      (thread-index :int))
;;   (declare (ignore thread-index))
;;   (let ((material (%material-pair-ptr->material-pair material-ptr))
;;         (body0 (%body-ptr->body body0-ptr))
;;         (body1 (%body-ptr->body body1-ptr)))
;;     (funcall (%material-pair-aabb-overlap-callback material)
;;              material
;;              node0-ptr
;;              body0
;;              node1-ptr
;;              body1
;;              thread-index)))
