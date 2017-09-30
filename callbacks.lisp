(in-package :issac)

;;------------------------------------------------------------
;; Body

;; NewtonApplyForceAndTorque
(defcallback %body-apply-force-and-torque
    :void ((body-ptr :pointer) (timestep :float) (thread-index :int))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (declare (ignore thread-index))
  (let* ((body (%body-ptr->body body-ptr))
         (cb (%body-force-torque-callback body)))
    (when cb
      (funcall cb body timestep))
    (values)))

;; NewtonBodyDestructor
(defcallback %body-destructor :void ((body-ptr :pointer))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((body (%body-ptr->body body-ptr))
        (cb (%body-destructor-callback body)))
    (when cb
      (funcall cb body))
    (values)))

;; NewtonSetTransform
(defcallback %body-transform
    :void ((body-ptr :pointer) (mat4 (:pointer :float)) (thread-index :int))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (declare (ignore thread-index))
  (let* ((body (%body-ptr->body body-ptr))
         (cb (%body-transform-callback body)))
    (when cb
      (funcall cb body (ptr->m4 mat4)))
    (values)))

;;------------------------------------------------------------
;; newtonconstraintdestructor

(defcallback %joint-destructor-cb :void ((joint-ptr :pointer))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((joint (%joint-ptr->joint joint-ptr))
         (cb (%joint-destructor-callback joint)))
    (when cb
      (funcall cb joint))
    (values)))

;;------------------------------------------------------------
;; newtonballcallback

(defcallback %ball-cb :void ((joint-ptr :pointer))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((joint (%joint-ptr->joint joint-ptr))
         (cb (%ball-&-socket-callback joint)))
    (when cb
      (funcall cb joint))
    (values)))

;;------------------------------------------------------------
;; newtoncorkscrewcallback

(defcallback %corkscrew-cb :uint
    ((joint-ptr :pointer)
     (data (:pointer NewtonHingeSliderUpdateDesc)))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-foreign-slots ((m-accel m-minFriction m-maxFriction m-timestep)
                       data NewtonHingeSliderUpdateDesc)
    (let* ((joint (%joint-ptr->joint joint-ptr))
           (cb (%corkscrew-callback joint)))
      (when cb
        (funcall cb joint m-accel m-maxFriction m-maxFriction m-timestep)))))

;;------------------------------------------------------------
;; newtonslidercallback

(defcallback %slider-cb :uint
    ((joint-ptr :pointer)
     (data (:pointer NewtonHingeSliderUpdateDesc)))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-foreign-slots ((m-accel m-minFriction m-maxFriction m-timestep)
                       data NewtonHingeSliderUpdateDesc)
    (let* ((joint (%joint-ptr->joint joint-ptr))
           (cb (%slider-callback joint)))
      (when cb
        (funcall cb joint m-accel m-maxFriction m-maxFriction m-timestep)))))

;;------------------------------------------------------------
;; newtonuniversalcallback

(defcallback %universal-cb :uint
    ((joint-ptr :pointer)
     (data (:pointer NewtonHingeSliderUpdateDesc)))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-foreign-slots ((m-accel m-minFriction m-maxFriction m-timestep)
                       data NewtonHingeSliderUpdateDesc)
    (let* ((joint (%joint-ptr->joint joint-ptr))
           (cb (%universal-callback joint)))
      (when cb
        (funcall cb joint m-accel m-maxFriction m-maxFriction m-timestep)))))

;;------------------------------------------------------------
;; newtonhingecallback

(defcallback %hinge-cb :uint
    ((joint-ptr :pointer)
     (data (:pointer NewtonHingeSliderUpdateDesc)))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-foreign-slots ((m-accel m-minFriction m-maxFriction m-timestep)
                       data NewtonHingeSliderUpdateDesc)
    (let* ((joint (%joint-ptr->joint joint-ptr))
           (cb (%hinge-callback joint)))
      (when cb
        (funcall cb joint m-accel m-maxFriction m-maxFriction m-timestep)))))

;;------------------------------------------------------------
;; newtonuserbilateralcallback

(defcallback %bilateral-cb
    :void ((joint-ptr :pointer) (timestep :float) (thread-index :int))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (declare (ignore thread-index))
  (let* ((joint (%joint-ptr->joint joint-ptr))
         (cb (%bilateral-callback joint)))
    (when cb
      (funcall cb joint timestep))
    (values)))

;; newtonuserbilateralgetinfocallback
(defcallback %bilateral-info-cb :void
    ((joint-ptr :pointer) (data (:pointer NewtonJointRecord)))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
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
    (let* ((joint (%joint-ptr->joint joint-ptr))
           (cb (%bilateral-info-callback joint)))
      (when cb
        (funcall cb
                 joint
                 (make-joint-info
                  :attachment-0-body (%body-ptr->body m-attachbody-0)
                  :attachment-1-body (%body-ptr->body m-attachbody-1)
                  :attachment-0-mat4 (ptr->m4 m-attachbody-0)
                  :attachment-1-mat4 (ptr->m4 m-attachbody-1)
                  :min-linear-dof (ptr->v3 m-minlineardof)
                  :max-linear-dof (ptr->v3 m-maxlineardof)
                  :min-angular-dof (ptr->v3 m-minangulardof)
                  :max-angular-dof (ptr->v3 m-maxangulardof)
                  :extra-params (foreign-array-to-lisp
                                 m-extraparameters '(:array :float 64))
                  :bodies-collision-on (= 1 m-bodiescollisionon)
                  :description (foreign-string-to-lisp m-descriptiontype :count 128))))
      (values))))

;;------------------------------------------------------------
;; world

;; newtonbodyiterator
;; we use the userdata to store the world-ptr
(defcallback %body-iterator-cb :int ((body-ptr :pointer) (user-data :pointer))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((body (%body-ptr->body body-ptr))
         (world (%world-from-world-ptr user-data))
         (cb (%world-body-iterator-callback world)))
    (when cb
      (funcall cb body))))

;; newtoncollisioncopyconstructioncallback
(defcallback %world-geom-constructor-cb :void ((world-ptr :pointer)
                                               (geom :pointer)
                                               (src-geom :pointer))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((world (%world-from-world-ptr world-ptr))
         (geometry (%geom-ptr->geom geom))
         (src-geometry (%geom-ptr->geom src-geom))
         (cb (%world-geom-constructor-callback world)))
    (when cb
      (funcall cb world geometry src-geometry))
    (values)))

;; newtoncollisiondestructorcallback
(defcallback %world-geom-destruction-cb :void ((world-ptr :pointer)
                                               (geom :pointer))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((world (%world-from-world-ptr world-ptr))
        (geometry (%geom-ptr->geom geom))
        (cb (%world-geom-destructor-callback world)))
    (when cb
      (funcall cb world geometry))
    (values)))


;; newtonworlddestroylistenercallback
(defcallback %world-pre-destroy-listener-cb :void ((world-ptr :pointer)
                                                   (user-data :pointer))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((world (%world-from-world-ptr world-ptr)))
    (format t "{TODO} geometry destruction ~s ~s"
            world user-data)
    (values)))

(defcallback %world-post-destroy-listener-cb :void ((world-ptr :pointer)
                                                    (user-data :pointer))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((world (%world-from-world-ptr world-ptr)))
    (format t "{TODO} geometry destruction ~s ~s"
            world user-data)
    (values)))

;; newtonworldupdatelistenercallback
(defcallback %world-pre-update-listener-cb :void ((world-ptr :pointer)
                                                  (listener-user-data :pointer)
                                                  (timestep :float))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((world (%world-from-world-ptr world-ptr))
         (cb (%world-pre-update-listener-callback world)))
    (when cb
      (funcall cb
               world
               listener-user-data
               timestep))
    (values)))

(defcallback %world-post-update-listener-cb :void ((world-ptr :pointer)
                                                   (listener-user-data :pointer)
                                                   (timestep :float))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((world (%world-from-world-ptr world-ptr))
         (cb (%world-post-update-listener-callback world)))
    (when cb
      (funcall cb
               world
               listener-user-data
               timestep))
    (values)))

;; newtonworlddestructorcallback
(defcallback %world-destructor-cb :void ((world-ptr :pointer))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((world (%world-from-world-ptr world-ptr)))
    (format t "{TODO} world destruction ~s" world)
    (values)))

;; newtonworldlistenerbodydestroycallback
(defcallback %world-destroy-listener-cb :void ((world-ptr :pointer)
                                               (user-data :pointer)
                                               (body-ptr :pointer))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
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
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((world (%world-from-world-ptr user-data))
         (body (%body-ptr->body body-ptr))
         (shape-hit (%geom-ptr->geom geom-ptr))
         (hit-contact (ptr->v3 hit-contact))
         (hit-normal (ptr->v3 hit-normal))
         (cb (%world-ray-filter-callback world)))
    (when cb
      (funcall cb
               body
               shape-hit
               hit-contact
               hit-normal
               collision-id
               intersect-param))))

;; newtonworldrayprefiltercallback
(defcallback %world-ray-prefilter-cb :uint ((body-ptr :pointer)
                                            (geom-ptr :pointer)
                                            (user-data :pointer))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((world (%world-by-id (pointer-address user-data)))
        (body (%body-ptr->body body-ptr))
        (shape-hit (%geom-ptr->geom geom-ptr))
        (cb (%world-ray-prefilter-callback world)))
    (when cb
      (funcall cb body shape-hit))))

;;------------------------------------------------------------
;; Geometry

;; newtoncollisioniterator
;; we us the user-data to store the geom-ptr
(defcallback %geometry-iterator-cb :void ((user-ptr :pointer)
                                          (vertex-count :int)
                                          (face-arr :pointer)
                                          (face-id :int))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((geom (%geom-ptr->geom user-ptr))
         (arr (make-array vertex-count :element-type 'single-float))
         (cb (%geometry-iterator-callback geom)))
    (loop :for i :below vertex-count :do
       (setf (aref arr i) (mem-aref face-arr :float i)))
    (when cb
      (funcall cb geom arr face-id))
    (values)))


;; newtoncollisiontreeraycastcallback
(defcallback %geom-tree-raycast-cb :float ((body-ptr :pointer)
                                           (tree-ptr :pointer)
                                           (intersection :float)
                                           (normal :pointer)
                                           (face-id :int)
                                           (used-data :pointer))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (declare (ignore used-data))
  (let* ((body (%body-ptr->body body-ptr))
         (geom (%geom-ptr->geom tree-ptr))
         (normal (ptr->v3 normal))
         (cb (%geometry-tree-raycast-callback geom)))
    (when cb
      (funcall cb
               geom
               body
               intersection
               normal
               face-id))))

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
(defcallback %geom-tree-debug-cb :void ((tree-ptr :pointer)
                                        (body-ptr :pointer)
                                        (face-id :int)
                                        (vertex-count :int)
                                        (vertex-arr :pointer)
                                        (stride-in-bytes :int))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((tree (%geom-ptr->geom tree-ptr))
         (body (%body-ptr->body body-ptr))
         (arr (make-array vertex-count :element-type 'single-float))
         (cb (%geometry-tree-debug-callback tree)))
    (loop :for i :below vertex-count :do
       (setf (aref arr i) (mem-aref vertex-arr :float i)))
    (when cb
      (funcall cb
               tree
               body
               face-id
               arr
               stride-in-bytes))
    (values)))

;; newtonfracturecompoundcollisionreconstructmainmeshcallback
;; need to work out NewtonFracturedCompoundMeshPart first
;; (NewtonBody* const body,
;;  NewtonFracturedCompoundMeshPart* mainMesh,
;;  NewtonCollision* fracturedCompountCollision)


;; newtonheightfieldraycastcallback
(defcallback %geom-height-field-raycast-cb :float
    ((body-ptr :pointer)
     (height-field-ptr :pointer)
     (intersection :float)
     (row :int)
     (col :int)
     (normal :pointer)
     (face-id :int)
     (user-data :pointer))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (declare (ignore user-data))
  (let* ((hfield (%geom-ptr->geom height-field-ptr))
         (body (%body-ptr->body body-ptr))
         (cb (%height-field-ray-cast-callback hfield)))
    (when cb
      (funcall cb
               hfield
               body
               intersection
               row
               col
               (ptr->v3 normal)
               face-id))))

;;------------------------------------------------------------
;; material

;; newtononaabboverlap
(defcallback %geom-height-field-raycat-cb :int
    ((material-ptr :pointer)
     (body0-ptr :pointer)
     (body1-ptr :pointer)
     (thread-index :int))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (declare (ignore thread-index))
  (let* ((material (%material-pair-ptr->material-pair material-ptr))
         (body0 (%body-ptr->body body0-ptr))
         (body1 (%body-ptr->body body1-ptr))
         (cb (%material-pair-aabb-overlap-callback material)))
    (when cb
      (funcall cb
               material
               body0
               body1))))

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
