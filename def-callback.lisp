(in-package :issac)

;;------------------------------------------------------------
;; Body

(defcallback %body-apply-force-and-torque
    :void ((body-ptr :pointer) (timestep :float) (thread-index :int))
  (declare (ignore thread-index))
  (let ((body (%body-ptr->body body-ptr)))
    (funcall (%body-force-torque-callback body)
             body
             timestep)))

(defcallback %body-destructor :void ((body-ptr :pointer))
  (let ((body (%body-ptr->body body-ptr)))
    (funcall (%body-destructor-callback body) body)))

(defcallback %body-transform
    :void ((body-ptr :pointer) (mat4 (:pointer :float)) (thread-index :int))
  (declare (ignore thread-index))
  (let ((body (%body-ptr->body body-ptr)))
    (funcall (%body-transform-callback body) body (ptr->m4 mat4))))

;;------------------------------------------------------------

;; newtoncollisioniterator
;; (void* const userData, int vertexCount, const dFloat* const faceArray, int faceId)
;; NewtonCollisionForEachPolygonDo
;; NewtonDeformableMeshSetDebugCallback

;;------------------------------------------------------------
;; newtonballcallback

(defcallback %ball-cb :void ((joint-ptr :pointer))
  (let ((joint (%joint-ptr->joint joint-ptr)))
    (funcall (%ball-&-socket-callback joint) joint)))

;;------------------------------------------------------------
;; newtoncorkscrewcallback

(defcallback %corkscrew-cb :void
    ((joint-ptr :pointer)
     (data (:pointer NewtonHingeSliderUpdateDesc)))
  (with-foreign-slots ((m-accel m-minFriction m-maxFriction m-timestep)
                       data NewtonHingeSliderUpdateDesc)
    (let ((joint (%joint-ptr->joint joint-ptr)))
      (funcall (%corkscrew-callback joint)
               joint m-accel m-maxFriction m-maxFriction m-timestep))))

;;------------------------------------------------------------
;; newtonslidercallback

(defcallback %slider-cb :void
    ((joint-ptr :pointer)
     (data (:pointer NewtonHingeSliderUpdateDesc)))
  (with-foreign-slots ((m-accel m-minFriction m-maxFriction m-timestep)
                       data NewtonHingeSliderUpdateDesc)
    (let ((joint (%joint-ptr->joint joint-ptr)))
      (funcall (%slider-callback joint)
               joint m-accel m-maxFriction m-maxFriction m-timestep))))

;;------------------------------------------------------------
;; newtonuniversalcallback

(defcallback %universal-cb :void
    ((joint-ptr :pointer)
     (data (:pointer NewtonHingeSliderUpdateDesc)))
  (with-foreign-slots ((m-accel m-minFriction m-maxFriction m-timestep)
                       data NewtonHingeSliderUpdateDesc)
    (let ((joint (%joint-ptr->joint joint-ptr)))
      (funcall (%universal-callback joint)
               joint m-accel m-maxFriction m-maxFriction m-timestep))))

;;------------------------------------------------------------
;; newtonhingecallback

(defcallback %hinge-cb :void
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
             timestep)))

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
               (foreign-string-to-lisp m-descriptiontype :count 128)))))

;;------------------------------------------------------------
;; world

;; newtonbodyiterator
;; we use the userdata to store the world-id
(defcallback %body-iterator-cb :int ((body-ptr :pointer) (user-data :pointer))
  (let ((body (%body-ptr->body body-ptr))
        (world (%world-by-id (pointer-address user-data))))
    (funcall (%world-body-iterator-callback world) body)))

;; newtoncollisioncopyconstructioncallback
(defcallback %geom-copy-construction-cb :void ((world-ptr :pointer)
                                               (geom :pointer)
                                               (src-geom :pointer))
  (let ((world (%world-from-world-ptr world-ptr))
        (geometry (%geom-ptr->geom geom))
        (src-geometry (%geom-ptr->geom src-geom)))
    (format t "{TODO} geometry copy construction ~s ~s ~s"
            world geometry src-geometry)))

;; newtoncollisiondestructorcallback
(defcallback %geom-copy-destruction-cb :void ((world-ptr :pointer)
                                              (geom :pointer))
  (let ((world (%world-from-world-ptr world-ptr))
        (geometry (%geom-ptr->geom geom)))
    (format t "{TODO} geometry destruction ~s ~s"
            world geometry)))


;; newtonworlddestroylistenercallback
(defcallback %world-destroy-listener-cb :void ((world-ptr :pointer)
                                               (user-data :pointer))
  (let ((world (%world-from-world-ptr world-ptr)))
    (format t "{TODO} geometry destruction ~s ~s"
            world user-data)))

;; newtonworlddestructorcallback
(defcallback %world-destructor-cb :void ((world-ptr :pointer))
  (let ((world (%world-from-world-ptr world-ptr)))
    (format t "{TODO} world destruction ~s" world)))

;; newtonworldlistenerbodydestroycallback
(defcallback %world-destroy-listener-cb :void ((world-ptr :pointer)
                                               (user-data :pointer)
                                               (body-ptr :pointer))
  (let ((world (%world-from-world-ptr world-ptr))
        (body (%body-ptr->body body-ptr)))
    (format t "{TODO} listener body destruction ~s ~s ~s"
            world user-data body)))


;; newtonworldrayfiltercallback
;; we use the userdata to store the world-id
(defcallback %world-ray-filter-cb :float ((body-ptr :pointer)
                                          (geom-ptr :pointer)
                                          (hit-contact :pointer)
                                          (hit-normal :pointer)
                                          (collision-id :long)
                                          (user-data :pointer)
                                          (intersect-param :float))
  (let ((world (%world-by-id (pointer-address user-data)))
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
(defcallback %world-ray-prefilter-cb :float ((body-ptr :pointer)
                                             (geom-ptr :pointer)
                                             (user-data :pointer))
  (let ((world (%world-by-id (pointer-address user-data)))
        (body (%body-ptr->body body-ptr))
        (shape-hit (%geom-ptr->geom geom-ptr)))
    (funcall (%world-ray-filter-callback world) body shape-hit)))

;; newtonworldupdatelistenercallback
(defcallback %world-update-listener-cb :void ((world-ptr :pointer)
                                                 (listener-user-data :pointer)
                                                 (timestep :float))
  (let ((world (%world-from-world-ptr world-ptr)))
    (funcall (%world-update-listener-callback world)
             world
             listener-user-data
             timestep)))

;; ;;------------------------------------------------------------
;; ;; callbacks

;; ;; geometry - tree
;; newtoncollisiontreeraycastcallback
;; ;; (const NewtonBody* const body, const NewtonCollision* const treeCollision, dFloat intersection, dFloat* const normal, int faceId, void* const usedData)
;; NewtonTreeCollisionSetUserRayCastCallback

;; newtontreecollisionfacecallback
;; ;; (void* const context, const dFloat* const polygon, int strideInBytes, const int* const indexArray, int indexCount)
;; ;; NewtonTreeCollisionForEachFace

;; newtontreecollisioncallback
;; ;; (const NewtonBody* const bodyWithTreeCollision, const NewtonBody* const body, int faceID, int vertexCount, const dFloat* const vertex, int vertexStrideInByte)
;; ;; NewtonStaticCollisionSetDebugCallback

;; ;; geometry - fracturedcompound
;; newtonfracturecompoundcollisionreconstructmainmeshcallback

;; ;; geometry - height-field
;; newtonheightfieldraycastcallback

;; ;; material
;; newtononaabboverlap
;; newtononcompoundsubcollisionaabboverlap
