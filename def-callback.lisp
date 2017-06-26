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

;; ;;------------------------------------------------------------

;; ;; callbacks

;; ;; joint - ball
;; newtonballcallback

;; ;; joint - corkscrew
;; newtoncorkscrewcallback

;; ;; joint - hinge
;; newtonhingecallback

;; ;; joint - slider
;; newtonslidercallback

;; ;; joint - universal
;; newtonuniversalcallback

;; ;; joint - bilateral

;; newtonuserbilateralcallback
;; newtonuserbilateralgetinfocallback

;; ;; world
;; newtonbodyiterator
;; newtoncollisioncopyconstructioncallback
;; newtoncollisiondestructorcallback
;; newtonworlddestroylistenercallback
;; newtonworlddestructorcallback
;; newtonworldlistenerbodydestroycallback
;; newtonworldrayfiltercallback
;; newtonworldrayprefiltercallback
;; newtonworldupdatelistenercallback

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


;; ;; Notes on user data & ids
;; ;;
;; ;; newtonbodysetuserdata
;; ;;
;; ;; newtoncollisionsetuserid
;; ;; newtoncollisiondatapointer
;; ;;
;; ;; newtonmaterialsetcallbackuserdata
;; ;;
;; ;; newtonworldsetuserdata
;; ;; newtonjointsetuserdata
;; ;;
;; ;; body world joint collision
