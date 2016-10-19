(in-package :issac)

;;------------------------------------------------------------
;; Kinds of callback

(defmacro def-callback-apply-force-and-torque
    (name (body-var &optional timestep-var) &body body)
  (with-gensyms (%body %timestep %thread-index)
    `(defcallback ,name :void
         ((,%body :pointer) (,%timestep :float) (,%thread-index :int))
       (declare (ignorable ,%timestep ,%thread-index))
       (let ((,body-var (%body-ptr->body ,%body))
             ,@(when timestep-var `((,timestep-var ,%timestep))))
         ,@body))))

;; ball

;; collision-copy-construction
;; collision-destructor
;; collision-tree-raycast

;; corkscrew

;; deserialize

;; fracture-compound-collision-reconstruct-main-mesh

;; get-time-in-microsenconds

;; heightfield-raycast

;; hinge

;; on-body-deserialization
;; on-body-serialization

;; on-joint-deserialization
;; on-joint-serialization
;; on-user-collision-serialization

;; serialize

;; slider

;; tree-collision
;; tree-collision-face

;; universal

;; user-bilateral
;; user-bilateral-get-info
;; user-mesh-collision-collide
;; user-mesh-collision-destroy
;; user-mesh-collision-rayhit

;; world-destroy-listener
;; world-destructor
;; world-listener-body-destroy
;; world-ray-filter
;; world-ray-prefilter
;; world-update-listener

;; joint-iterator
;; body-iterator
;; collision-iterator
;; collision-iterator

;; body-destructor
;; set-transform
