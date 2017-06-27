(in-package :issac)

;;------------------------------------------------------------

;; newtonconstraintcreatehinge
;; newtonhingecalculatestopalpha

(defun hinge-force (hinge)
  (with-foreign-object (v3 :float 3)
    (newtonhingegetjointforce (%joint-ptr hinge) v3)
    (ptr->v3 v3)))

(defun hinge-omega (hinge)
  (newtonhingegetjointomega (%joint-ptr hinge)))

;;------------------------------------------------------------

(defun hinge-callback (joint)
  (%hinge-callback joint))

(defun (setf hinge-callback) (callback joint)
  (let ((cb (if callback
                (get-callback '%hinge-cb)
                (null-pointer))))
    (NewtonHingeSetUserCallback (%joint-ptr joint) cb)
    (setf (%hinge-callback joint) callback)))

;;------------------------------------------------------------
