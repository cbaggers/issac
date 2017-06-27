(in-package :issac)

;;------------------------------------------------------------

newtonconstraintcreatehinge
newtonhingecalculatestopalpha
newtonhingegetjointforce
newtonhingegetjointomega

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
