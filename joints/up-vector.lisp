(in-package :issac)

;;------------------------------------------------------------

(defun make-up-vector (world body pin-dir-v3)
  (with-foreign-array (dir3 pin-dir-v3 '(:array :float 3))
    (newtonconstraintcreateupvector
     (%world-ptr world) dir3
     (%body-ptr body))))

;;------------------------------------------------------------

(defun up-vector-pin (body)
  "Get the global angular velocity (omega) of the body"
  (with-foreign-object (v3 :float 3)
    (newtonupvectorgetpin (%body-ptr body) v3)
    (ptr->v3 v3)))

(defun (setf up-vector-pin) (value body)
  "Get the global angular velocity (omega) of the body"
  (with-foreign-array (v3 value '(:array :float 3))
    (newtonupvectorsetpin (%body-ptr body) v3))
  value)
