(in-package :issac)

;;------------------------------------------------------------

(deftclass (up-vector (:constructor %make-up-vector) (:include joint)))

newtonconstraintcreateupvector

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
