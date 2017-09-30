(in-package :issac)

;;------------------------------------------------------------

;; {TODO} weird name
(defn make-up-vector ((world world)
                      (body body)
                      (pin-dir-v3 (simple-array single-float (3))))
    joint
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-foreign-array (dir3 pin-dir-v3 '(:array :float 3))
    (let ((jnt (%make-up-vector
                :ptr (newtonconstraintcreateupvector
                      (%world-ptr world) dir3
                      (%body-ptr body)))))
      (setf (%joint-user-data jnt) (make-pointer (%add-joint-to-system jnt)))
      jnt)))

;;------------------------------------------------------------

(defn up-vector-pin ((body body)) (simple-array single-float (3))
  "Get the global angular velocity (omega) of the body"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-foreign-object (v3 :float 3)
    (newtonupvectorgetpin (%body-ptr body) v3)
    (ptr->v3 v3)))

(defn (setf up-vector-pin) ((value (simple-array single-float (3)))
                            (body body))
    (simple-array single-float (3))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  "Get the global angular velocity (omega) of the body"
  (with-foreign-array (v3 value '(:array :float 3))
    (newtonupvectorsetpin (%body-ptr body) v3))
  value)
