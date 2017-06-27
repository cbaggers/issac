(in-package :issac)

;;------------------------------------------------------------

;; newtonconstraintcreateball

(defun ball-&-socket-force (ball-&-socket)
  (with-foreign-object (v3 :float 3)
    (newtonballgetjointforce (%joint-ptr ball-&-socket) v3)
    (ptr->v3 v3)))

(defun ball-&-socket-omega (ball-&-socket)
  (with-foreign-object (v3 :float 3)
    (newtonballgetjointomega (%joint-ptr ball-&-socket) v3)
    (ptr->v3 v3)))

;; newtonballsetconelimits

(defun ball-&-socket-callback (joint)
  (%ball-&-socket-callback joint))

(defun (setf ball-&-socket-callback) (callback joint)
  (let ((cb (if callback
                (get-callback '%ball-cb)
                (null-pointer))))
    (NewtonBallSetUserCallback (%joint-ptr joint) cb)
    (setf (%ball-&-socket-callback joint) callback)))
