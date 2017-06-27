(in-package :issac)

;;------------------------------------------------------------

;; newtonconstraintcreateball
;; newtonballgetjointforce
;; newtonballgetjointomega
;; newtonballsetconelimits

(defun ball-&-socket-callback (joint)
  (%ball-&-socket-callback joint))

(defun (setf ball-&-socket-callback) (callback joint)
  (let ((cb (if callback
                (get-callback '%ball-cb)
                (null-pointer))))
    (NewtonBallSetUserCallback (%joint-ptr joint) cb)
    (setf (%ball-&-socket-callback joint) callback)))
