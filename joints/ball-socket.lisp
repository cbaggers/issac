(in-package :issac)

;;------------------------------------------------------------

(defn make-ball-&-socket ((world world)
                          (pivot-point-v3 (simple-array single-float (3)))
                          (child-body body)
                          (parent-body body))
    ball-&-socket
  (with-foreign-array (v3 pivot-point-v3 '(:array :float 3))
    (let ((jnt (%make-ball-&-socket
                :ptr (newtonconstraintcreateball (%world-ptr world)
                                                 v3
                                                 (%body-ptr child-body)
                                                 (%body-ptr parent-body)))))
      (setf (%joint-user-data jnt) (make-pointer (%add-joint-to-system jnt)))
      jnt)))

(defn ball-&-socket-force ((ball-&-socket ball-&-socket))
    (simple-array single-float (3))
  (with-foreign-object (v3 :float 3)
    (newtonballgetjointforce (%joint-ptr ball-&-socket) v3)
    (ptr->v3 v3)))

(defn ball-&-socket-omega ((ball-&-socket ball-&-socket))
    (simple-array single-float (3))
  (with-foreign-object (v3 :float 3)
    (newtonballgetjointomega (%joint-ptr ball-&-socket) v3)
    (ptr->v3 v3)))

(defn ball-&-socket-set-cone-limits ((ball-&-socket ball-&-socket)
                                     (pin-v3 (simple-array single-float (3)))
                                     (max-cone-angle single-float)
                                     (max-twist-angle single-float))
    null
  (with-foreign-array (pin3 pin-v3 '(:array :float 3))
    (newtonballsetconelimits (%joint-ptr ball-&-socket)
                             pin3
                             max-cone-angle
                             max-twist-angle))
  nil)

(defn ball-&-socket-callback ((ball-&-socket ball-&-socket))
    (or null ball-&-socket-cb-function)
  (%ball-&-socket-callback ball-&-socket))

(defn (setf ball-&-socket-callback)
    ((callback (or null ball-&-socket-cb-function))
     (joint ball-&-socket))
    (or null ball-&-socket-cb-function)
  (let ((cb (if callback
                (get-callback '%ball-cb)
                (null-pointer))))
    (NewtonBallSetUserCallback (%joint-ptr joint) cb)
    (setf (%ball-&-socket-callback joint) callback)))
