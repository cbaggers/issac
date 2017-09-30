(in-package :issac)

;;------------------------------------------------------------

(defn make-corkscrew ((world world)
                      (parent-body body)
                      (child-body body)
                      (pivot-point-v3 (simple-array single-float (3)))
                      (pin-dir-v3 (simple-array single-float (3))))
    corkscrew
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-foreign-array (piv3 pivot-point-v3 '(:array :float 3))
    (with-foreign-array (dir3 pin-dir-v3 '(:array :float 3))
      (let ((jnt (%make-corkscrew
                  :ptr (newtonconstraintcreatecorkscrew
                        (%world-ptr world) piv3 dir3
                        (%body-ptr child-body)
                        (%body-ptr parent-body)))))
        (setf (%joint-user-data jnt) (make-pointer (%add-joint-to-system jnt)))
        jnt))))

;;------------------------------------------------------------

(defn corkscrew-force ((corkscrew corkscrew))
    (simple-array single-float (3))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-foreign-object (v3 :float 3)
    (newtoncorkscrewgetjointforce (%joint-ptr corkscrew) v3)
    (ptr->v3 v3)))

(defn corkscrew-omega ((corkscrew corkscrew)) single-float
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtoncorkscrewgetjointomega (%joint-ptr corkscrew)))

(defn corkscrew-position ((corkscrew corkscrew)) single-float
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtoncorkscrewgetjointposit (%joint-ptr corkscrew)))

(defn corkscrew-velocity ((corkscrew corkscrew)) single-float
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtoncorkscrewgetjointveloc (%joint-ptr corkscrew)))

;;------------------------------------------------------------

(defn corkscrew-calc-stop-acceleration ((corkscrew corkscrew)
                                        (acceleration single-float)
                                        (min-friction single-float)
                                        (max-friction single-float)
                                        (timestep single-float)
                                        (position single-float))
    single-float
  "Calculate the the relative linear acceleration needed to stop the
   corkscrew at the desired angle. this function can only be called from
   a NewtonCorkscrewCallback and it can be used by the application to
   implement corkscrew limits."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-foreign-object (desc 'newtonhingesliderupdatedesc)
    (with-foreign-slots ((m-accel m-minfriction m-maxfriction m-timestep)
                         desc newtonhingesliderupdatedesc)
      (setf m-accel acceleration
            m-minfriction min-friction
            m-maxfriction max-friction
            m-timestep timestep))
    (newtoncorkscrewcalculatestopaccel (%joint-ptr corkscrew) desc position)))

(defn corkscrew-calc-stop-alpha ((corkscrew corkscrew)
                                 (acceleration single-float)
                                 (min-friction single-float)
                                 (max-friction single-float)
                                 (timestep single-float)
                                 (angle single-float))
    single-float
  "Calculate the relative angular acceleration needed to stop the
   corkscrew at the desired angle. this function can only be called from
   a NewtonCorkscrewCallback and it can be used by the application to
   implement corkscrew limits."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-foreign-object (desc 'newtonhingesliderupdatedesc)
    (with-foreign-slots ((m-accel m-minfriction m-maxfriction m-timestep)
                         desc newtonhingesliderupdatedesc)
      (setf m-accel acceleration
            m-minfriction min-friction
            m-maxfriction max-friction
            m-timestep timestep))
    (newtoncorkscrewcalculatestopalpha (%joint-ptr corkscrew) desc angle)))

;;------------------------------------------------------------

(defn corkscrew-callback ((joint corkscrew))
    (or null corkscrew-cb-function)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (%corkscrew-callback joint))

(defn (setf corkscrew-callback)
    ((callback (or null corkscrew-cb-function))
     (joint corkscrew))
    (or null corkscrew-cb-function)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((cb (if callback
                (get-callback '%corkscrew-cb)
                (null-pointer))))
    (NewtonCorkscrewSetUserCallback (%joint-ptr joint) cb)
    (setf (%corkscrew-callback joint) callback)))
