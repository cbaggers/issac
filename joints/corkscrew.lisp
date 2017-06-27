(in-package :issac)

;;------------------------------------------------------------

(defun make-corkscrew (world parent-body child-body pivot-point-v3 pin-dir-v3)
  (with-foreign-array (piv3 pivot-point-v3 '(:array :float 3))
    (with-foreign-array (dir3 pin-dir-v3 '(:array :float 3))
      (newtonconstraintcreatecorkscrew
       (%world-ptr world) piv3 dir3
       (%body-ptr child-body)
       (%body-ptr parent-body)))))

;;------------------------------------------------------------

(defun corkscrew-force (corkscrew)
  (with-foreign-object (v3 :float 3)
    (newtoncorkscrewgetjointforce (%joint-ptr corkscrew) v3)
    (ptr->v3 v3)))

(defun corkscrew-omega (corkscrew)
  (newtoncorkscrewgetjointomega (%joint-ptr corkscrew)))

(defun corkscrew-position (corkscrew)
  (newtoncorkscrewgetjointposit (%joint-ptr corkscrew)))

(defun corkscrew-velocity (corkscrew)
  (newtoncorkscrewgetjointveloc (%joint-ptr corkscrew)))

;;------------------------------------------------------------

(defun corkscrew-calc-stop-acceleration (corkscrew
                                         acceleration
                                         min-friction
                                         max-friction
                                         timestep
                                         position)
  "Calculate the the relative linear acceleration needed to stop the
   corkscrew at the desired angle. this function can only be called from
   a NewtonCorkscrewCallback and it can be used by the application to
   implement corkscrew limits."
  (with-foreign-object (desc 'newtonhingesliderupdatedesc)
    (with-foreign-slots ((m-accel m-minfriction m-maxfriction m-timestep)
                         desc newtonhingesliderupdatedesc)
      (setf m-accel acceleration
            m-minfriction min-friction
            m-maxfriction max-friction
            m-timestep timestep))
    (newtoncorkscrewcalculatestopaccel (%joint-ptr corkscrew) desc position)))

(defun corkscrew-calc-stop-alpha (corkscrew
                                  acceleration
                                  min-friction
                                  max-friction
                                  timestep
                                  angle)
  "Calculate the relative angular acceleration needed to stop the
   corkscrew at the desired angle. this function can only be called from
   a NewtonCorkscrewCallback and it can be used by the application to
   implement corkscrew limits."
  (with-foreign-object (desc 'newtonhingesliderupdatedesc)
    (with-foreign-slots ((m-accel m-minfriction m-maxfriction m-timestep)
                         desc newtonhingesliderupdatedesc)
      (setf m-accel acceleration
            m-minfriction min-friction
            m-maxfriction max-friction
            m-timestep timestep))
    (newtoncorkscrewcalculatestopalpha (%joint-ptr corkscrew) desc angle)))

;;------------------------------------------------------------

(defun corkscrew-callback (joint)
  (%corkscrew-callback joint))

(defun (setf corkscrew-callback) (callback joint)
  (let ((cb (if callback
                (get-callback '%corkscrew-cb)
                (null-pointer))))
    (NewtonCorkscrewSetUserCallback (%joint-ptr joint) cb)
    (setf (%corkscrew-callback joint) callback)))
