(in-package :issac)

;;------------------------------------------------------------

(defun make-slider (world parent-body child-body pivot-point-v3 pin-dir-v3)
  (with-foreign-array (piv3 pivot-point-v3 '(:array :float 3))
    (with-foreign-array (dir3 pin-dir-v3 '(:array :float 3))
      (newtonconstraintcreateslider
       (%world-ptr world) piv3 dir3
       (%body-ptr child-body)
       (%body-ptr parent-body)))))

;;------------------------------------------------------------

(defun slider-position (slider)
  (newtonslidergetjointposit (%joint-ptr slider)))

(defun slider-velocity (slider)
  (newtonslidergetjointveloc (%joint-ptr slider)))

(defun slider-force (slider)
  (with-foreign-object (v3 :float 3)
    (newtonslidergetjointforce (%joint-ptr slider) v3)
    (ptr->v3 v3)))

;;------------------------------------------------------------

(defun slider-calc-stop-acceleration (slider
                                      acceleration
                                      min-friction
                                      max-friction
                                      timestep
                                      position)
  "Calculate the the relative linear acceleration needed to stop the
   slider at the desired angle. this function can only be called from
   a NewtonSliderCallback and it can be used by the application to
   implement slider limits."
  (with-foreign-object (desc 'newtonhingesliderupdatedesc)
    (with-foreign-slots ((m-accel m-minfriction m-maxfriction m-timestep)
                         desc newtonhingesliderupdatedesc)
      (setf m-accel acceleration
            m-minfriction min-friction
            m-maxfriction max-friction
            m-timestep timestep))
    (newtonslidercalculatestopaccel (%joint-ptr slider) desc position)))

;;------------------------------------------------------------

(defun slider-callback (joint)
  (%slider-callback joint))

(defun (setf slider-callback) (callback joint)
  (let ((cb (if callback
                (get-callback '%slider-cb)
                (null-pointer))))
    (NewtonSliderSetUserCallback (%joint-ptr joint) cb)
    (setf (%slider-callback joint) callback)))
