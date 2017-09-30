(in-package :issac)

;;------------------------------------------------------------

(defn make-slider ((world world)
                   (parent-body body)
                   (child-body body)
                   (pivot-point-v3 (simple-array single-float (3)))
                   (pin-dir-v3 (simple-array single-float (3))))
    slider
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-foreign-array (piv3 pivot-point-v3 '(:array :float 3))
    (with-foreign-array (dir3 pin-dir-v3 '(:array :float 3))
      (let ((jnt (%make-slider
                  :ptr (newtonconstraintcreateslider
                        (%world-ptr world) piv3 dir3
                        (%body-ptr child-body)
                        (%body-ptr parent-body)))))
        (setf (%joint-user-data jnt) (make-pointer (%add-joint-to-system jnt)))
        jnt))))

;;------------------------------------------------------------

(defn slider-position ((slider slider)) single-float
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtonslidergetjointposit (%joint-ptr slider)))

(defn slider-velocity ((slider slider)) single-float
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtonslidergetjointveloc (%joint-ptr slider)))

(defn slider-force ((slider slider)) (simple-array single-float (3))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-foreign-object (v3 :float 3)
    (newtonslidergetjointforce (%joint-ptr slider) v3)
    (ptr->v3 v3)))

;;------------------------------------------------------------

(defn slider-calc-stop-acceleration ((slider slider)
                                     (acceleration single-float)
                                     (min-friction single-float)
                                     (max-friction single-float)
                                     (timestep single-float)
                                     (position single-float))
    single-float
  "Calculate the the relative linear acceleration needed to stop the
   slider at the desired angle. this function can only be called from
   a NewtonSliderCallback and it can be used by the application to
   implement slider limits."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-foreign-object (desc 'newtonhingesliderupdatedesc)
    (with-foreign-slots ((m-accel m-minfriction m-maxfriction m-timestep)
                         desc newtonhingesliderupdatedesc)
      (setf m-accel acceleration
            m-minfriction min-friction
            m-maxfriction max-friction
            m-timestep timestep))
    (newtonslidercalculatestopaccel (%joint-ptr slider) desc position)))

;;------------------------------------------------------------

(defn slider-callback ((joint joint))
    (or null slider-cb-function)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (%slider-callback joint))

(defn (setf slider-callback) ((callback (or null slider-cb-function))
                              (joint slider))
    (or null slider-cb-function)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((cb (if callback
                (get-callback '%slider-cb)
                (null-pointer))))
    (NewtonSliderSetUserCallback (%joint-ptr joint) cb)
    (setf (%slider-callback joint) callback)))
