(in-package :issac)

;;------------------------------------------------------------

(defn make-hinge ((world world)
                  (pivot-point-v3 (simple-array single-float (3)))
                  (pin-dir-v3 (simple-array single-float (3)))
                  (child-body body)
                  (parent-body body))
    hinge
  (with-foreign-array (pp3 pivot-point-v3 '(:array :float 3))
    (with-foreign-array (pd3 pin-dir-v3 '(:array :float 3))
      (let ((jnt (%make-hinge
                  :ptr (newtonconstraintcreatehinge (%world-ptr world)
                                                    pp3
                                                    pd3
                                                    (%body-ptr child-body)
                                                    (%body-ptr parent-body)))))
        (setf (%joint-user-data jnt) (make-pointer (%add-joint-to-system jnt)))
        jnt))))

(defn hinge-calc-stop-alpha ((hinge hinge)
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
  (with-foreign-object (desc 'newtonhingesliderupdatedesc)
    (with-foreign-slots ((m-accel m-minfriction m-maxfriction m-timestep)
                         desc newtonhingesliderupdatedesc)
      (setf m-accel acceleration
            m-minfriction min-friction
            m-maxfriction max-friction
            m-timestep timestep))
    (newtonhingecalculatestopalpha (%joint-ptr hinge) desc angle)))

(defn hinge-force ((hinge hinge))
    (simple-array single-float (3))
  (with-foreign-object (v3 :float 3)
    (newtonhingegetjointforce (%joint-ptr hinge) v3)
    (ptr->v3 v3)))

(defn hinge-omega ((hinge hinge)) single-float
  (newtonhingegetjointomega (%joint-ptr hinge)))

;;------------------------------------------------------------

(defn hinge-callback ((hinge hinge))
    (or null hinge-cb-function)
  (%hinge-callback hinge))

(defn (setf hinge-callback)
    ((callback (or null hinge-cb-function))
     (hinge hinge))
    (or null hinge-cb-function)
  (let ((cb (if callback
                (get-callback '%hinge-cb)
                (null-pointer))))
    (NewtonHingeSetUserCallback (%joint-ptr hinge) cb)
    (setf (%hinge-callback hinge) callback)))

;;------------------------------------------------------------
