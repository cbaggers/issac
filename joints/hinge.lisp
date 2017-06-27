(in-package :issac)

;;------------------------------------------------------------

;; newtonconstraintcreatehinge

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
    (newtonhingecalculatestopalpha (%joint-ptr corkscrew) desc angle)))

(defun hinge-force (hinge)
  (with-foreign-object (v3 :float 3)
    (newtonhingegetjointforce (%joint-ptr hinge) v3)
    (ptr->v3 v3)))

(defun hinge-omega (hinge)
  (newtonhingegetjointomega (%joint-ptr hinge)))

;;------------------------------------------------------------

(defun hinge-callback (joint)
  (%hinge-callback joint))

(defun (setf hinge-callback) (callback joint)
  (let ((cb (if callback
                (get-callback '%hinge-cb)
                (null-pointer))))
    (NewtonHingeSetUserCallback (%joint-ptr joint) cb)
    (setf (%hinge-callback joint) callback)))

;;------------------------------------------------------------
