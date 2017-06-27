(in-package :issac)

;;------------------------------------------------------------

(defun make-universal (world parent-body child-body pivot-point-v3
                       pin0-dir-v3 pin1-dir-v3)
  (with-foreign-array (piv3 pivot-point-v3 '(:array :float 3))
    (with-foreign-array (dir0 pin0-dir-v3 '(:array :float 3))
      (with-foreign-array (dir1 pin1-dir-v3 '(:array :float 3))
        (newtonconstraintcreateuniversal
         (%world-ptr world) piv3 dir0 dir1
         (%body-ptr child-body)
         (%body-ptr parent-body))))))

;;------------------------------------------------------------

(defun universal-joint-force (universal-joint)
  (with-foreign-object (v3 :float 3)
    (newtonuniversalgetjointforce (%joint-ptr universal-joint) v3)
    (ptr->v3 v3)))

(defun universal-joint-angle (universal-joint)
  (newtonuniversalgetjointangle1 (%joint-ptr universal-joint)))

(defun universal-joint-omega-0 (universal-joint)
  (newtonuniversalgetjointomega0 (%joint-ptr universal-joint)))

(defun universal-joint-omega-1 (universal-joint)
  (newtonuniversalgetjointomega1 (%joint-ptr universal-joint)))

(defun universal-joint-omega (universal-joint)
  (let ((ptr (%joint-ptr universal-joint)))
    (v! (newtonuniversalgetjointomega0 ptr)
        (newtonuniversalgetjointomega1 ptr))))

;;------------------------------------------------------------

(defun universal-joint-calc-stop-alpha-0 (universal
                                          acceleration
                                          min-friction
                                          max-friction
                                          timestep
                                          position)
  (with-foreign-object (desc 'newtonhingesliderupdatedesc)
    (with-foreign-slots ((m-accel m-minfriction m-maxfriction m-timestep)
                         desc newtonhingesliderupdatedesc)
      (setf m-accel acceleration
            m-minfriction min-friction
            m-maxfriction max-friction
            m-timestep timestep))
    (newtonuniversalcalculatestopalpha0 (%joint-ptr universal) desc position)))

(defun universal-joint-calc-stop-alpha-1 (universal
                                          acceleration
                                          min-friction
                                          max-friction
                                          timestep
                                          position)
  (with-foreign-object (desc 'newtonhingesliderupdatedesc)
    (with-foreign-slots ((m-accel m-minfriction m-maxfriction m-timestep)
                         desc newtonhingesliderupdatedesc)
      (setf m-accel acceleration
            m-minfriction min-friction
            m-maxfriction max-friction
            m-timestep timestep))
    (newtonuniversalcalculatestopalpha0 (%joint-ptr universal) desc position)))

;;------------------------------------------------------------

(defun universal-joint-callback (joint)
  (%universal-callback joint))

(defun (setf universal-joint-callback) (callback joint)
  (let ((cb (if callback
                (get-callback '%universal-cb)
                (null-pointer))))
    (NewtonUniversalSetUserCallback (%joint-ptr joint) cb)
    (setf (%universal-callback joint) callback)))
