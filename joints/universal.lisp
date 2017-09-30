(in-package :issac)

;;------------------------------------------------------------

(defn make-universal ((world world)
                      (parent-body body)
                      (child-body body)
                      (pivot-point-v3 (simple-array single-float (3)))
                      (pin0-dir-v3 (simple-array single-float (3)))
                      (pin1-dir-v3 (simple-array single-float (3))))
    universal-joint
  (with-foreign-array (piv3 pivot-point-v3 '(:array :float 3))
    (with-foreign-array (dir0 pin0-dir-v3 '(:array :float 3))
      (with-foreign-array (dir1 pin1-dir-v3 '(:array :float 3))
        (let ((jnt (%make-universal-joint
                    :ptr (newtonconstraintcreateuniversal
                          (%world-ptr world) piv3 dir0 dir1
                          (%body-ptr child-body)
                          (%body-ptr parent-body)))))
          (setf (%joint-user-data jnt) (make-pointer (%add-joint-to-system jnt)))
          jnt)))))

;;------------------------------------------------------------

(defn universal-joint-force ((universal-joint universal-joint))
    (simple-array single-float (3))
  (with-foreign-object (v3 :float 3)
    (newtonuniversalgetjointforce (%joint-ptr universal-joint) v3)
    (ptr->v3 v3)))

(defn universal-joint-angle (universal-joint) single-float
  (newtonuniversalgetjointangle1 (%joint-ptr universal-joint)))

(defn universal-joint-omega-0 (universal-joint) single-float
  (newtonuniversalgetjointomega0 (%joint-ptr universal-joint)))

(defn universal-joint-omega-1 (universal-joint) single-float
  (newtonuniversalgetjointomega1 (%joint-ptr universal-joint)))

(defn universal-joint-omega ((universal-joint universal-joint))
    (simple-array single-float (2))
  (let ((ptr (%joint-ptr universal-joint)))
    (v2:make (newtonuniversalgetjointomega0 ptr)
             (newtonuniversalgetjointomega1 ptr))))

;;------------------------------------------------------------

(defn universal-joint-calc-stop-alpha-0 ((universal universal-joint)
                                         (acceleration single-float)
                                         (min-friction single-float)
                                         (max-friction single-float)
                                         (timestep single-float)
                                         (position single-float))
    single-float
  (with-foreign-object (desc 'newtonhingesliderupdatedesc)
    (with-foreign-slots ((m-accel m-minfriction m-maxfriction m-timestep)
                         desc newtonhingesliderupdatedesc)
      (setf m-accel acceleration
            m-minfriction min-friction
            m-maxfriction max-friction
            m-timestep timestep))
    (newtonuniversalcalculatestopalpha0 (%joint-ptr universal) desc position)))

(defn universal-joint-calc-stop-alpha-1 ((universal universal-joint)
                                         (acceleration single-float)
                                         (min-friction single-float)
                                         (max-friction single-float)
                                         (timestep single-float)
                                         (position single-float))
    single-float
  (with-foreign-object (desc 'newtonhingesliderupdatedesc)
    (with-foreign-slots ((m-accel m-minfriction m-maxfriction m-timestep)
                         desc newtonhingesliderupdatedesc)
      (setf m-accel acceleration
            m-minfriction min-friction
            m-maxfriction max-friction
            m-timestep timestep))
    (newtonuniversalcalculatestopalpha0 (%joint-ptr universal) desc position)))

;;------------------------------------------------------------

(defn universal-joint-callback ((joint universal-joint))
    (or null universal-joint-cb-function)
  (%universal-callback joint))

(defn (setf universal-joint-callback)
    ((callback (or null universal-joint-cb-function))
     (joint universal-joint))
  (or null universal-joint-cb-function)
  (let ((cb (if callback
                (get-callback '%universal-cb)
                (null-pointer))))
    (NewtonUniversalSetUserCallback (%joint-ptr joint) cb)
    (setf (%universal-callback joint) callback)))
