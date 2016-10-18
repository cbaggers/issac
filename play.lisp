(in-package :issac)

(defvar points
  (list (list (v! -100s0 0s0  100s0)
              (v! 100s0 0s0  100s0)
              (v! 100s0 0s0 -100s0)
              (v! -100s0 0s0 -100s0))))

(defcallback apply-gravity :void ((body :pointer) (timestep :float)
                                  (thread-index :int))
  (declare (ignore timestep thread-index))
  (multiple-value-bind (mass) (body-mass body)
    (setf (body-force body) (v! 0s0 (* -9.8 mass) 0s0 0s0)))

  (with-foreign-objects ((mass :float)
                         (ixx :float)
                         (iyy :float)
                         (izz :float))
    (NewtonBodyGetMass body mass Ixx Iyy Izz)
    (with-foreign-array (g (v! 0s0
                               (* -9.8 (mem-aref mass :float))
                               0s0
                               0s0)
                           '(:array :float 4))
      (NewtonBodySetForce body g))))

(defun create-background-body (world)
  (with-geometry (geom (make-geometry-tree world points))
    (make-body world geom)))

(defun create-freefall-ball (world)
  (with-geometry (geom (make-sphere-geometry world :radius 1s0))
    (let ((body (make-body world geom :linear-damping 0s0
                           :mass 1s0)))
      (%set-force-torque-callback body (cffi:callback apply-gravity)))))

(defvar world (make-world))
(defvar background-body (create-background-body world))
(defvar freefall-ball (create-freefall-ball world))

(defun newt ()
  (let ((step (/ 1s0 60)))
    (loop :for i :below 300 :do
       (newtonupdate world step)
       (with-foreign-object (m :float 16)
         (newtonbodygetmatrix freefall-ball m)
         (print (ptr->m4 m))))))

(defun ptr->m4 (ptr)
  (make-array
   16 :element-type 'single-float
   :initial-contents
   (loop :for i :below 16 :collect (mem-aref ptr :float i))))
