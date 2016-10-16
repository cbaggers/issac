(in-package :issac)

(defvar points
  (vector -100s0 0s0  100s0
          100s0 0s0  100s0
          100s0 0s0 -100s0
          -100s0 0s0 -100s0))

(defcallback apply-gravity :void ((body :pointer) (timestep :float)
                                  (thread-index :int))
  (declare (ignore timestep thread-index))
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
  (let ((collision (newtoncreatetreecollision world 0)))
    (with-foreign-array (pnts points '(:array :float 12))
      (newtontreecollisionbeginbuild collision)
      (newtontreecollisionaddface
       collision 4 pnts (* 3 (foreign-type-size :float)) 0)
      (newtontreecollisionendbuild collision 1)
      (with-foreign-array (mptr (m4:identity) '(:array :float 16))
        (prog1 (newtoncreatedynamicbody world collision mptr)
          (newtondestroycollision collision))))))

(defun create-freefall-ball (world)
  (let ((collision (NewtonCreateSphere world 1s0 0 (null-pointer)))
        (mass 1s0))
    (with-foreign-array (mptr (m4:translation (v! 0 50 0)) '(:array :float 16))
      (let ((body (newtoncreatedynamicbody world collision mptr)))
        (newtonbodysetforceandtorquecallback body (cffi:callback apply-gravity))
        (newtonbodysetmassproperties body mass collision)
        (newtonbodysetlineardamping body 0s0)
        (newtondestroycollision collision)
        body))))

(defvar world (let ((w (newtoncreate)))
                (newtoninvalidatecache w)
                w))
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
