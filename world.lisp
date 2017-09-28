(in-package :issac)

;;------------------------------------------------------------

(defmethod print-object ((obj world) stream)
  (if (eq *null-world* obj)
      (format stream "#<NULL-WORLD>")
      (call-next-method)))

;;------------------------------------------------------------

(defn-inline %world-by-id ((id issac-id)) world
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (aref *worlds* id))

(defn ensure-world-pool-size ((len issac-id)) null
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (when (< (length *worlds*) len)
    (if (< (array-dimension *worlds* 0) len)
        (adjust-array
         *worlds* (+ len 20) :fill-pointer len :initial-element *null-world*)
        (setf (fill-pointer *worlds*) len)))
  nil)

(defn (setf %world-by-id) ((world world) (id issac-id)) world
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (ensure-world-pool-size (1+ id))
  (setf (aref *worlds* id) world))


;;------------------------------------------------------------

(defun make-world (&key (broadphase-algorithm :default)
                     (stack-size-mb :default)
                     (max-thread-count :default)
                     (solve-model :adaptive)
                     (friction-model :adaptive)
                     (minimum-frame-rate 60))
  "Create an instance of the Newton world.

   Specifying the stack size for each thread is only useful for
   simulation with very many (ie thousands or more objects). If in
   doubt, use :default as it uses reasonable defaults."
  (let* ((solve-model (validate-solver-model solve-model))
         (friction-model (validate-friction-model friction-model))
         (minimum-frame-rate (validate-min-frame-rate minimum-frame-rate))
         (world (world-invalidate-cache
                 (%make-world :ptr (if (eq stack-size-mb :default)
                                       (newtoncreate)
                                       (progn
                                         (assert (integerp stack-size-mb))
                                         (newtoncreateex stack-size-mb)))
                              :solve-model solve-model
                              :friction-model friction-model
                              :min-frame-rate minimum-frame-rate))))
    ;;
    ;; add to world table
    (setf (%world-by-id (%world-id world)) world)
    (newtonworldsetuserdata (%world-ptr world) (make-pointer (%world-id world)))
    ;;
    ;; set defaults
    (unless (eq broadphase-algorithm :default)
      (setf (world-broadphase-algorithm world) broadphase-algorithm))
    (unless (eq max-thread-count :default)
      (setf (world-max-thread-count world) max-thread-count))
    (setf (world-solver-model world) solve-model)
    (setf (world-friction-model world) friction-model)
    world))

;;------------------------------------------------------------

(defn-inline %world-stack-size ((world world)) (signed-byte 32)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (newtongetstacksize (%world-ptr world)))

(defn (setf %world-stack-size) ((value (signed-byte 32)) (world world))
    (signed-byte 32)
  (assert (> value 0))
  (newtonsetstacksize (%world-ptr world) value)
  value)

;; newtonsetperformanceclock

;;------------------------------------------------------------

(defn %world-from-world-ptr ((world-ptr cffi:foreign-pointer)) world
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((id (pointer-address (newtonworldgetuserdata world-ptr))))
    (%world-by-id id)))

;;------------------------------------------------------------

(defn free-world ((world world)) null
  "Destroy an instance of the Newton world. This function will destroy the entire Newton world."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtondestroy (%world-ptr world))
  nil)

(defn world-destroy-all-bodies ((world world)) null
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtondestroyallbodies (%world-ptr world))
  nil)

;;------------------------------------------------------------

(declaim (type single-float +1/60f0+))
(defconstant +1/60f0+ (/ 1f0 60f0))

(defn world-step ((world world) &optional (timestep single-float +1/60f0+))
    world
  "Advance the simulation by a user defined amount of time. This
   function will advance the simulation by the specified amount of
   time."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtonupdate (%world-ptr world) timestep)
  world)

(defn world-step-async ((world world)
                        &optional (timestep single-float +1/60f0+))
    world
  "Advance the simulation by a user defined amount of time. This
   function will advance the simulation by the specified amount of
   time."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtonupdateasync (%world-ptr world) timestep)
  world)


(defn world-wait-for-update-to-finish ((world world)) world
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtonwaitforupdatetofinish (%world-ptr world))
  world)

(defn world-last-update-time ((world world)) single-float
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtongetlastupdatetime (%world-ptr world)))

(defn world-substep-count ((world world)) (signed-byte 32)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtongetnumberofsubsteps (%world-ptr world)))

(defn (setf world-substep-count) ((value (signed-byte 32)) (world world))
    (signed-byte 32)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (assert (typep value '(unsigned-byte 16)))
  (newtonsetnumberofsubsteps (%world-ptr world) value)
  value)

;;------------------------------------------------------------

(defun validate-min-frame-rate (frame-rate)
  (assert (and (>= 60) (<= frame-rate 1000)
               (typep frame-rate '(unsigned-byte 16)))
          (frame-rate))
  frame-rate)

(defn world-minimum-frame-rate ((world world)) (unsigned-byte 16)
  "The minimum frame rate at which the simulation can run. The default
  minimum frame rate of the simulation is 60 frame per second. Newton
  will perform sub steps to meet the desired minimum FPS, should the
  frame rate drop below the specified minimum.
  This value is clamped between 60fps and 1000fps"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (%world-min-frame-rate world))

;; (defun (setf world-minimum-frame-rate) (frame-rate world)
;;   "The minimum frame rate at which the simulation can run. The default
;;   minimum frame rate of the simulation is 60 frame per second. Newton
;;   will perform sub steps to meet the desired minimum FPS, should the
;;   frame rate drop below the specified minimum.
;;   This value is clamped between 60fps and 1000fps"
;;   (let ((frame-rate (validate-min-frame-rate frame-rate)))
;;     (raw-bindings-newton:newton )
;;     (newtonsetminimumframerate (%world-ptr world) frame-rate)
;;     (setf (%world-min-frame-rate world) frame-rate)))


;;------------------------------------------------------------

(defn world-invalidate-cache ((world world)) world
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtoninvalidatecache (%world-ptr world))
  world)

;;------------------------------------------------------------

(defn world-current-device ((world world)) (signed-byte 32)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtongetcurrentdevice (%world-ptr world)))

(defn (setf world-current-device) ((value (signed-byte 32)) (world world))
    (signed-byte 32)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (assert (and (< value (%world-device-count world))
               (>= value 0)))
  (newtonsetcurrentdevice (%world-ptr world) value)
  value)

(defn world-devices ((world world)) list
  (loop :for i :below (%world-device-count world) :collect
     (list i (%device-string world i))))

(defun %device-string (world device-index)
  (with-foreign-object (cstr :char 100)
    (let ((len (newtongetdevicestring (%world-ptr world) device-index cstr 100)))
      (cffi:foreign-string-to-lisp cstr :count len))))

(defn %world-device-count ((world world)) (signed-byte 32)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtonenumeratedevices (%world-ptr world)))

;;------------------------------------------------------------

(declaim (type list *broadphase-algorithms*))
(defvar *broadphase-algorithms* '(:default :persintent))

(defn world-broadphase-algorithm ((world world)) keyword
  (elt *broadphase-algorithms*
       (newtongetbroadphasealgorithm (%world-ptr world))))

(defn (setf world-broadphase-algorithm) ((algorithm-id symbol) (world world))
    symbol
  (assert (member algorithm-id *broadphase-algorithms*) (algorithm-id))
  (newtonselectbroadphasealgorithm
   (%world-ptr world)
   (position algorithm-id *broadphase-algorithms*))
  algorithm-id)

;;------------------------------------------------------------

;; solver-model 0 = exact, 1 = adaptive, n = linear.

(defun valid-solver-model-p (model)
  (or (eq model :exact)
      (eq model :adaptive)
      (and (typep model '(unsigned-byte 8))
           (> model 1))))

(defun validate-solver-model (solver-model)
  (assert (valid-solver-model-p solver-model) (solver-model))
  solver-model)

(defun world-solver-model (world)
  ":exact Is the exact mode. This is good for application where precision
          is more important than speed, ex: realistic simulation.

   :adaptive Is the adaptive mode, the solver is not as exact but the
             simulation will still maintain a high degree of accuracy. This
             mode is good for applications were a good degree of stability is
             important but not as important as speed.

   <integer> means Linear mode. The solver will not try to reduce the
             joints relative acceleration errors to below some limit, instead
             it will perform up to n passes over the joint configuration each
             time reducing the acceleration error, but it will terminate when
             the number of passes is exhausted regardless of the error
             magnitude. In general this is the fastest mode and is is good
             for applications where speed is the only important factor, ex:
             video games.the adaptive friction model combined with the linear
             model make for the fastest possible configuration of the Newton
             solver. This setup is best for games. If you need the best
             realistic behavior, we recommend the use of the exact solver and
             exact friction model which are the defaults.

    See also: #'world-friction-model "
  (%world-solve-model world))

(defun (setf world-solver-model) (model world)
  (let ((model (validate-solver-model model)))
    (case model
      (:exact (newtonsetsolvermodel (%world-ptr world) 0))
      (:adaptive (newtonsetsolvermodel (%world-ptr world) 1))
      (otherwise (newtonsetsolvermodel (%world-ptr world) model)))
    (setf (%world-solve-model world) model)))

;;------------------------------------------------------------

(defn validate-friction-model ((friction-model symbol)) symbol
  (declare (optimize (speed 3) (debug 1) (safety 1)))
  (assert (or (eq friction-model :exact) (eq friction-model :adaptive))
          (friction-model))
  friction-model)

(defn world-friction-model ((world world)) symbol
  (declare (optimize (speed 3) (debug 1) (safety 1)))
  (%world-friction-model world))

(defn (setf world-friction-model) ((model symbol) (world world)) symbol
  (declare (optimize (speed 3) (debug 1) (safety 1)))
  "Set coulomb model of friction. This function allows the application
   to chose between and exact or an adaptive coulomb friction model

    :exact Friction forces are calculated in each frame. This model
           is good for applications where precision is more important than
           speed, ex: realistic simulation.

    :adaptive Is the adaptive model. Here values from previous frames
              are used to determine the maximum friction values of the current
              frame. This is about 10% faster than the exact model however it
              may introduce strange friction behaviors. For example a bouncing
              object tumbling down a ramp will act as a friction less object
              because the contacts do not have continuity. In general each time
              a new contact is generated the friction value is zero, only if the
              contact persist a non zero friction values is used. The second
              effect is that if a normal force is very strong, and if the
              contact is suddenly destroyed, a very strong friction force will
              be generated at the contact point making the object react in a
              non-familiar way.the adaptive friction model combined with the
              linear model make for the fastest possible configuration of the
              Newton solver. This setup is best for games. If you need the best
              realistic behavior, we recommend the use of the exact solver and
              exact friction model which are the defaults.

    See also: #'world-solver-model"
  (let ((model (validate-friction-model model)))
    (case model
      (:exact (newtonsetfrictionmodel (%world-ptr world) 0))
      (:adaptive (newtonsetfrictionmodel (%world-ptr world) 1)))
    (setf (%world-friction-model world) model)))

;;------------------------------------------------------------

(defn world-contact-merge-tolerance ((world world)) single-float
  (declare (optimize (speed 3) (debug 1) (safety 1)))
  (newtongetcontactmergetolerance (%world-ptr world)))

(defn (setf world-contact-merge-tolerance) ((value single-float) (world world))
    single-float
  (declare (optimize (speed 3) (debug 1) (safety 1)))
  (newtonsetcontactmergetolerance (%world-ptr world) value)
  value)

;;------------------------------------------------------------

;; newtonsetsolverconvergencequality

;;------------------------------------------------------------

(defn world-thread-count ((world world)) (signed-byte 32)
  (declare (optimize (speed 3) (debug 1) (safety 1)))
  (newtongetthreadscount (%world-ptr world)))

(defn world-max-thread-count ((world world)) (signed-byte 32)
  (declare (optimize (speed 3) (debug 1) (safety 1)))
  (newtongetmaxthreadscount (%world-ptr world)))

(defun (setf world-max-thread-count) (value world)
  (newtonsetthreadscount (%world-ptr world) value)
  value)

;; (defun world-dispatch-thread-job (world task)
;;   (newtondispachthreadjob world  )
;;     )

(defn world-sync-thread-jobs ((world world)) world
  (declare (optimize (speed 3) (debug 1) (safety 1)))
  (newtonsyncthreadjobs (%world-ptr world))
  world)

;; newtonjobtask

;; newtonyield

;;------------------------------------------------------------

(defn world-body-count ((world world)) (signed-byte 32)
  (newtonworldgetbodycount (%world-ptr world)))

(defn world-constraint-count ((world world)) (signed-byte 32)
  (newtonworldgetconstraintcount (%world-ptr world)))

;;------------------------------------------------------------

(defn %world-first-body ((world world)) foreign-pointer
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (newtonworldgetfirstbody (%world-ptr world)))

(defn %world-next-body ((world world) (body-ptr foreign-pointer))
    foreign-pointer
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (newtonworldgetnextbody (%world-ptr world) body-ptr))

(defmacro do-world-bodys ((var-name world) &body body)
  ;; hidden is just so the user can't fuck the process by setting
  ;; var-name to something else
  (with-gensyms (gworld hidden)
    `(let* ((,gworld ,world)
            (,hidden (%world-first-body ,gworld))
            (,var-name ,hidden))
       (loop :until (null-pointer-p ,hidden) :do
          (setf ,var-name (%body-ptr->body ,hidden))
          (progn ,@body)
          (setf ,hidden (%world-next-body ,gworld ,hidden)
                ,var-name ,hidden)))))

(defn %world-first-material-pair ((world world)) foreign-pointer
  (newtonworldgetfirstmaterial (%world-ptr world)))

(defn %world-next-material-pair ((world world)
                                 (material-pair-ptr foreign-pointer))
    foreign-pointer
  (newtonworldgetnextmaterial (%world-ptr world) material-pair-ptr))

(defmacro do-world-material-pairs ((var-name world) &body body)
  ;; hidden is just so the user can't fuck the process by setting
  ;; var-name to something else
  (with-gensyms (gworld hidden)
    `(let* ((,gworld ,world)
            (,hidden (%world-first-material-pair ,gworld)))
       (loop :until (null-pointer-p ,hidden) :do
          (let ((,var-name (%material-pair-ptr->material-pair ,hidden)))
            ,@body
            (setf ,hidden (%world-next-material-pair ,gworld ,hidden)))))))

(defn map-bodies-in-aabb ((function (or null body-iterator-function))
                          (world world)
                          (p0-v3 (simple-array single-float (3)))
                          (p1-v3 (simple-array single-float (3))))
    null
  (assert function)
  (with-foreign-array (p0 p0-v3 '(:array :float 3))
    (with-foreign-array (p1 p1-v3 '(:array :float 3))
      (let ((cb (get-callback '%body-iterator-cb))
            (world-ptr (%world-ptr world)))
        (setf (%world-body-iterator-callback world) function)
        (unwind-protect
             (newtonworldforeachbodyinaabbdo world-ptr p0 p1 cb world-ptr)
          (setf (%world-body-iterator-callback world) nil)))))
  nil)

;; newtonworldforeachjointdo

;;------------------------------------------------------------

;; newtonworldsetdestructorcallback
;; newtonworldgetdestructorcallback

;;------------------------------------------------------------

;; newtonworldsetcollisionconstructordestructorcallback

(defn world-geometry-constructor-callback ((world world))
    (or null (function (world body body) t))
  (declare (optimize (speed 3) (debug 1) (safety 1)))
  (%world-geom-constructor-callback world))

(defn (setf world-geometry-constructor-callback)
    ((callback (or null (function (world body body) t)))
     (world world))
    (or null (function (world body body) t))
  (declare (optimize (speed 3) (debug 1) (safety 1)))
  (let ((con (if callback
                 (get-callback '%world-geom-constructor-cb)
                 (null-pointer)))
        (des (if (%world-geom-destructor-callback world)
                 (get-callback '%world-geom-destruction-cb)
                 (null-pointer))))
    (newtonworldsetcollisionconstructordestructorcallback
     (%world-ptr world) con des)
    (setf (%world-geom-constructor-callback world) callback)))

(defn world-geometry-destructor-callback ((world world))
    (or null (function (world body) t))
  (declare (optimize (speed 3) (debug 1) (safety 1)))
  (%world-geom-destructor-callback world))

(defn (setf world-geometry-destructor-callback)
    ((callback (or null (function (world body) t)))
     (world world))
    (or null (function (world body) t))
  (declare (optimize (speed 3) (debug 1) (safety 1)))
  (let ((con (if (%world-geom-constructor-callback world)
                 (get-callback '%world-geom-destructor-cb)
                 (null-pointer)))
        (des (if callback
                 (get-callback '%world-geom-destruction-cb)
                 (null-pointer))))
    (newtonworldsetcollisionconstructordestructorcallback
     (%world-ptr world) con des)
    (setf (%world-geom-destructor-callback world) callback)))


;;------------------------------------------------------------

;; newtonworldgetprelistener
;; newtonworldaddprelistener

(defn world-pre-update-listener ((world world))
    (or null (function (world single-float) t))
  (declare (optimize (speed 3) (debug 1) (safety 1)))
  (%world-pre-update-listener-callback world))

(defn (setf world-pre-update-listener)
    ((callback (or null (function (world single-float) t)))
     (world world))
    (or null (function (world single-float) t))
  (declare (optimize (speed 3) (debug 1) (safety 1)))
  (let ((upd (if callback
                 (get-callback '%world-pre-update-listener-cb)
                 (null-pointer)))
        (des (if (%world-geom-constructor-callback world)
                 (get-callback '%world-pre-destroy-listener-cb)
                 (null-pointer))))
    (with-foreign-string (name "l")
      (NewtonWorldAddPreListener
       (%world-ptr world) name (null-pointer) upd des))
    (setf (%world-pre-update-listener-callback world) callback)))

(defn world-pre-destroy-listener ((world world))
    (or null (function (world) t))
  (declare (optimize (speed 3) (debug 1) (safety 1)))
  (%world-pre-destroy-listener-callback world))

(defn (setf world-pre-destroy-listener)
    ((callback (or null (function (world) t)))
     (world world))
    (or null (function (world) t))
  (declare (optimize (speed 3) (debug 1) (safety 1)))
  (let ((upd (if callback
                 (get-callback '%world-pre-destroy-listener-cb)
                 (null-pointer)))
        (des (if (%world-geom-constructor-callback world)
                 (get-callback '%world-pre-destroy-listener-cb)
                 (null-pointer))))
    (with-foreign-string (name "l")
      (NewtonWorldAddPreListener
       (%world-ptr world) name (null-pointer) upd des))
    (setf (%world-pre-destroy-listener-callback world) callback)))

;; newtonworldgetpostlistener
;; newtonworldaddpostlistener

(defn world-post-update-listener ((world world))
    (or null (function (world single-float) t))
  (declare (optimize (speed 3) (debug 1) (safety 1)))
  (%world-post-update-listener-callback world))

(defn (setf world-post-update-listener)
    ((callback (or null (function (world single-float) t)))
     (world world))
    (or null (function (world single-float) t))
  (declare (optimize (speed 3) (debug 1) (safety 1)))
  (let ((upd (if callback
                 (get-callback '%world-post-update-listener-cb)
                 (null-pointer)))
        (des (if (%world-geom-constructor-callback world)
                 (get-callback '%world-post-destroy-listener-cb)
                 (null-pointer))))
    (with-foreign-string (name "l")
      (NewtonWorldAddPostListener
       (%world-ptr world) name (null-pointer) upd des))
    (setf (%world-post-update-listener-callback world) callback)))

(defn world-post-destroy-listener ((world world))
    (or null (function (world) t))
  (declare (optimize (speed 3) (debug 1) (safety 1)))
  (%world-post-destroy-listener-callback world))

(defn (setf world-post-destroy-listener)
    ((callback (or null (function (world) t)))
     (world world))
    (or null (function (world) t))
  (declare (optimize (speed 3) (debug 1) (safety 1)))
  (let ((upd (if callback
                 (get-callback '%world-post-destroy-listener-cb)
                 (null-pointer)))
        (des (if (%world-geom-constructor-callback world)
                 (get-callback '%world-post-destroy-listener-cb)
                 (null-pointer))))
    (with-foreign-string (name "l")
      (NewtonWorldAddPostListener
       (%world-ptr world) name (null-pointer) upd des))
    (setf (%world-post-destroy-listener-callback world) callback)))

;;------------------------------------------------------------

(defn world-destructor ((world world))
    (or null (function (world) t))
  (declare (optimize (speed 3) (debug 1) (safety 1)))
  (%world-destructor-callback world))

(defn (setf world-destructor) ((callback (or null (function (world) t)))
                               (world world))
    (or null (function (world) t))
  (declare (optimize (speed 3) (debug 1) (safety 1)))
  (let ((cb (if callback
                (get-callback '%world-destructor-cb)
                (null-pointer))))
    (NewtonWorldSetDestructorCallback (%world-ptr world) cb)
    (setf (%world-destructor-callback world) callback)))

;;------------------------------------------------------------

;; newtonworldlistenersetbodydestroycallback
;; newtonworldlistenergetbodydestroycallback
;; newtonworldgetlisteneruserdata

;; {TODO} need to work out listeners first and then finish this

;; (defun world-body-destructor (world)
;;   (%world-destructor-callback world))

;; (defun (setf world-body-destructor) (callback world)
;;   (let ((cb (if callback
;;                 (get-callback '%world-destructor-cb)
;;                 (null-pointer))))
;;     (NewtonWorldListenerSetBodyDestroyCallback (%world-ptr world) cb)
;;     (setf (%world-destructor-callback world) callback)))

;;------------------------------------------------------------

;; newtonworldcollide

;;------------------------------------------------------------

(defn world-ray-cast ((world world)
                      (point-a-v3 (simple-array single-float (3)))
                      (point-b-v3 (simple-array single-float (3)))
                      (filter ray-filter-function)
                      &optional
                      (prefilter ray-prefilter-function))
    null
  "Shoot ray from point p0 to p1 and trigger callback for each body on
  that line. The ray cast function will trigger the callback for every
  intersection between the line segment (from p0 to p1) and a body in
  the world.

  By writing the callback filter function in different ways the
  application can implement different flavors of ray casting. For
  example an all body ray cast can be easily implemented by having the
  filter function always returning 1.0, and copying each rigid body into
  an array of pointers;

  a closest hit ray cast can be implemented by
  saving the body with the smaller intersection parameter and returning
  the parameter t; and a report the first body hit can be implemented by
  having the filter function returning zero after the first call and
  saving the pointer to the rigid body.

  The most common use for the ray
  cast function is the closest body hit, In this case it is important,
  for performance reasons, that the filter function returns the
  intersection parameter. If the filter function returns a value of zero
  the ray cast will terminate immediately.if prefilter is not NULL,
  Newton will call the application right before executing the
  intersections between the ray and the primitive. if the function
  returns zero the Newton will not ray cast the primitive. passing a
  NULL pointer will ray cast the. The application can use this implement
  faster or smarter filters when implementing complex logic, otherwise
  for normal all ray cast this parameter could be NULL.The ray cast
  function is provided as an utility function, this means that even
  thought the function is very high performance by function standards,
  it can not by batched and therefore it can not be an incremental
  function. For example the cost of calling 1000 ray cast is 1000 times
  the cost of calling one ray cast. This is much different than the
  collision system where the cost of calculating collision for 1000
  pairs in much, much less that the 1000 times the cost of one
  pair. Therefore this function must be used with care, as excessive use
  of it can degrade performance."
  ;; {TODO} handle the thread index
  (assert filter)
  (with-foreign-array (p0 point-a-v3 '(:array :float 3))
    (with-foreign-array (p1 point-b-v3 '(:array :float 3))
      (setf (%world-ray-filter-callback world) filter)
      (setf (%world-ray-prefilter-callback world) prefilter)
      (unwind-protect
           (newtonworldraycast (%world-ptr world)
                               p0
                               p1
                               '%world-ray-filter-cb
                               (%world-ptr world)
                               (if prefilter
                                   '%world-ray-prefilter-cb
                                   (null-pointer))
                               0)
        (setf (%world-ray-filter-callback world) nil)
        (setf (%world-ray-prefilter-callback world) nil))))
  nil)

;; newtonworldconvexcast
;; newtonworldconvexcastreturninfo
;; newtoncollisionraycast
