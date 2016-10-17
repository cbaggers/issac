(in-package :issac)

(deftclass (world (:constructor %make-world)
                  (:conc-name %world-))
  (ptr (error "") :type foreign-pointer)
  (solve-model (error "") :type t)
  (friction-model (error "") :type keyword)
  (min-frame-rate 60 :type (unsigned-byte 16)))

;; Create an instance of the Newton world. This function does the same
;; as NewtonCreate, except that it accepts an extra argument to
;; specify the stack size for each thread. This is only useful for
;; simulation with very many (ie thousands or more objects). If in
;; doubt, use NewtonCreate as it uses reasonable defaults.

;;------------------------------------------------------------

(defun make-world (&key (broadphase-algorithm :default)
                     (stack-size-mb :default)
                     (max-thread-count :default)
                     (solve-model :adaptive)
                     (friction-model :adaptive)
                     (minimum-frame-rate 60))
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
    (unless (eq broadphase-algorithm :default)
      (setf (world-broadphase-algorithm world) broadphase-algorithm))
    (unless (eq max-thread-count :default)
      (setf (world-max-thread-count world) max-thread-count))
    (setf (world-solver-model world) solve-model)
    (setf (world-friction-model world) friction-model)
    world))

;;------------------------------------------------------------

(defun free-world (world)
  "Destroy an instance of the Newton world. This function will destroy the entire Newton world."
  (newtondestroy (%world-ptr world))
  t)

(defun world-destroy-all-bodies (world)
  (newtondestroyallbodies (%world-ptr world)))

;;------------------------------------------------------------

(defconstant +1/60s0+ (/ 1s0 60s0))

(defun update-world (world &optional (timestep +1/60s0+))
  "Advance the simulation by a user defined amount of time. This
   function will advance the simulation by the specified amount of
   time."
  (newtonupdate (%world-ptr world) timestep)
  world)

(defun update-world-async (world &optional (timestep +1/60s0+))
  "Advance the simulation by a user defined amount of time. This
   function will advance the simulation by the specified amount of
   time."
  (newtonupdateasync (%world-ptr world) timestep)
  world)


(defun world-wait-for-update-to-finish (world)
  (newtonwaitforupdatetofinish (%world-ptr world))
  world)

(defun world-last-update-time (world)
  (newtongetlastupdatetime (%world-ptr world)))

;;------------------------------------------------------------

(defun validate-min-frame-rate (frame-rate)
  (assert (and (>= 60) (<= frame-rate 1000)
               (typep frame-rate '(unsigned-byte 16)))
          (frame-rate))
  frame-rate)

(defun world-minimum-frame-rate (world)
  "The minimum frame rate at which the simulation can run. The default
  minimum frame rate of the simulation is 60 frame per second. Newton
  will perform sub steps to meet the desired minimum FPS, should the
  frame rate drop below the specified minimum.
  This value is clamped between 60fps and 1000fps"
  (%world-min-frame-rate world))

;; (defun (setf world-minimum-frame-rate) (frame-rate world)
;;   "The minimum frame rate at which the simulation can run. The default
;;   minimum frame rate of the simulation is 60 frame per second. Newton
;;   will perform sub steps to meet the desired minimum FPS, should the
;;   frame rate drop below the specified minimum.
;;   This value is clamped between 60fps and 1000fps"
;;   (let ((frame-rate (validate-min-frame-rate frame-rate)))
;;     (newtonsetminimumframerate (%world-ptr world) frame-rate)
;;     (setf (%world-min-frame-rate world) frame-rate)))


;;------------------------------------------------------------

(defun world-invalidate-cache (world)
  (newtoninvalidatecache (%world-ptr world))
  world)

;;------------------------------------------------------------

(defun world-current-device (world)
  (newtongetcurrentdevice (%world-ptr world)))

(defun (setf world-current-device) (value world)
  (assert (and (< value (%world-device-count world))
               (>= value 0)))
  (newtonsetcurrentdevice (%world-ptr world) (float value))
  value)

(defun world-devices (world)
  (loop :for i :below (%world-device-count world) :collect
     (list i (%device-string world i))))

(defun %device-string (world device-index)
  (with-foreign-object (cstr :char 100)
    (let ((len (newtongetdevicestring (%world-ptr world) device-index cstr 100)))
      (cffi:foreign-string-to-lisp cstr :count len))))

(defun %world-device-count (world)
  (newtonenumeratedevices (%world-ptr world)))

;;------------------------------------------------------------

(defparameter *broadphase-algorithms* '(:default :persintent))

(defun world-broadphase-algorithm (world)
  (elt *broadphase-algorithms*
       (newtongetbroadphasealgorithm (%world-ptr world))))


(defun (setf world-broadphase-algorithm) (algorithm-id world)
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
  (assert (validate-solver-model solver-model) (solver-model))
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

(defun validate-friction-model (friction-model)
  (assert (or (eq friction-model :exact) (eq friction-model :adaptive))
          (friction-model))
  friction-model)

(defun world-friction-model (world)
  (%world-friction-model world))

(defun (setf world-friction-model) (model world)
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

(defun world-contact-merge-tolerance (world)
  (newtongetcontactmergetolerance (%world-ptr world)))

(defun (setf world-contact-merge-tolerance) (value world)
  (newtonsetcontactmergetolerance (%world-ptr world) (float value))
  value)

;;------------------------------------------------------------

;; newtonsetsolverconvergencequality

;;------------------------------------------------------------

(defun world-thread-count (world)
  (newtongetthreadscount (%world-ptr world)))

(defun world-max-thread-count (world)
  (newtongetmaxthreadscount (%world-ptr world)))

(defun (setf world-max-thread-count) (value world)
  (assert (typep value '(unsigned-byte 8)))
  (newtonsetthreadscount (%world-ptr world) value)
  value)

;; (defun world-dispatch-thread-job (world task)
;;   (newtondispachthreadjob world  )
;;     )

(defun world-sync-thread-jobs (world)
  (newtonsyncthreadjobs (%world-ptr world))
  world)

;; newtonyield

;;------------------------------------------------------------

;; newtonworldgetnextbody

;;------------------------------------------------------------

;; newtongetjointserializationcallbacks
;; newtonsetjointserializationcallbacks

;;------------------------------------------------------------

;; newtonworldcriticalsectionlock
;; newtonworldcriticalsectionunlock
;; newtonatomicadd
;; newtonatomicswap

;;------------------------------------------------------------

;;
;; WORLD
;; newtonworldaddpostlistener
;; newtonworldaddprelistener
;; newtonworldcollide
;; newtonworldconvexcast
;; newtonworldconvexcastreturninfo
;;
;; newtonworldforeachbodyinaabbdo
;; newtonworldforeachjointdo
;; newtonworldgetbodycount
;; newtonworldgetconstraintcount
;; newtonworldgetfirstmaterial
;; newtonworldgetlisteneruserdata

;; newtonworldgetnextmaterial
;; newtonworldgetpostlistener
;; newtonworldgetprelistener
;; newtonworldgetuserdata
;;
;; newtonworldsetuserdata
