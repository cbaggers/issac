(in-package :issac)

(deftclass (world (:constructor %make-world))
  (ptr (error "") :type foreign-pointer))

;; Create an instance of the Newton world. This function does the same
;; as NewtonCreate, except that it accepts an extra argument to
;; specify the stack size for each thread. This is only useful for
;; simulation with very many (ie thousands or more objects). If in
;; doubt, use NewtonCreate as it uses reasonable defaults.

(defun make-world (&key (broadphase-algorithm :default)
                     (stack-size-mb :default)
                     (max-thread-count :default))
  (let ((world (world-invalidate-cache
                (%make-world :ptr (if (eq stack-size-mb :default)
                                      (newtoncreate)
                                      (progn
                                        (assert (integerp stack-size-mb))
                                        (newtoncreateex stack-size-mb)))))))
    (unless (eq broadphase-algorithm :default)
      (setf (world-broadphase-algorithm world) broadphase-algorithm))
    (unless (eq max-thread-count :default)
      (setf (world-max-thread-count world) max-thread-count))
    world))

(defun free-world (world)
  "Destroy an instance of the Newton world. This function will destroy the entire Newton world."
  (newtondestroy (world-ptr world))
  t)

;;------------------------------------------------------------

(defun world-invalidate-cache (world)
  (newtoninvalidatecache (world-ptr world))
  world)

;;------------------------------------------------------------

(defun world-current-device (world)
  (newtongetcurrentdevice (world-ptr world)))

(defun (setf world-current-device) (value world)
  (assert (and (< value (%world-device-count world))
               (>= value 0)))
  (newtonsetcurrentdevice (world-ptr world) (float value))
  value)

(defun world-devices (world)
  (loop :for i :below (%world-device-count world) :collect
     (list i (%device-string world i))))

(defun %device-string (world device-index)
  (with-foreign-object (cstr :char 100)
    (let ((len (newtongetdevicestring (world-ptr world) device-index cstr 100)))
      (cffi:foreign-string-to-lisp cstr :count len))))

(defun %world-device-count (world)
  (newtonenumeratedevices (world-ptr world)))

;;------------------------------------------------------------

(defparameter *broadphase-algorithms* '(:default :persintent))

(defun world-broadphase-algorithm (world)
  (elt *broadphase-algorithms*
       (newtongetbroadphasealgorithm (world-ptr world))))


(defun (setf world-broadphase-algorithm) (algorithm-id world)
  (assert (member algorithm-id *broadphase-algorithms*) (algorithm-id))
  (newtonselectbroadphasealgorithm
   (world-ptr world)
   (position algorithm-id *broadphase-algorithms*))
  algorithm-id)

;;------------------------------------------------------------

(defun world-contact-merge-tolerance (world)
  (newtongetcontactmergetolerance (world-ptr world)))

(defun (setf world-contact-merge-tolerance) (value world)
  (newtonsetcontactmergetolerance (world-ptr world) (float value))
  value)

;;------------------------------------------------------------

(defun world-thread-count (world)
  (newtongetthreadscount (world-ptr world)))

(defun world-max-thread-count (world)
  (newtongetmaxthreadscount (world-ptr world)))

(defun (setf world-max-thread-count) (value world)
  (assert (typep value '(unsigned-byte 8)))
  (newtonsetthreadscount (world-ptr world) value)
  value)

;; (defun world-dispatch-thread-job (world task)
;;   (newtondispachthreadjob world  )
;;     )

(defun world-sync-thread-jobs (world)
  (newtonsyncthreadjobs (world-ptr world))
  world)

;;------------------------------------------------------------

;; newtonyield

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
;; newtonworldfloatsize
;; newtonworldforeachbodyinaabbdo
;; newtonworldforeachjointdo
;; newtonworldgetbodycount
;; newtonworldgetconstraintcount
;; newtonworldgetfirstmaterial
;; newtonworldgetlisteneruserdata
;; newtonworldgetnextbody
;; newtonworldgetnextmaterial
;; newtonworldgetpostlistener
;; newtonworldgetprelistener
;; newtonworldgetuserdata
;; newtonworldgetversion
;; newtonworldsetuserdata

;;
;; UPDATE
;; newtonupdate
;; newtonupdateasync
;; newtonwaitforupdatetofinish
;; newtongetlastupdatetime
