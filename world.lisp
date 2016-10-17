(in-package :issac)

(deftclass (world (:constructor %make-world))
  (ptr (error "") :type foreign-pointer))

(defun make-world (&key (broadphase-algorithm :default))
  (let ((world (world-invalidate-cache (%make-world :ptr (newtoncreate)))))
    (unless (eq broadphase-algorithm :default)
      (setf (world-broadphase-algorithm )))))

(defun world-invalidate-cache (world)
  (newtoninvalidatecache (world-ptr world))
  world)

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



;;
;; WORLD
;; newtonworldaddpostlistener
;; newtonworldaddprelistener
;; newtonworldcollide
;; newtonworldconvexcast
;; newtonworldconvexcastreturninfo
;; newtonworldcriticalsectionlock
;; newtonworldcriticalsectionunlock
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
