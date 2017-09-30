(in-package :issac)

;;------------------------------------------------------------
;; newtonuserjoin
;;------------------------------------------------------------

(defn bilateral-callback ((joint bilateral-joint))
    (or null bilateral-joint-cb-function)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (%bilateral-callback joint))

(defn (setf bilateral-callback)
    ((callback (or null bilateral-joint-cb-function))
     (joint bilateral-joint))
    (or null bilateral-joint-cb-function)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((cb (if callback
                (get-callback '%bilateral-cb)
                (null-pointer))))
    (NewtonUserJointSetFeedbackCollectorCallback (%joint-ptr joint) cb)
    (setf (%bilateral-callback joint) callback)))


;; VVV this one can only be set during NewtonConstraintCreateUserJoint
;; (defun bilateral-info-callback (joint)
;;   (%bilateral-info-callback joint))

;; (defun (setf bilateral-info-callback) (callback joint)
;;   (let ((cb (if callback
;;                 (get-callback '%bilateral-info-cb)
;;                 (null-pointer))))
;;     (NewtonUserJointSetFeedbackCollectorCallback (%joint-ptr joint) cb)
;;     (setf (%bilateral-info-callback joint) callback)))

;;------------------------------------------------------------

;; newtonuserjoinrowscount
;; newtonuserjointaddangularrow
;; newtonuserjointaddgeneralrow
;; newtonuserjointaddlinearrow
;; newtonuserjointgetgeneralrow
;; newtonuserjointgetrowforce
;; newtonuserjointsetfeedbackcollectorcallback
;; newtonuserjointsetrowacceleration
;; newtonuserjointsetrowmaximumfriction
;; newtonuserjointsetrowminimumfriction
;; newtonuserjointsetrowspringdamperacceleration
;; newtonuserjointsetrowstiffness
