(in-package :issac)

(defun %init ()
  (if *initialized*
      (progn
        (warn "Issac is already initialized")
        nil)
      (progn
        ;;(dinitode2 0)
        (setf *initialized* t))))

(defun %uninit ()
  (when *initialized*
    ;;(dcloseode)
    (setf *initialized* nil)
    t))

(defun init ()
  (when (%init)
    ;;(reset-worlds)
    t))

(defun uninit ()
  (when (%uninit)
    ;;(reset-worlds)
    ))

(defmacro with-ode-initialized (&body body)
  `(unwind-protect (progn (init) ,@body)
     (uninit)))
