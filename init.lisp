(in-package :issac)

(defvar *initialized* nil)

(defun %init ()
  (if *initialized*
      (progn
        (warn "Issac is already initialized")
        nil)
      (progn
        (setf *initialized* t))))

(defun %uninit ()
  (when *initialized*
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
