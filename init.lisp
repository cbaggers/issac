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

(defun initialize-issac ()
  (when (%init)
    ;;(reset-worlds)
    t))

(defun uninitialize-issac ()
  (when (%uninit)
    ;;(reset-worlds)
    ))
