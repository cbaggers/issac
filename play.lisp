(in-package :issac)

(defvar points
  (list (list (v! -100s0 0s0  100s0)
              (v! 100s0 0s0  100s0)
              (v! 100s0 0s0 -100s0)
              (v! -100s0 0s0 -100s0))))

(def-callback-apply-force-and-torque apply-gravity (body)
  (multiple-value-bind (mass) (body-mass body)
    (setf (body-force body) (v! 0s0 (* -9.8 mass) 0s0 0s0))))

(defun create-background-body (world)
  (with-geometry (geom (make-geometry-tree world points))
    (make-body world geom)))

(defun create-freefall-ball (world)
  (with-geometry (geom (make-sphere-geometry world :radius 1s0))
    (let ((body (make-body world geom :linear-damping 0s0
                           :mass 1s0)))
      (body-set-force-torque-callback body 'apply-gravity)
      (setf (body-matrix4 body) (m4:translation (v! 0 40 -10)))
      body)))

(defvar world (make-world))
(defvar background-body (create-background-body world))
(defvar freefall-ball (create-freefall-ball world))

(defun newt ()
  (let ((step (/ 1s0 60)))
    (loop :for i :below 300 :do
       (world-step world step)
       (print (body-matrix4 freefall-ball)))))
