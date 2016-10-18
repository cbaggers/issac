(in-package :issac)

;;------------------------------------------------------------

(deftclass (slider (:constructor %make-slider) (:include joint)))

(defun make-slider (world parent-body child-body pivot-point-v3 pin-dir-v3)
  (with-foreign-array (piv3 pivot-point-v3 '(:array :float 3))
    (with-foreign-array (dir3 pin-dir-v3 '(:array :float 3))
      (newtonconstraintcreateslider
       (%world-ptr world) piv3 dir3
       (%body-ptr child-body)
       (%body-ptr parent-body)))))

;;------------------------------------------------------------

(defun slider-position (slider)
  (newtonslidergetjointposit (%joint-ptr slider)))

(defun slider-velocity (slider)
  (newtonslidergetjointveloc (%joint-ptr slider)))

;;------------------------------------------------------------

;; (defun corkscrew-calc-stop-acceleration (corkscrew desc-ptr position)
;;   (newtonslidercalculatestopaccel (%joint-ptr corkscrew) desc-ptr position))
