(in-package :issac)

;;------------------------------------------------------------

(deftclass (slider (:constructor %make-slider) (:include joint)))

newtonconstraintcreateslider

;;------------------------------------------------------------

(defun slider-position (slider)
  (newtonslidergetjointposit (%joint-ptr slider)))

(defun slider-velocity (slider)
  (newtonslidergetjointveloc (%joint-ptr slider)))

;;------------------------------------------------------------

;; (defun corkscrew-calc-stop-acceleration (corkscrew desc-ptr position)
;;   (newtonslidercalculatestopaccel (%joint-ptr corkscrew) desc-ptr position))
