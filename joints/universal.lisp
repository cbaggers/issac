(in-package :issac)

;;------------------------------------------------------------

(deftclass (universal-joint (:constructor %make-universal-joint)
                            (:include joint)))

newtonconstraintcreateuniversal

;;------------------------------------------------------------

(defun universal-joint-force (universal-joint)
  (with-foreign-object (v3 :float 3)
    (newtonuniversalgetjointforce (%joint-ptr universal-joint) v3)
    (ptr->v3 v3)))

(defun universal-joint-angle (universal-joint)
  (newtonuniversalgetjointangle1 (%joint-ptr universal-joint)))

(defun universal-joint-omega-0 (universal-joint)
  (newtonuniversalgetjointomega0 (%joint-ptr universal-joint)))

(defun universal-joint-omega-1 (universal-joint)
  (newtonuniversalgetjointomega1 (%joint-ptr universal-joint)))

;;------------------------------------------------------------

;; (defun universal-joint-calc-stop-alpha-0 (universal-joint desc-ptr angle)
;;   (newtonuniversalcalculatestopalpha0 (%joint-ptr universal-joint) desc-ptr angle))

;; (defun universal-joint-calc-stop-alpha-1 (universal-joint desc-ptr angle)
;;   (newtonuniversalcalculatestopalpha1 (%joint-ptr universal-joint) desc-ptr angle))
