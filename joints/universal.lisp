(in-package :issac)

;;------------------------------------------------------------

(defun make-universal (world parent-body child-body pivot-point-v3
                       pin0-dir-v3 pin1-dir-v3)
  (with-foreign-array (piv3 pivot-point-v3 '(:array :float 3))
    (with-foreign-array (dir0 pin0-dir-v3 '(:array :float 3))
      (with-foreign-array (dir1 pin1-dir-v3 '(:array :float 3))
        (newtonconstraintcreateuniversal
         (%world-ptr world) piv3 dir0 dir1
         (%body-ptr child-body)
         (%body-ptr parent-body))))))

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
