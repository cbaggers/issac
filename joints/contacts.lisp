(in-package :issac)

;; Contact is void* in Newton so not sure how to deal with that

;;------------------------------------------------------------
;; Get the geometry involved (or just the ids of the geometry)

(defun contact-geometry-0 (contact)
  (%geom-ptr->geom
   (newtoncontactgetcollision0 contact)))

(defun contact-geometry-1 (contact)
  (%geom-ptr->geom
   (newtoncontactgetcollision1 contact)))

(defun contact-geometry-id-0 (contact)
  (newtoncontactgetcollisionid0 contact))

(defun contact-geometry-id-1 (contact)
  (newtoncontactgetcollisionid1 contact))

;;------------------------------------------------------------
;; Contact Material

(defun contact-material-pair (contact)
  "Get the material-pair holding the materials of the bodies involved
   in the contact"
  (%material-pair-ptr->material-pair
   (newtoncontactgetmaterial contact)))

;;------------------------------------------------------------
;; Iteration over contacts

(defun %joint-contact-count (contact-joint)
  (newtoncontactjointgetcontactcount contact-joint))

(defun %joint-first-contact (contact-joint)
  (newtoncontactjointgetfirstcontact (%joint-ptr contact-joint)))

(defun %joint-next-contact (contact-joint contact)
  ;; contact needed as this seems to be a linked list
  (newtoncontactjointgetnextcontact contact-joint contact))

(defmacro do-joint-contacts ((var-name joint) &body body)
  ;; hidden is just so the user can't fuck the process by setting
  ;; var-name to something else
  (with-gensyms (gjoint hidden)
    `(let* ((,gjoint ,joint)
            (,hidden (%joint-first-contact ,gjoint))
            (,var-name ,hidden))
       (loop :for i :below (%joint-contact-count ,gjoint) :do
          (setf ,var-name ,hidden)
          (progn ,@body)
          (setf ,hidden (%joint-next-contact ,gjoint ,hidden)
                ,var-name ,hidden)))))

;;------------------------------------------------------------

(defun joint-remove-contact (contact-joint contact)
  (newtoncontactjointremovecontact contact-joint contact))

(defun joint-closest-distance (contact-joint)
  (newtoncontactjointgetclosestdistance contact-joint))
