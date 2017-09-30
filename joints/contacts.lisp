(in-package :issac)

;; Contact is void* in Newton so not sure how to deal with that

;;------------------------------------------------------------
;; Get the geometry involved (or just the ids of the geometry)

(defn contact-geometry-0 ((contact foreign-pointer))
    geometry
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (%geom-ptr->geom
   (newtoncontactgetcollision0 contact)))

(defn contact-geometry-1 ((contact foreign-pointer))
    geometry
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (%geom-ptr->geom
   (newtoncontactgetcollision1 contact)))

(defn contact-geometry-id-0 ((contact foreign-pointer))
    foreign-pointer
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtoncontactgetcollisionid0 contact))

(defn contact-geometry-id-1 ((contact foreign-pointer))
    foreign-pointer
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtoncontactgetcollisionid1 contact))

;;------------------------------------------------------------
;; Contact Material

(defn contact-material-pair ((contact foreign-pointer))
    material-pair
  "Get the material-pair holding the materials of the bodies involved
   in the contact"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (%material-pair-ptr->material-pair
   (newtoncontactgetmaterial contact)))

;;------------------------------------------------------------
;; Iteration over contacts

(defn %joint-contact-count ((contact-joint joint))
    (signed-byte 32)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtoncontactjointgetcontactcount contact-joint))

(defn %joint-first-contact ((contact-joint joint))
    foreign-pointer
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtoncontactjointgetfirstcontact (%joint-ptr contact-joint)))

(defn %joint-next-contact ((contact-joint joint) (contact foreign-pointer))
    foreign-pointer
  ;; contact needed as this seems to be a linked list
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtoncontactjointgetnextcontact contact-joint contact))

(defmacro do-joint-contacts ((var-name joint) &body body)
  ;; hidden is just so the user can't fuck the process by setting
  ;; var-name to something else
  (with-gensyms (gjoint hidden)
    `(let* ((,gjoint ,joint)
            (,hidden (%joint-first-contact ,gjoint)))
       (loop :for i :below (%joint-contact-count ,gjoint) :do
          (let ((,var-name (%joint-ptr->joint ,hidden)))
            ,@body
            (setf ,hidden (%joint-next-contact ,gjoint ,hidden)))))))

;;------------------------------------------------------------

(defn joint-remove-contact ((contact-joint joint) (contact foreign-pointer))
    null
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtoncontactjointremovecontact contact-joint contact)
  nil)

(defn joint-closest-distance ((contact-joint joint))
    single-float
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (newtoncontactjointgetclosestdistance contact-joint))
