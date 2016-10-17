(in-package :issac)

;;------------------------------------------------------------

(deftclass (geometry (:constructor %make-geometry)
                     (:conc-name %geometry-))
  (ptr (error "") :type foreign-pointer))

(deftclass (null-geometry
            (:constructor %make-null)
            (:include geometry)))

(deftclass (box-geometry
            (:constructor %make-box)
            (:include geometry)))

(deftclass (sphere-geometry
            (:constructor %make-sphere)
            (:include geometry)))

(deftclass (cone-geometry
            (:constructor %make-cone)
            (:include geometry)))

(deftclass (capsule-geometry
            (:constructor %make-capsule)
            (:include geometry)))

(deftclass (cylinder-geometry
            (:constructor %make-cylinder)
            (:include geometry)))

(deftclass (chamfer-cylinder-geometry
            (:constructor %make-chamfer-cylinder)
            (:include geometry)))

(defun make-null-geometry (world)
  (let ((wptr (%world-ptr world)))
    (%make-null
     :ptr (newtoncreatenull wptr))))

(defun make-box-geometry (world &key (dimensions (v! 1s0 1s0 1s0))
                                  (offset-matrix4 (m4:identity)))
  (let ((wptr (%world-ptr world)))
    (with-foreign-array (m4 offset-matrix4 '(:array :float 16))
      (%make-box
       :ptr (newtoncreatebox
             wptr (v:x dimensions) (v:y dimensions) (v:z dimensions) 0 m4)))))

(defun make-sphere-geometry (world &key (radius 1s0)
                                     (offset-matrix4 (m4:identity)))
  (let ((wptr (%world-ptr world)))
    (with-foreign-array (m4 offset-matrix4 '(:array :float 16))
      (%make-sphere
       :ptr (newtoncreatesphere wptr 0 (float radius) m4)))))

(defun make-cone-geometry (world &key (radius 1s0) (height 1s0)
                                   (offset-matrix4 (m4:identity)))
  (let ((wptr (%world-ptr world)))
    (with-foreign-array (m4 offset-matrix4 '(:array :float 16))
      (%make-cone
       :ptr (newtoncreatecone wptr (float radius) (float height) 0  m4)))))

(defun make-capsule-geometry (world &key (radius-0 1s0) (radius-1 1s0)
                                      (height 4s0)
                                      (offset-matrix4 (m4:identity)))
  (let ((wptr (%world-ptr world)))
    (with-foreign-array (m4 offset-matrix4 '(:array :float 16))
      (%make-capsule
       :ptr (newtoncreatecapsule
             wptr (float radius-0) (float radius-1) (float height) 0 m4)))))

(defun make-cylinder-geometry (world &key (radius-0 1s0) (radius-1 1s0)
                                       (height 4s0)
                                       (offset-matrix4 (m4:identity)))
  (let ((wptr (%world-ptr world)))
    (with-foreign-array (m4 offset-matrix4 '(:array :float 16))
      (%make-cylinder
       :ptr (newtoncreatecylinder
             wptr (float radius-0) (float radius-1) (float height) 0 m4)))))

(defun make-chamfer-cylinder-geometry (world &key (radius 1s0) (height 1s0)
                                               (offset-matrix4 (m4:identity)))
  (let ((wptr (%world-ptr world)))
    (with-foreign-array (m4 offset-matrix4 '(:array :float 16))
      (%make-chamfer-cylinder
       :ptr (newtoncreatechamfercylinder
             wptr (float radius) (float height) 0 m4)))))
