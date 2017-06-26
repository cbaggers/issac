(in-package #:issac)

(defconstant +vec3-size+ (* 3 (foreign-type-size :float)))

(defcfun (%memset "memset") :pointer
  (destination-pointer :pointer)
  (val :int)
  (byte-length :long))

(defun zero-out (ptr type &optional (count 1))
  (%memset ptr 0 (* count (foreign-type-size type))))

(defmacro c-ptr (ptr type &rest path)
  (labels ((gen (ptr type path)
             (let ((slot (first path)))
               (if (= 1 (length path))
                   `(foreign-slot-pointer ,ptr ',type ',slot  )
                   (let ((form `(foreign-slot-pointer ,ptr ',type ',slot))
                         (type (cffi:foreign-slot-type type slot)))
                     `(c-ptr ,form ,type ,@(rest path)))))))
    (assert path)
    (gen ptr type path)))

(defmacro c-val (ptr type &rest path)
  (labels ((gen (ptr type path)
             (let ((slot (first path)))
               (if (= 1 (length path))
                   `(foreign-slot-value ,ptr ',type ',slot  )
                   (let ((form `(foreign-slot-pointer ,ptr ',type ',slot))
                         (type (cffi:foreign-slot-type type slot)))
                     `(c-val ,form ,type ,@(rest path)))))))
    (assert path)
    (gen ptr type path)))

(defun ptr->m4 (ptr)
  (make-array
   16 :element-type 'single-float
   :initial-contents
   (loop :for i :below 16 :collect (mem-aref ptr :float i))))

(defun ptr->v3 (ptr)
  (v! (mem-aref ptr :float 0)
      (mem-aref ptr :float 1)
      (mem-aref ptr :float 2)))

(defun fnum (foreign-type)
  (let ((size (* 8 (foreign-type-size foreign-type))))
    (ecase foreign-type
      ((:uint :uint8 :ushort :ubyte) `(unsigned-byte ,size))
      ((:int :int8 :short :byte) `(unsigned-byte ,size)))))
