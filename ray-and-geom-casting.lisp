(in-package :issac)

;;------------------------------------------------------------

(defcstruct ray-cast-filter-all-data
  (hit-count :int)
  (bodies :pointer))

(defcallback ray-cast-filter-all :float ()

  1s0)

;;------------------------------------------------------------

(defun world-ray-cast (world point-a-v3 point-b-v3 filter &optional prefilter)
  "Shoot ray from point p0 to p1 and trigger callback for each body on
  that line. The ray cast function will trigger the callback for every
  intersection between the line segment (from p0 to p1) and a body in
  the world.

  By writing the callback filter function in different ways the
  application can implement different flavors of ray casting. For
  example an all body ray cast can be easily implemented by having the
  filter function always returning 1.0, and copying each rigid body into
  an array of pointers;

  a closest hit ray cast can be implemented by
  saving the body with the smaller intersection parameter and returning
  the parameter t; and a report the first body hit can be implemented by
  having the filter function returning zero after the first call and
  saving the pointer to the rigid body.

  The most common use for the ray
  cast function is the closest body hit, In this case it is important,
  for performance reasons, that the filter function returns the
  intersection parameter. If the filter function returns a value of zero
  the ray cast will terminate immediately.if prefilter is not NULL,
  Newton will call the application right before executing the
  intersections between the ray and the primitive. if the function
  returns zero the Newton will not ray cast the primitive. passing a
  NULL pointer will ray cast the. The application can use this implement
  faster or smarter filters when implementing complex logic, otherwise
  for normal all ray cast this parameter could be NULL.The ray cast
  function is provided as an utility function, this means that even
  thought the function is very high performance by function standards,
  it can not by batched and therefore it can not be an incremental
  function. For example the cost of calling 1000 ray cast is 1000 times
  the cost of calling one ray cast. This is much different than the
  collision system where the cost of calculating collision for 1000
  pairs in much, much less that the 1000 times the cost of one
  pair. Therefore this function must be used with care, as excessive use
  of it can degrade performance."
  ;; {TODO} handle the thread index
  (assert filter)
  (with-foreign-array (p0 point-a-v3 '(:array :float 3))
    (with-foreign-array (p1 point-b-v3 '(:array :float 3))
      (setf (%world-ray-filter-callback world) filter)
      (when prefilter
        (setf (%world-ray-prefilter-callback world) prefilter))
      (unwind-protect
           (newtonworldraycast (%world-ptr world)
                               p0
                               p1
                               '%world-ray-filter-cb
                               (%world-ptr world)
                               (if prefilter
                                   '%world-ray-prefilter-cb
                                   (null-pointer))
                               0)
        (setf (%world-ray-filter-callback world) nil)
        (setf (%world-ray-prefilter-callback world) nil)))))



;; newtoncollisionraycast
;; newtonworldconvexcast
;; newtonworldconvexcastreturninfo
