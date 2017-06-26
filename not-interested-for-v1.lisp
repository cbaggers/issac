#||

newtongeteulerangle
newtonseteulerangle

newtondeserializecallback
newtononbodydeserializationcallback
newtononbodyserializationcallback
newtononjointdeserializationcallback
newtononjointserializationcallback
newtoncreatecollisionfromserialization
newtoncollisionserialize
newtononusercollisionserializationcallback
newtonserializecallback

(defstruct ll-collision
  ())

(defun geometry-collide ((WORLD)
                         (MAXSIZE)
                         (geometry-A)
                         (MATRIX-a)
                         (geometry-b)
                         (matrix-b (:POINTER :FLOAT))
                         (CONTACTS (:POINTER :FLOAT))
                         (NORMALS (:POINTER :FLOAT))
                         (PENETRATION (:POINTER :FLOAT))
                         (ATTRIBUTE-A (:POINTER :LONG-LONG))
                         (ATTRIBUTE-B (:POINTER :LONG-LONG))
                         (thread-index 0))
  "This function can be used as a low-level building block for a
   stand-alone collision system.  Applications that have already there
   own physics system, and only want and quick and fast collision
   solution, can use Newton advanced collision engine as the low level
   collision detection part.  To do this the application only needs to
   initialize Newton, create the collision primitives at application
   discretion, and just call this function when the objects are in close
   proximity. Applications using Newton as a collision system only, are
   responsible for implementing their own broad phase collision
   determination, based on any high level tree structure.  Also the
   application should implement their own trivial aabb test, before
   calling this function."
  :INT)
(newtoncollisioncollide)
;; newtoncollisioncollidecontinue

(newtoncollisionclosestpoint)
(newtoncollisionintersectiontest)
(defun geometry-point-distance (world geometry dir3)
  (with-foreign-array (d3 dir3 '(:array :float 3))
    (newtoncollisionpointdistance
     (%world-ptr world) d3 (%geometry-ptr geometry)
     )))

(newtoncreateusermeshcollision)
||#


#|| Is not valid in Newton itself

(newtonconvexhullgetvertexdata )

||#
