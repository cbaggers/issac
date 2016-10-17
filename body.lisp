(in-package :issac)

;;------------------------------------------------------------

;;
;; BODY

(deftclass (body (:constructor %make-world)
                  (:conc-name %body-))
  (ptr (error "") :type foreign-pointer))

;;------------------------------------------------------------

(defun validate-body-kind (body-kind)
  (assert (member body-kind '(:kinematic :dynamic :deformable)) (body-kind))
  body-kind)

(defun make-body (world collision &key (kind :dynamic) (matrix4 (m4:identity)))
  (let ((kind (validate-body-kind kind))
        (wptr (%world-ptr world)))
    (with-foreign-array (m4 matrix4 '(:array :float 16))
      (%make-body
       :ptr (ecase kind
              (:kinematic (newtoncreatekinematicbody
                           wptr (%collision-ptr collision) m4))
              (:dynamic (newtoncreatedynamicbody
                         wptr (%collision-ptr collision) m4))
              (:deformable (newtoncreatedeformablebody
                            wptr (%def-mesh-ptr collision) m4)))))))

;;------------------------------------------------------------

(defun body-id (body)
  (newtonbodygetid (%body-ptr)))


newtonbodygetworld
newtonbodygetuserdata
newtonbodygetmaterialgroupid

;;------------------------------------------------------------

newtonbodyaddforce
newtonbodyaddimpulse
newtonbodyaddtorque

;;------------------------------------------------------------

newtonbodygetangulardamping
newtonbodysetangulardamping

;;------------------------------------------------------------

newtonbodygetautosleep
newtonbodysetautosleep

;;------------------------------------------------------------

newtonbodygetcentreofmass
newtonbodysetcentreofmass

;;------------------------------------------------------------

newtonbodygetcollidable
newtonbodysetcollidable

;;------------------------------------------------------------

newtonbodygetcollision
newtonbodysetcollision

;;------------------------------------------------------------

newtonbodysetcontinuouscollisionmode
newtonbodygetcontinuouscollisionmode

;;------------------------------------------------------------

newtonbodygetjointrecursivecollision
newtonbodysetjointrecursivecollision

;;------------------------------------------------------------

newtonbodygetlineardamping
newtonbodysetlineardamping

;;------------------------------------------------------------

newtonbodygetmatrix
newtonbodysetmatrix

;;------------------------------------------------------------

newtonbodygetomega
newtonbodysetomega

;;------------------------------------------------------------

newtonbodygettorque
newtonbodysettorque

;;------------------------------------------------------------

newtonbodygetmassmatrix
newtonbodysetmassmatrix

;;------------------------------------------------------------

newtonbodygetvelocity
newtonbodysetvelocity

;;------------------------------------------------------------

newtonbodygetsleepstate
newtonbodysetsleepstate

;;------------------------------------------------------------

newtonbodygetmaxrotationperstep
newtonbodysetmaxrotationperstep

;;------------------------------------------------------------
;;------------------------------------------------------------

newtonbodygetaabb

;;------------------------------------------------------------

newtonbodygetmass
newtonbodygetinvmass

newtonbodysetmassproperties
newtonbodysetfullmassmatrix

;;------------------------------------------------------------

newtonbodygetforce
newtonbodygetforceacc
newtonbodycalculateinversedynamicsforce

;;------------------------------------------------------------

newtonbodygetposition
newtonbodygetrotation
newtonbodygetpointvelocity



newtonbodygetinertiamatrix
newtonbodygetinvinertiamatrix



newtonbodygetsimulationstate
newtonbodygetskeleton
newtonbodygettorqueacc


newtonbodysetmaterialgroupid
newtonbodysetvelocitynosleep
newtonbodysetmatrixnosleep
newtonbodysetomeganosleep
newtonbodysetmatrixrecursive
newtonbodysetsimulationstate
newtonbodysetcollisionscale


newtonbodyapplyimpulsearray
newtonbodyapplyimpulsepair

newtonbodydestructor



newtonbodygetfirstjoint
newtonbodygetnextjoint
newtonbodygetnextcontactjoint




newtonbodyintegratevelocity
newtonbodyiterator
