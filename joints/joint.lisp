(in-package :issac)

;;------------------------------------------------------------

(deftclass (joint (:constructor %make-joint)
                     (:conc-name %joint-))
  (ptr (error "") :type foreign-pointer))

NewtonJointSetUserData
NewtonJointGetUserData
NewtonJointGetInfo
NewtonJointGetBody0
NewtonJointGetBody1
NewtonJointSetCollisionState
NewtonJointGetCollisionState
NewtonJointSetStiffness
NewtonJointGetStiffness
NewtonJointSetDestructor
NewtonDestroyJoint
newtonjointisactive
