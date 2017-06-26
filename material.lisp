(in-package :issac)

;;
;; NewtonMaterial* is a pair so we call it material-pair
;;
;; MaterialGroupID is what we call a material
;;

;;------------------------------------------------------------

(defun world-default-material (world)
  "Gets the defaul material for this world.

   A Material can be interpreted as the nodes of a dense graph. The edges of
   the graph are called material-pairs and we describe the relationships
   between pairs of materials.

   When the Newton world is created, the default material is created by the
   engine. When bodies are created the application assigns a material to the
   body."
  (NewtonMaterialGetDefaultGroupID (%world-ptr world)))


;; NewtonMaterialCreateGroupID


(defun create-material (world)
  "Create a new material.

   A Material can be interpreted as the nodes of a dense graph. The edges of
   the graph are called material-pairs and we describe the relationships
   between pairs of materials.

   When the Newton world is created, the default material is created by the
   engine. When bodies are created the application assigns a material to the
   body.

   Note: The only way to destroy a material after its creation is by
   destroying all the bodies and calling the function `destroy-all-materials`"
  (NewtonMaterialCreateGroupID (%world-ptr world)))

(defun destroy-all-materials ()
  "Remove all materials from the Newton world. This function must be called
   after there are no more rigid bodies in the word."
  (NewtonMaterialDestroyAllGroupID world))

;; newtononaabboverlap
;; newtonmaterialcontactrotatetangentdirections
;; newtonmaterialgetbodycollidingshape
;; newtonmaterialgetcontactfaceattribute
;; newtonmaterialgetcontactforce
;; newtonmaterialgetcontactmaxnormalimpact
;; newtonmaterialgetcontactmaxtangentimpact
;; newtonmaterialgetcontactnormalspeed
;; newtonmaterialgetcontactpenetration
;; newtonmaterialgetcontactpositionandnormal
;; newtonmaterialgetcontacttangentdirections
;; newtonmaterialgetcontacttangentspeed
;; newtonmaterialsetcallbackuserdata
;; newtonmaterialsetcontactelasticity
;; newtonmaterialsetcontactfrictioncoef
;; newtonmaterialsetcontactfrictionstate
;; newtonmaterialsetcontactnormaldirection
;; newtonmaterialsetcontactposition
;; newtonmaterialsetcontactsoftness
;; newtonmaterialsetcontacttangentacceleration
;; newtonmaterialsetcontacttangentfriction
;; newtonmaterialsetdefaultcollidable
;; newtonmaterialsetdefaultelasticity
;; newtonmaterialsetdefaultfriction
;; newtonmaterialsetdefaultsoftness
;; newtonmaterialsetsurfacethickness
