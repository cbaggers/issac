(in-package :issac)

;;------------------------------------------------------------

;; NewtonMaterialGetDefaultGroupID
;; "Get the value of the default MaterialGroupID. Group IDs can be
;; interpreted as the nodes of a dense graph. The edges of the graph are
;; the physics materials. When the Newton world is created, the default
;; Group ID is created by the engine. When bodies are created the
;; application assigns a group ID to the body."


;; NewtonMaterialCreateGroupID
;; "Create a new MaterialGroupID. Group IDs can be interpreted as the
;; nodes of a dense graph. The edges of the graph are the physics
;; materials. When the Newton world is created, the default Group ID is
;; created by the engine. When bodies are created the application assigns
;; a group ID to the body.
;;
;; Note: The only way to destroy a Group ID after its creation is by
;; destroying all the bodies and calling the function
;; NewtonMaterialDestroyAllGroupID.See also:
;; NewtonMaterialDestroyAllGroupID"


;; NewtonMaterialDestroyAllGroupID
;; "Remove all groups ID from the Newton world. This function removes all
;; groups ID from the Newton world. This function must be called after
;; there are no more rigid bodies in the word."


newtononaabboverlap
newtonmaterial
newtonmaterialcontactrotatetangentdirections
newtonmaterialgetbodycollidingshape
newtonmaterialgetcontactfaceattribute
newtonmaterialgetcontactforce
newtonmaterialgetcontactmaxnormalimpact
newtonmaterialgetcontactmaxtangentimpact
newtonmaterialgetcontactnormalspeed
newtonmaterialgetcontactpenetration
newtonmaterialgetcontactpositionandnormal
newtonmaterialgetcontacttangentdirections
newtonmaterialgetcontacttangentspeed
newtonmaterialgetmaterialpairuserdata
newtonmaterialgetuserdata
newtonmaterialsetcallbackuserdata
newtonmaterialsetcontactelasticity
newtonmaterialsetcontactfrictioncoef
newtonmaterialsetcontactfrictionstate
newtonmaterialsetcontactnormaldirection
newtonmaterialsetcontactposition
newtonmaterialsetcontactsoftness
newtonmaterialsetcontacttangentacceleration
newtonmaterialsetcontacttangentfriction
newtonmaterialsetdefaultcollidable
newtonmaterialsetdefaultelasticity
newtonmaterialsetdefaultfriction
newtonmaterialsetdefaultsoftness
newtonmaterialsetsurfacethickness
