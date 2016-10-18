(in-package :issac)

;;------------------------------------------------------------

(deftclass (mesh (:constructor %make-mesh)
                     (:conc-name %mesh-))
  (ptr (error "") :type foreign-pointer))

;; newtondeformablemeshbeginconfiguration
;; newtondeformablemeshconstraintparticle
;; newtondeformablemeshcreateclusters
;; newtondeformablemeshendconfiguration
;; newtondeformablemeshgetfirstsegment
;; newtondeformablemeshgetnextsegment
;; newtondeformablemeshgetparticlecount
;; newtondeformablemeshgetparticleposition
;; newtondeformablemeshgetvertexcount
;; newtondeformablemeshgetvertexstreams
;; newtondeformablemeshsegmentgetindexcount
;; newtondeformablemeshsegmentgetindexlist
;; newtondeformablemeshsegmentgetmaterialid
;; newtondeformablemeshunconstraintparticle
;; newtondeformablemeshupdaterendernormals

;; newtondeformablemeshbeginconfiguration
;; newtondeformablemeshendconfiguration
