(in-package :issac)

;;------------------------------------------------------------

(deftclass (hinge (:constructor %make-hinge) (:include joint)))

newtonconstraintcreatehinge
newtonhingecalculatestopalpha
newtonhingegetjointforce
newtonhingegetjointomega
