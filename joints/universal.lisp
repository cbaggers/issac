(in-package :issac)

;;------------------------------------------------------------

(deftclass (universal-joint (:constructor %make-universal-joint)
                            (:include joint)))

newtonconstraintcreateuniversal
newtonuniversalcalculatestopalpha0
newtonuniversalcalculatestopalpha1
newtonuniversalgetjointangle1
newtonuniversalgetjointforce
newtonuniversalgetjointomega0
newtonuniversalgetjointomega1
