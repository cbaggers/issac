(in-package :issac)

;;------------------------------------------------------------

(deftclass (corkscrew (:constructor %make-corkscrew) (:include joint)))

newtonconstraintcreatecorkscrew
newtoncorkscrewcalculatestopaccel
newtoncorkscrewcalculatestopalpha
newtoncorkscrewgetjointforce
newtoncorkscrewgetjointomega
newtoncorkscrewgetjointposit
newtoncorkscrewgetjointveloc
