(in-package :issac)

;;------------------------------------------------------------

(deftclass (slider (:constructor %make-slider) (:include joint)))

newtonconstraintcreateslider
newtonslidercalculatestopaccel
newtonslidergetjointposit
newtonslidergetjointveloc
