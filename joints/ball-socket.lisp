(in-package :issac)

;;------------------------------------------------------------

(deftclass (ball-&-socket (:constructor %make-ball-&-socket) (:include joint)))

newtonconstraintcreateball
newtonballgetjointforce
newtonballgetjointomega
newtonballsetconelimits
