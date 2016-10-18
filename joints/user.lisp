(in-package :issac)

;;------------------------------------------------------------

(deftclass (bilateral-joint (:constructor %make-bilateral-joint) (:include joint)))

newtonconstraintcreateuserjoint
newtonuserjointaddangularrow
newtonuserjointaddgeneralrow
newtonuserjointaddlinearrow
newtonuserjointgetgeneralrow
newtonuserjointgetrowforce
newtonuserjointsetrowmaximumfriction
newtonuserjointsetrowminimumfriction
newtonuserjointsetrowspringdamperacceleration
newtonuserjointsetrowstiffness
