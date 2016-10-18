(in-package :issac)

;;------------------------------------------------------------

(deftclass (mesh (:constructor %make-mesh)
                     (:conc-name %mesh-))
  (ptr (error "") :type foreign-pointer))
