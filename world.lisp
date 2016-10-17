(in-package :issac)

(deftclass (world (:constructor %make-world))
  (ptr (error) :type foreign-pointer))

(defun make-world (&key )
  (newtoncreate)
  )

;;
;; WORLD
;; newtonworld
;; newtonworldaddpostlistener
;; newtonworldaddprelistener
;; newtonworldcollide
;; newtonworldconvexcast
;; newtonworldconvexcastreturninfo
;; newtonworldcriticalsectionlock
;; newtonworldcriticalsectionunlock
;; newtonworldfloatsize
;; newtonworldforeachbodyinaabbdo
;; newtonworldforeachjointdo
;; newtonworldgetbodycount
;; newtonworldgetconstraintcount
;; newtonworldgetfirstmaterial
;; newtonworldgetlisteneruserdata
;; newtonworldgetnextbody
;; newtonworldgetnextmaterial
;; newtonworldgetpostlistener
;; newtonworldgetprelistener
;; newtonworldgetuserdata
;; newtonworldgetversion
;; newtonworldsetuserdata

;;
;; UPDATE
;; newtonupdate
;; newtonupdateasync
;; newtonwaitforupdatetofinish
;; newtongetlastupdatetime
