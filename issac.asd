;;;; issac.asd

(asdf:defsystem #:issac
  :description "Lispy abstraction over newton-dynamics"
  :author "Baggers <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :encoding :utf-8
  :serial t
  :depends-on (#:raw-bindings-newton
               #:cffi #:uiop #:rtg-math #:rtg-math.vari)
  :components ((:file "package")
               (:file "cffi-helpers")
               (:file "types")
               (:file "system")
               (:file "world")
               (:file "maths")
               (:file "geometry")
               (:file "body")
               (:file "mesh")
               (:file "skeleton")
               (:file "material")
               (:file "joints/joint")
               (:file "joints/up-vector")
               (:file "joints/slider")
               (:file "joints/corkscrew")
               (:file "joints/contacts")
               (:file "joints/ball-socket")
               (:file "joints/bilateral")
               (:file "joints/hinge")
               (:file "joints/universal")
               (:file "callbacks")
               (:file "init")))
