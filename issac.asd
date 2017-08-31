;;;; issac.asd

(asdf:defsystem #:issac
  :description "Lispy abstraction over newton-dynamics"
  :author "Baggers <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :encoding :utf-8
  :serial t
  :depends-on (#:raw-bindings-newton
               #:cffi #:uiop #:rtg-math)
  :components ((:file "package")
               (:file "cffi-helpers")
               (:file "types")
               (:file "system")
               (:file "world")
               (:file "geometry")
               (:file "body")
               (:file "mesh")
               (:file "joints/up-vector")
               (:file "joints/joint")
               (:file "joints/slider")
               (:file "joints/corkscrew")
               (:file "joints/contacts")
               (:file "callbacks")
               (:file "init")))
