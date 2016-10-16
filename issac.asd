;;;; issac.asd

(asdf:defsystem #:issac
  :description "Lispy abstraction over newton-dynamics"
  :author "Baggers <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :encoding :utf-8
  :serial t
  :depends-on (#:raw-bindings-newton
               #:cffi #:uiop #:structy-defclass #:rtg-math
               #:fn)
  :components ((:file "package")
               (:file "extra-bindings")
               (:file "cffi-helpers")
               ;;(:file "play")
               ))
