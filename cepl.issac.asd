;;;; cepl.issac.asd

(asdf:defsystem #:cepl.issac
  :description "CEPL specific helpers for Issac"
  :author "Baggers <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :encoding :utf-8
  :serial t
  :depends-on (:cepl :issac :rtg-math)
  :components ((:file "cepl/package")
               (:file "cepl/misc")))
