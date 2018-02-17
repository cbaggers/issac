;;;; package.lisp

(uiop:define-package #:cepl.issac
    (:use #:cl #:cepl #:vari #:rtg-math :cepl.defn #:issac)
  (:import-from #:alexandria :with-gensyms))
