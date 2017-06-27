;;;; package.lisp

(uiop:define-package #:cepl.issac
    (:use #:cl #:cepl #:vari #:rtg-math :%rtg-math #:issac)
  (:import-from #:alexandria :with-gensyms))
