;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package asdf)

(defsystem :pdf-parse
  :name "pdf-parse"
  :author "Marc Battyani <marc.battyani@fractalconcept.com>"
  :maintainer "Marc Battyani <marc.battyani@fractalconcept.com>"
  :description "Pdf parser"
  :long-description "Pdf parser"
  :components ((:file "defpackage")
	       (:file "pdf-parse" :depends-on ("defpackage")))
  :depends-on (:cl-pdf)
  )
