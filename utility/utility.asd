;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package asdf)

(defsystem :utility
  :name "utility"
  :author "Marc Battyani <marc.battyani@fractalconcept.com>"
  :maintainer "Marc Battyani <marc.battyani@fractalconcept.com>"
  :description "Meta Interface for the Framework"
  :long-description "Meta Interface for the Framework"
  :components ((:file "defpackage")
	       (:file "utility" :depends-on ("defpackage"))
	       (:file "infix" :depends-on ("defpackage"))
	       (:file "string" :depends-on ("defpackage"))
	       (:file "log-errors" :depends-on ("utility")))
  :depends-on (:split-sequence)
  )
