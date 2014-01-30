; -*- mode:common-lisp -*-

(in-package #:asdf)

(defsystem #:html
  :name "html"
  :author "Marc Battyani <marc.battyani@fractalconcept.com>"
  :maintainer "Marc Battyani <marc.battyani@fractalconcept.com>"
  :description "HTML generation macro and utilities"
  :long-description "HTML generation macro and utilities"
  :components ((:file "defpackage")
	       (:file "html-utils" :depends-on ("defpackage"))
	       (:file "parsers" :depends-on ("html-utils"))
	       (:file "html-gen2" :depends-on ("html-utils"))
	       (:file "func-tags" :depends-on ("html-gen2"))
	       )
  :depends-on (#:utility #:cl-html-parse #:parenscript)
  )
