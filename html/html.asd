;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package asdf)

(defsystem :html
  :name "html"
  :author "Marc Battyani <marc.battyani@fractalconcept.com>"
  :maintainer "Marc Battyani <marc.battyani@fractalconcept.com>"
  :description "HTML generation macro and utilities"
  :long-description "HTML generation macro and utilities"
  :components ((:file "defpackage")
	       (:file "html-utils" :depends-on ("defpackage"))
	       (:file "http-client" :depends-on ("html-utils"))
	       (:file "parsers" :depends-on ("html-utils"))
	       (:file "html-gen2" :depends-on ("html-utils"))
	       (:file "func-tags" :depends-on ("html-gen2"))
	       (:file "base64" :depends-on ("defpackage"))
	       (:file "sha" :depends-on ("defpackage"))
	       (:file "franz-if" :depends-on ("defpackage"))
	       (:file "html-parse" :depends-on ("franz-if")))
  :depends-on (:utility)
  )
