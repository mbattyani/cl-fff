;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package asdf)

(defsystem :interface
  :name "interface"
  :author "Marc Battyani <marc.battyani@fractalconcept.com>"
  :maintainer "Marc Battyani <marc.battyani@fractalconcept.com>"
  :description "General HTML interface for the Framework"
  :long-description "General HTML interface for the Framework"
  :components ((:file "defpackage")
	       (:file "specials" :depends-on ("defpackage"))
	       (:file "sql-lists" :depends-on ("session" "dispatcher"))
	       (:file "request" :depends-on ("apache" "http"))
	       (:file "session" :depends-on ("request"))
	       (:file "process-url" :depends-on ("session"))
	       (:file "http-link" :depends-on ("session"))
	       (:file "view" :depends-on ("html-view"))
	       (:file "ui-desc" :depends-on ("specials"))
	       (:file "dispatcher" :depends-on ("ui-desc"))
	       (:file "ui-html" :depends-on ("ui-desc"))
	       (:file "html-view" :depends-on ("ui-html" "dispatcher"))
	       (:file "html-slot-view" :depends-on ("html-view"))
	       (:file "http" :depends-on ("specials"))
	       (:file "rpc" :depends-on ("specials"))
	       (:file "std-web" :depends-on ("specials"))
	       (:file "apache" :depends-on ("specials")))
  :depends-on (:utility :iterate :meta :html)
  )
