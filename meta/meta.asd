;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package asdf)

(defsystem :meta
  :name "meta"
  :author "Marc Battyani <marc.battyani@fractalconcept.com>"
  :maintainer "Marc Battyani <marc.battyani@fractalconcept.com>"
  :description "Meta Level for the Framework"
  :long-description "Meta Level for the Framework"
  :components ((:file "defpackage")
	       (:file "specials" :depends-on ("defpackage"))
	       (:file "rules" :depends-on ("object"))
	       (:file "meta-class" :depends-on ("specials"))
	       (:file "slot" :depends-on ("meta-class"))
	       (:file "object" :depends-on ("meta-class" "slot"))
	       (:file "store" :depends-on ("object"))
	       (:file "psql-store" :depends-on ("object"))
	       (:file "class-info" :depends-on ("object"))
	       (:file "sql" :depends-on ("object"))
	       (:file "utilities" :depends-on ("object")))
  :depends-on (:utility :clsql :clsql-postgresql)
  )
