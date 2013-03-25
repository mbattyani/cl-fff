;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package asdf)

(defsystem :psql-store
  :name "psql-store"
  :author "Marc Battyani <marc.battyani@fractalconcept.com>"
  :maintainer "Marc Battyani <marc.battyani@fractalconcept.com>"
  :description "Postgresql back-end for the Framework"
  :long-description "Postgresql back-end for the Framework"
  :components ((:file "lw-psql" :depends-on ())
	       (:file "psql-store" :depends-on ("lw-psql"))
	       (:file "sql" :depends-on ())
	       #+nil(:file "psql-export" :depends-on ("sql" "psql-store")))
  :depends-on (:meta))
