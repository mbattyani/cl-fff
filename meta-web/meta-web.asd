;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package asdf)

(defsystem :meta-web
  :name "meta-web"
  :author "Marc Battyani <marc.battyani@fractalconcept.com>"
  :maintainer "Marc Battyani <marc.battyani@fractalconcept.com>"
  :description "Meta Interface for the Framework"
  :long-description "Meta Interface for the Framework"
  :components ((:file "global")
	       (:file "style" :depends-on ("global"))
	       (:file "classes" :depends-on ("global"))
	       (:file "util-classes" :depends-on ("classes"))
	       (:file "slot-info" :depends-on ("classes"))
	       (:file "view-info" :depends-on ("classes"))
	       (:file "class-info" :depends-on ("view-info" "slot-info"))
	       (:file "sql-list" :depends-on ("class-info"))
	       (:file "project-info" :depends-on ("class-info"))
	       (:file "upgrade" :depends-on ("project-info"))
	       (:file "upgrade-meta" :depends-on ("project-info"))
	       (:file "pages" :depends-on ("style")))
  :depends-on (:interface :utility :clsql :clsql-postgresql)
  )
