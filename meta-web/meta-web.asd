; -*- mode:common-lisp -*-

(in-package #:asdf)

(defsystem :meta-web
  :name "meta-web"
  :author "Marc Battyani <marc.battyani@fractalconcept.com>"
  :maintainer "Marc Battyani <marc.battyani@fractalconcept.com>"
  :description "Meta Web Interface for the Framework"
  :long-description "Meta Web Interface for the Framework"
  :components ((:file "defpackage")
               (:file "global" :depends-on ("defpackage"))
	       (:file "style" :depends-on ("global"))
	       (:file "meta-classes" :depends-on ("global"))
	       (:file "util-classes" :depends-on ("meta-classes"))
	       (:file "slot-info" :depends-on ("meta-classes"))
	       (:file "view-info" :depends-on ("meta-classes"))
	       (:file "class-info" :depends-on ("view-info" "slot-info"))
	       (:file "sql-list" :depends-on ("class-info"))
	       (:file "project-info" :depends-on ("class-info"))
	       (:file "upgrade" :depends-on ("project-info"))
	       (:file "gen-doc" :depends-on ("meta-classes"))
	       (:file "pages" :depends-on ("style")))
  :depends-on (:webapp :cl-typegraph :s-dot :postmodern-store))
