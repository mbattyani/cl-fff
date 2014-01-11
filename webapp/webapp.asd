; -*- mode:common-lisp -*-

(in-package #:asdf)

(defsystem :webapp
  :name "webapp"
  :author "Marc Battyani <marc.battyani@fractalconcept.com>"
  :maintainer "Marc Battyani <marc.battyani@fractalconcept.com>"
  :description "A webapp base layer to start using the webapp framework"
  :components ((:file "package")
               (:file "specials" :depends-on ("package"))
               (:file "pages" :depends-on ("specials"))
	       (:file "style" :depends-on ("pages")))
  :depends-on (:interface))
