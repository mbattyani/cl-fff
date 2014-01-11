; -*- mode:common-lisp -*-

(in-package #:asdf)

(defsystem :webapp
  :name "webapp"
  :author "Marc Battyani <marc.battyani@fractalconcept.com>"
  :maintainer "Marc Battyani <marc.battyani@fractalconcept.com>"
  :description "A webapp base layer to start using the webapp framework"
  :components ((:file "package")
               (:file "global" :depends-on ("package"))
	       (:file "style" :depends-on ("global"))
	       (:file "pages" :depends-on ("style")))
  :depends-on (:interface))
