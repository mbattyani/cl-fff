; -*- mode:common-lisp -*-

(in-package #:asdf)

(defsystem #:postmodern-store
  :name "postmodern-store"
  :author "Michał Psota"
  :description "Postmodern back-end for the Framework"
  :components ((:file "postmodern")
               (:file "sql")
	       (:file "psql-store" :depends-on ("postmodern" "sql"))
               (:file "postmodern-store" :depends-on ("psql-store")))
  :depends-on (:meta :postmodern))
