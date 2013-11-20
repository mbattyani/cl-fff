; -*- mode:common-lisp -*-

(in-package #:asdf)

(defsystem #:psql-store
  :name "psql-store"
  :author "Marc Battyani <marc.battyani@fractalconcept.com>"
  :maintainer "Marc Battyani <marc.battyani@fractalconcept.com>"
  :description "Postgresql back-end for the Framework using lispworks's SQL interface"
  :long-description "Postgresql back-end for the Framework using lispworks's SQL interface"
  :components ((:file "lw-psql")
               (:file "sql")
	       (:file "psql-store" :depends-on ("lw-psql" "sql"))
               (:file "lw-store" :depends-on ("psql-store")))
  :depends-on (:meta))
