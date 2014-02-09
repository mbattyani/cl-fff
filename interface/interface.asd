; -*- mode:common-lisp -*-

(in-package #:asdf)

(defsystem #:interface
  :name "interface"
  :author "Marc Battyani <marc.battyani@fractalconcept.com>"
  :maintainer "Marc Battyani <marc.battyani@fractalconcept.com>"
  :description "General HTML interface for the Framework"
  :long-description "General HTML interface for the Framework"
  :components ((:file "defpackage")
	       (:file "specials" :depends-on ("defpackage"))
               (:file "http" :depends-on ("specials"))
               (:file "request" :depends-on ("http"))
               (:file "session" :depends-on ("request"))
               (:file "process-url" :depends-on ("session"))
               (:file "apache" :depends-on ("process-url"))
               (:file "hunchentoot" :depends-on ("process-url"))
               ;;;; (:file "rpc" :depends-on ("log" "request" "specials")) not used anymore?
               (:file "localize" :depends-on ("process-url"))
               (:file "sql-lists" :depends-on ("session" #+nil "dispatcher"))

               (:file "view" :depends-on ("specials"))
               (:file "pane" :depends-on ("specials"))
	       (:file "http-link" :depends-on ("session" "view" #+nil "view-layout"))
	       (:file "ui-desc" :depends-on ("pane"))
               (:file "ui-html" :depends-on ("ui-desc" #+nil "view-layout"))
	       (:file "dispatcher" :depends-on ("ui-desc" "ui-html"))
	       (:file "html-view" :depends-on ("ui-html" "dispatcher" #+nil "view-layout" #+nil "layouts" #+nil "pane-utilities"))
	       (:file "bs-view" :depends-on ("html-view"))
	       (:file "clipboard" :depends-on ("html-view" "view"))
               (:file "html-slot-view" :depends-on ("html-view" "process-url" "clipboard"))
               (:file "bs-slot-view" :depends-on ("bs-view" "html-slot-view"))
	       (:file "std-web" :depends-on ("specials"))
               (:file "clws-link" )
               )
  :depends-on (#:iterate #:bordeaux-threads #:meta #:html #:clws #:cl-json #:usocket #:babel #:closer-mop #:hunchentoot #:log4cl))
