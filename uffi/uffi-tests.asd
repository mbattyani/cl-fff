;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          uffi-tests.asd
;;;; Purpose:       ASDF system definitionf for uffi testing package
;;;; Author:        Kevin M. Rosenberg
;;;; Date Started:  Apr 2003
;;;;
;;;; $Id: uffi-tests.asd 480 2003-11-11 22:41:57Z kevin $
;;;; *************************************************************************

(defpackage #:uffi-tests-system
  (:use #:asdf #:cl))
(in-package #:uffi-tests-system)

(defvar *library-file-dir* (append (pathname-directory *load-truename*)
				   (list "tests")))

(defclass uffi-test-source-file (c-source-file)
  ())

(defmethod output-files ((o compile-op) (c uffi-test-source-file))
  (let ((found (some #'(lambda (dir)
			    (probe-file (make-pathname :directory dir
						       :name (component-name c)
						       :type "so")))
			'((:absolute "usr" "lib" "uffi")))))
    (list (if found
	      found
	      (make-pathname :name (component-name c)
			     :type "so"
			     :directory *library-file-dir*)))))

(defmethod perform ((o load-op) (c uffi-test-source-file))
  nil) ;; lisp loader file will load library

(defmethod perform ((o compile-op) (c uffi-test-source-file))
  (unless (zerop (run-shell-command
		  "cd ~A; make"
		  (namestring (make-pathname :name nil
					     :type nil
					     :directory *library-file-dir*))))
    (error 'operation-error :component c :operation o)))


(defsystem uffi-tests
    :depends-on (:uffi)
    :components
    ((:module tests
	      :components
	      ((:file "rt")
	       (:uffi-test-source-file "uffi-c-test")
	       (:file "package" :depends-on ("rt"))
	       (:file "strtol" :depends-on ("package"))
	       (:file "atoifl" :depends-on ("package"))
	       (:file "getenv" :depends-on ("package"))
	       (:file "gethostname" :depends-on ("package"))
	       (:file "union" :depends-on ("package"))
	       (:file "arrays" :depends-on ("package"))
	       (:file "structs" :depends-on ("package"))
	       (:file "pointers" :depends-on ("package"))
	       (:file "time" :depends-on ("package"))
	       (:file "foreign-loader" :depends-on ("package" "uffi-c-test"))
	       (:file "uffi-c-test-lib" :depends-on ("foreign-loader"))
	       (:file "compress" :depends-on ("foreign-loader"))
	       (:file "casts" :depends-on ("foreign-loader"))
	       (:file "foreign-var" :depends-on ("foreign-loader"))
	       ))))

(defmethod perform ((o test-op) (c (eql (find-system :uffi-tests))))
  (or (funcall (intern (symbol-name '#:do-tests)
		       (find-package '#:regression-test)))
      (error "test-op failed")))
