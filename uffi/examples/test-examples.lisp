;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          test-examples.cl
;;;; Purpose:       Load and execute all examples for UFFI
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id: test-examples.lisp 454 2003-09-07 06:34:45Z kevin $
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; UFFI users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

#-uffi (asdf:oos 'asdf:load-op :uffi)

(unless (ignore-errors (find-package :util.test))
  (load (make-pathname :name "acl-compat-tester" :defaults *load-truename*)))

(defun do-tests ()
  (pushnew :test-uffi cl:*features*)
  (util.test:with-tests (:name "UFFI-Tests")
    (setq util.test:*break-on-test-failures* nil)
    (flet ((load-test (name)
		      (load (make-pathname :name name :defaults *load-truename*))))
      (load-test "c-test-fns")
      (load-test "arrays")
      (load-test "union")
      (load-test "strtol")
      (load-test "atoifl")
      (load-test "gettime")
      (load-test "getenv")
      (load-test "gethostname")
      (load-test "getshells")
      (load-test "compress"))
    (setq cl:*features* (remove :test-uffi cl:*features*))))

(do-tests)

