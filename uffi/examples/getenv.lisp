;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          getenv.cl
;;;; Purpose:       UFFI Example file to get environment variable
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id: getenv.lisp 454 2003-09-07 06:34:45Z kevin $
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; UFFI users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package :cl-user)


(uffi:def-function ("getenv" c-getenv) 
    ((name :cstring))
  :returning :cstring)

(defun my-getenv (key)
  "Returns an environment variable, or NIL if it does not exist"
  (check-type key string)
  (uffi:with-cstring (key-native key)
    (uffi:convert-from-cstring (c-getenv key-native))))
    
#+examples-uffi
(progn
  (flet ((print-results (str)
	   (format t "~&(getenv ~S) => ~S" str (my-getenv str))))
    (print-results "USER")
    (print-results "_FOO_")))


#+test-uffi
(progn
  (util.test:test (my-getenv "_FOO_") nil :fail-info "Error retrieving non-existent getenv")
  (util.test:test (and (stringp (my-getenv "USER"))
		       (< 0 (length (my-getenv "USER"))))
		  t :fail-info "Error retrieving getenv")
)

