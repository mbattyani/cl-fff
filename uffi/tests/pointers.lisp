;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          pointers.lisp
;;;; Purpose:       Test file for UFFI pointers
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: pointers.lisp 454 2003-09-07 06:34:45Z kevin $
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2003 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(in-package #:uffi-tests)

(deftest chptr.1
    (let ((native-string "test string"))
      (uffi:with-foreign-string (fs native-string)
	(characterp
	 (ensure-char-character
	  (deref-pointer fs :char)))))
  t)

(deftest chptr.2
    (let ((native-string "test string"))
      (uffi:with-foreign-string (fs native-string)
	(characterp
	 (ensure-char-character
	  (deref-pointer fs :unsigned-char)))))
  t)

(deftest chptr.3
    (let ((native-string "test string"))
      (uffi:with-foreign-string (fs native-string)
	(numberp
	 (deref-pointer fs :byte))))
  t)

	
