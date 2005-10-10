;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          foreign-loader.lisp
;;;; Purpose:       Loads foreign libraries
;;;; Author:        Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id: foreign-loader.lisp 477 2003-11-11 14:58:32Z kevin $
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

;;; For CMUCL, it's necessary to load foreign files separate from their
;;; usage

(in-package uffi-tests)

(unless (uffi:load-foreign-library
	 (uffi:find-foreign-library
	  #-(or macosx darwin)
	  "libz"
	  #+(or macosx darwin)
	  "z"
	  (list (pathname-directory *load-pathname*)
		"/usr/local/lib/" "/usr/lib/" "/zlib/"))
	 :module "zlib" 
	 :supporting-libraries '("c"))
  (warn "Unable to load zlib"))
  
(unless (uffi:load-foreign-library 
	 (uffi:find-foreign-library
	  "uffi-c-test" 
	  (list (pathname-directory *load-truename*)
		"/usr/lib/uffi/"))
	 :supporting-libraries '("c")
	 :module "uffi_tests")
  (warn "Unable to load uffi-c-test library"))

