;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package file uffi testing suite
;;;; Author:        Kevin M. Rosenberg
;;;; Date Started:  Apr 2003
;;;;
;;;; $Id: package.lisp 454 2003-09-07 06:34:45Z kevin $
;;;; *************************************************************************

(defpackage #:uffi-tests
  (:use #:asdf #:cl #:uffi #:rtest)
  (:shadowing-import-from #:uffi #:run-shell-command))

(in-package #:uffi-tests)

