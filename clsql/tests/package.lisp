;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package file clsql testing suite
;;;; Author:        Kevin M. Rosenberg
;;;; Date Started:  Apr 2003
;;;;
;;;; $Id: package.lisp 955 2003-11-11 16:24:09Z kevin $
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage #:clsql-tests
  (:use #:asdf #:cl #:clsql #:ptester))


