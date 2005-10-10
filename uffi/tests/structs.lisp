;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          structs.lisp
;;;; Purpose:       Test file for UFFI structures
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id: structs.lisp 454 2003-09-07 06:34:45Z kevin $
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002-2003 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(in-package #:uffi-tests)

;; Compilation failure as reported by Edi Weitz


(uffi:def-struct foo
    (bar :pointer-self))

(uffi:def-foreign-type foo-ptr (* foo))

;; tests that compilation worked
(deftest structs.1 
  (with-foreign-object (p 'foo)
    t)
  t)

