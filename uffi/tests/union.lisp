;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          union.lisp
;;;; Purpose:       UFFI Example file to test unions
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Mar 2002
;;;;
;;;; $Id: union.lisp 454 2003-09-07 06:34:45Z kevin $
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002-2003 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(in-package #:uffi-tests)

(uffi:def-union tunion1 
    (char :char)
  (int :int)
  (uint :unsigned-int)
  (sf :float)
  (df :double))

(defvar *u* (uffi:allocate-foreign-object 'tunion1))
(setf (uffi:get-slot-value *u* 'tunion1 'uint)
      #-(or sparc sparc-v9 powerpc ppc)
      (+ (* 1 (char-code #\A))
	 (* 256 (char-code #\B))
	 (* 65536 (char-code #\C))
	 (* 16777216 128))
      #+(or sparc sparc-v9 powerpc ppc)
      (+ (* 16777216 (char-code #\A))
	 (* 65536 (char-code #\B))
	 (* 256 (char-code #\C))
	 (* 1 128)))

(deftest union.1 (uffi:ensure-char-character 
		  (uffi:get-slot-value *u* 'tunion1 'char)) #\A)

#-(or sparc sparc-v9 mcl)
(deftest union.2 (plusp (uffi:get-slot-value *u* 'tunion1 'uint)) t)


;;    (uffi:free-foreign-object u))

