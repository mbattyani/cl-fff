;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          arrays.cl
;;;; Purpose:       UFFI Example file to test arrays
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Mar 2002
;;;;
;;;; $Id: arrays.lisp 454 2003-09-07 06:34:45Z kevin $
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; UFFI users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package :cl-user)

(uffi:def-constant +column-length+ 10)
(uffi:def-constant +row-length+ 10)

(uffi:def-foreign-type long-ptr '(* :long))

(defun test-array-1d ()
  "Tests vector"
  (let ((a (uffi:allocate-foreign-object :long +column-length+)))
    (dotimes (i +column-length+)
      (setf (uffi:deref-array a '(:array :long) i) (* i i)))
    (dotimes (i +column-length+)
      (format t "~&~D => ~D" i (uffi:deref-array a '(:array :long) i)))
    (uffi:free-foreign-object a))
  (values))

(defun test-array-2d ()
  "Tests 2d array"
  (let ((a (uffi:allocate-foreign-object 'long-ptr +row-length+)))
    (dotimes (r +row-length+)
      (declare (fixnum r))
      (setf (uffi:deref-array a '(:array (* :long)) r)
	    (uffi:allocate-foreign-object :long +column-length+))
      (let ((col (uffi:deref-array a '(:array (* :long)) r)))
	(dotimes (c +column-length+)
	  (declare (fixnum c))
	  (setf (uffi:deref-array col '(:array :long) c) (+ (* r +column-length+) c)))))

    (dotimes (r +row-length+)
      (declare (fixnum r))
      (format t "~&Row ~D: " r)
      (let ((col (uffi:deref-array a '(:array (* :long)) r)))
	(dotimes (c +column-length+)
	  (declare (fixnum c))
	  (let ((result (uffi:deref-array col '(:array :long) c)))
	    (format t "~d " result)))))

    (uffi:free-foreign-object a))
  (values))

#+examples-uffi
(test-array-1d)

#+examples-uffi
(test-array-2d)


