;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compress.lisp
;;;; Purpose:       UFFI Example file for zlib compression
;;;; Author:        Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id: compress.lisp 454 2003-09-07 06:34:45Z kevin $
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002-2003 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(in-package #:uffi-tests)

(uffi:def-function ("compress" c-compress)
    ((dest (* :unsigned-char))
     (destlen (* :long))
     (source :cstring)
     (source-len :long))
  :returning :int
  :module "zlib")
  
(defun compress (source)
  "Returns two values: array of bytes containing the compressed data
 and the numbe of compressed bytes"
  (let* ((sourcelen (length source))
	 (destsize (+ 12 (ceiling (* sourcelen 1.01))))
	 (dest (uffi:allocate-foreign-string destsize :unsigned t))
	 (destlen (uffi:allocate-foreign-object :long)))
    (setf (uffi:deref-pointer destlen :long) destsize)
    (uffi:with-cstring (source-native source)
      (let ((result (c-compress dest destlen source-native sourcelen))
	    (newdestlen (uffi:deref-pointer destlen :long)))
	(unwind-protect
	    (if (zerop result)
		(values (uffi:convert-from-foreign-string 
			 dest
			 :length newdestlen
			 :null-terminated-p nil)
			newdestlen)
	      (error "zlib error, code ~D" result))
	  (progn
	    (uffi:free-foreign-object destlen)
	    (uffi:free-foreign-object dest)))))))

(uffi:def-function ("uncompress" c-uncompress)
    ((dest (* :unsigned-char))
     (destlen (* :long))
     (source :cstring)
     (source-len :long))
  :returning :int
  :module "zlib")

(defun uncompress (source)
  (let* ((sourcelen (length source))
	 (destsize 200000)  ;adjust as needed
	 (dest (uffi:allocate-foreign-string destsize :unsigned t))
	 (destlen (uffi:allocate-foreign-object :long)))
    (setf (uffi:deref-pointer destlen :long) destsize)
    (uffi:with-cstring (source-native source)
      (let ((result (c-uncompress dest destlen source-native sourcelen))
	    (newdestlen (uffi:deref-pointer destlen :long)))
	(unwind-protect
	     (if (zerop result)
		 (uffi:convert-from-foreign-string 
		  dest
		  :length newdestlen
		  :null-terminated-p nil)
		 (error "zlib error, code ~D" result))
	  (progn
	    (uffi:free-foreign-object destlen)
	    (uffi:free-foreign-object dest)))))))

(deftest compress.1 (map 'list #'char-code (compress ""))
  (120 156 3 0 0 0 0 1))
(deftest compress.2 (map 'list #'char-code  (compress "test"))
  (120 156 43 73 45 46 1 0 4 93 1 193))
(deftest compress.3 (map 'list #'char-code (compress "test2"))
  (120 156 43 73 45 46 49 2 0 6 80 1 243))

(defun compress-uncompress (str)
  (multiple-value-bind (compressed len) (compress str)
    (declare (ignore len))
    (multiple-value-bind (uncompressed len2) (uncompress compressed)
      (declare (ignore len2))
      uncompressed)))


(deftest uncompress.1 "" "")
(deftest uncompress.2 "test" "test")
(deftest uncompress.3 "test2" "test2")
