;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          file-socket.cl
;;;; Purpose:       UFFI Example file to get a socket on a file
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Jul 2002
;;;;
;;;; $Id: file-socket.lisp 454 2003-09-07 06:34:45Z kevin $
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; UFFI users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package :cl-user)

;; Values for linux
(uffi:def-constant PF_UNIX 1)
(uffi:def-constant SOCK_STREAM 1)

(uffi:def-function ("socket" c-socket)
    ((family :int)
     (type :int)
     (protocol :int))
    :returning :int)

(uffi:def-function ("connect" c-connect)
    ((sockfd :int)
     (serv-addr :void-pointer)
     (addr-len :int))
    :returning :int)
		  
(defun connect-to-file-socket (filename)
  (let ((socket (c-socket PF_UNIX SOCK_STREAM 0)))
    (if (plusp socket)
	(let ((stream (c-connect socket filename (length filename))))
	  stream)
      (error "Unable to create socket"))))
