;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          getshells.cl
;;;; Purpose:       UFFI Example file to get lisp of legal shells
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Mar 2002
;;;;
;;;; $Id: getshells.lisp 454 2003-09-07 06:34:45Z kevin $
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; UFFI users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package :cl-user)


(uffi:def-function "setusershell"
    nil
  :returning :void)

(uffi:def-function "endusershell"
    nil
  :returning :void)

(uffi:def-function "getusershell"
    nil
  :returning :cstring)

(defun getshells ()
  "Returns list of valid shells"
  (setusershell)
  (let (shells)
    (do ((shell (uffi:convert-from-cstring (getusershell))
                (uffi:convert-from-cstring (getusershell))))
	((null shell))
      (push shell shells))
    (endusershell)
    (nreverse shells)))

#+examples-uffi
(format t "~&Shells: ~S" (getshells))

