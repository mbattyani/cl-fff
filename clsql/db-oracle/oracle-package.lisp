;;; -*- Mode: Lisp -*-
;;; $Id: oracle-package.lisp 936 2003-09-07 06:34:45Z kevin $
;;;
;;; This is copyrighted software.  See documentation for terms.

(in-package :cl-user)

(defpackage :clsql-oracle
  (:nicknames :oracle)
  (:use :common-lisp :clsql-sys "ALIEN" "C-CALL" "SYSTEM")
  (:export #:oracle-database
	   #:*oracle-so-load-path*
	   #:*oracle-so-libraries*)
  (:documentation "This is the CLSQL interface to Oracle."))
