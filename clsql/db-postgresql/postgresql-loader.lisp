;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          postgresql-loader.sql
;;;; Purpose:       PostgreSQL library loader using UFFI
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id: postgresql-loader.lisp 959 2003-11-13 05:58:22Z kevin $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))
(in-package #+lispworks5 #:db-postgresql #-lispworks5 #:postgresql)


(defvar *postgresql-supporting-libraries* '("crypt" "c")
  "Used only by CMU. List of library flags needed to be passed to ld to
load the PostgresSQL client library succesfully.  If this differs at your site,
set to the right path before compiling or loading the system.")

(defvar *postgresql-library-loaded* nil
  "T if foreign library was able to be loaded successfully")

(defvar user::*postgresql-library-filename* "./psql.dll")

(defun load-postgresql-foreign-library (&optional force)
  (when force (setf *postgresql-library-loaded* nil))
    (unless *postgresql-library-loaded*
      (unless (probe-file user::*postgresql-library-filename*)
        (error "Unable to find ~s" user::*postgresql-library-filename*))
      (if (uffi:load-foreign-library user::*postgresql-library-filename*
                                     :module "postgresql"
                                     :supporting-libraries 
                                     *postgresql-supporting-libraries*)
          (setq *postgresql-library-loaded* t)
        (error "Can't load PostgreSQL client library ~A" libpath))))

(defmethod clsql-base-sys:database-type-library-loaded ((database-type
						    (eql :postgresql)))
  *postgresql-library-loaded*)

(defmethod clsql-base-sys:database-type-load-foreign ((database-type
						  (eql :postgresql)))
  (load-postgresql-foreign-library))

;(load-postgresql-foreign-library)

