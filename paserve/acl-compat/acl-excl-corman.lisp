;;;;
;;;; ACL-COMPAT - EXCL
;;;;
;;;; This is a modified version of Chris Doubles ACL excl wrapper library
;;;; As stated in the changelogs of his original this file includes the 
;;;; IF* macro placed in the public domain by John Foderaro. 
;;;; See: http://www.franz.com/~jkf/ifstar.txt
;;;;
;;;; It is not clear to this point if future releases will lead to a combined
;;;; effort - So you may find newer versions of *this* file at 
;;;; http://www.dataheaven.de
;;;;

;;;; This is the header of Chris Doubles original file. (but without Changelog)
;;;;
;;;; ACL excl wrapper library for Corman Lisp - Version 1.1
;;;;
;;;; Copyright (C) 2000 Christopher Double. All Rights Reserved.
;;;; 
;;;; License
;;;; =======
;;;; This software is provided 'as-is', without any express or implied
;;;; warranty. In no event will the author be held liable for any damages
;;;; arising from the use of this software.
;;;;
;;;; Permission is granted to anyone to use this software for any purpose,
;;;; including commercial applications, and to alter it and redistribute
;;;; it freely, subject to the following restrictions:
;;;;
;;;; 1. The origin of this software must not be misrepresented; you must
;;;;    not claim that you wrote the original software. If you use this
;;;;    software in a product, an acknowledgment in the product documentation
;;;;    would be appreciated but is not required.
;;;;
;;;; 2. Altered source versions must be plainly marked as such, and must
;;;;    not be misrepresented as being the original software.
;;;;
;;;; 3. This notice may not be removed or altered from any source 
;;;;    distribution.
;;;;
;;;; Notes
;;;; =====
;;;; A simple implementation of some of the EXCL package from Allegro
;;;; Common Lisp. Intended to be used for porting various ACL packages,
;;;; like AllegroServe. 
;;;;
;;;; More recent versions of this software may be available at:
;;;;   http://www.double.co.nz/cl
;;;;
;;;; Comments, suggestions and bug reports to the author, 
;;;; Christopher Double, at: chris@double.co.nz

(require 'nregex)
(require 'mp)

(defpackage :excl
	(:use :common-lisp :nregex)
        (:import-from :common-lisp "FIXNUMP")
	(:export 
		"IF*"
		"*INITIAL-TERMINAL-IO*"
		"*CL-DEFAULT-SPECIAL-BINDINGS*"
		"FILESYS-SIZE"
		"FILESYS-WRITE-DATE"
		"STREAM-INPUT-FN"
		"MATCH-REGEXP"
		"COMPILE-REGEXP"
		"*CURRENT-CASE-MODE*"
		"INTERN*"
		"FILESYS-TYPE"
		"ERRORSET"
		"ATOMICALLY"
		"FAST"
                "WITHOUT-PACKAGE-LOCKS"
		"SOCKET-ERROR"
                "RUN-SHELL-COMMAND"
                "FIXNUMP"
		))

(in-package :excl)

(defvar if*-keyword-list '("then" "thenret" "else" "elseif"))

(defmacro if* (&rest args)
   (do ((xx (reverse args) (cdr xx))
	(state :init)
	(elseseen nil)
	(totalcol nil)
	(lookat nil nil)
	(col nil))
       ((null xx)
	(cond ((eq state :compl)
	       `(cond ,@totalcol))
	      (t (error "if*: illegal form ~s" args))))
       (cond ((and (symbolp (car xx))
		   (member (symbol-name (car xx))
			   if*-keyword-list
			   :test #'string-equal))
	      (setq lookat (symbol-name (car xx)))))

       (cond ((eq state :init)
	      (cond (lookat (cond ((string-equal lookat "thenret")
				   (setq col nil
					 state :then))
				  (t (error
				      "if*: bad keyword ~a" lookat))))
		    (t (setq state :col
			     col nil)
		       (push (car xx) col))))
	     ((eq state :col)
	      (cond (lookat
		     (cond ((string-equal lookat "else")
			    (cond (elseseen
				   (error
				    "if*: multiples elses")))
			    (setq elseseen t)
			    (setq state :init)
			    (push `(t ,@col) totalcol))
			   ((string-equal lookat "then")
			    (setq state :then))
			   (t (error "if*: bad keyword ~s"
					      lookat))))
		    (t (push (car xx) col))))
	     ((eq state :then)
	      (cond (lookat
		     (error
		      "if*: keyword ~s at the wrong place " (car xx)))
		    (t (setq state :compl)
		       (push `(,(car xx) ,@col) totalcol))))
	     ((eq state :compl)
	      (cond ((not (string-equal lookat "elseif"))
		     (error "if*: missing elseif clause ")))
	      (setq state :init)))))

(defvar *initial-terminal-io* *terminal-io*)
(defvar *cl-default-special-bindings* nil)

(defun filesys-size (stream)
	(file-length stream))

(defun filesys-write-date (stream)
	(file-write-date stream))

#+obsolete
(defun stream-input-fn (stream)
  stream)

(defmethod stream-input-fn ((stream stream))
  stream)
	

(defun match-regexp (pattern string &key (return :string))
  (let ((res (cond ((stringp pattern)
		    (regex pattern string))
		   ((functionp pattern) (funcall pattern string))
		   (t (error "Wrong type for pattern")))))
    (case return
      (:string
       (values-list (cons (not (null res))
                          res)))
      (:index (error "REGEXP: INDEX Not implemented"))
      (otherwise (not (null res))))))

(defun compile-regexp (regexp)
  (compile nil (regex-compile regexp)))

(defvar *current-case-mode* :case-insensitive-upper)

(defun intern* (s len package)
  (intern (subseq s 0 len) package))

(defun filesys-type (file-or-directory-name)
	(if (ccl::directory-p file-or-directory-name)
		:directory
		(if (probe-file file-or-directory-name)
			:file
			nil)))

(defmacro errorset (form &optional (announce nil) (catch-breaks nil))
  "This macro is incomplete.  It was hacked to get AllegroServe
running, but the announce and catch-breaks arguments are ignored.  See
documentation at
http://franz.com/support/documentation/6.1/doc/pages/operators/excl/errorset.htm
An implementation of the catch-breaks argument will necessarily be
implementation-dependent, since Ansi does not allow any
program-controlled interception of a break."
  (declare (ignore announce catch-breaks))
  `(let* ((ok nil)
          (results (ignore-errors
                     (prog1 (multiple-value-list ,form)
                       (setq ok t)))))
     (if ok
         (apply #'values t results)
         nil)))


(defmacro atomically (&body forms)
  `(mp:without-scheduling ,@forms))

(defmacro fast (&body forms)
  `(locally (declare (optimize (speed 3) (safety 0) (debug 0)))
	    ,@forms))

(defmacro without-package-locks (&body forms)
  `(progn ,@forms))

(define-condition socket-error (error)
  ((stream :initarg :stream)
   (code :initarg :code :initform nil)
   (action :initarg :action)
   (identifier :initarg :identifier :initform nil))
  (:report (lambda (e s)
	     (with-slots (identifier code action stream) e
	       (format s "~S (errno ~A) occured while ~A"
		       (case identifier
			 (:connection-refused "Connection refused")
			 (t identifier))
		       code action)
	       (when stream
		 (prin1 stream s))
	       (format s ".")))))

#|
(defun run-shell-command ()
  (with-open-stream (s (open-pipe "/bin/sh"
                                  :direction :io
                                  :buffered nil))
    (loop for var in environment
          do (format stream "~A=~A~%" (car var) (cdr var)))
|#
  

(provide 'acl-excl)
