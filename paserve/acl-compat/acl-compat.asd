;;;; -*- mode: lisp -*-
;;;;
;;;; This as an ASDF system for ACL-COMPAT, meant to replace
;;;; acl-compat-cmu.system, but could replace all other systems, too.
;;;; (hint, hint)

(defpackage #:acl-compat-system
  (:use #:cl #:asdf))
(in-package #:acl-compat-system)

;;;; gray stream support for cmucl: Debian/common-lisp-controller has
;;;; a `cmucl-graystream' system; if this is not found, we assume a
;;;; cmucl downloaded from cons.org, where Gray stream support resides
;;;; in the subsystems/ directory.


#+cmu
(progn

(defclass precompiled-file (static-file)
  ())

(defmethod perform ((operation load-op) (c precompiled-file))
  (load (component-pathname c)))

(defmethod operation-done-p ((operation load-op) (c precompiled-file))
  nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (asdf:find-system :cmucl-graystream nil)
    (asdf:defsystem cmucl-graystream
        :pathname (make-pathname
                   :name nil :type nil :version nil
                   :defaults (truename "library:subsystems/gray-streams-library.x86f"))
      :components ((:precompiled-file "gray-streams-library.x86f")))))
)

;;;; ignore warnings
;;;;
;;;; FIXME: should better fix warnings instead of ignoring them
;;;; FIXME: (perform legacy-cl-sourcefile) duplicates ASDF code

(defclass legacy-cl-source-file (cl-source-file)
    ()
  (:documentation
   "Common Lisp source code module with (non-style) warnings.
In contrast to CL-SOURCE-FILE, this class does not think that such warnings
indicate failure."))

(defmethod perform ((operation compile-op) (c legacy-cl-source-file))
  (let ((source-file (component-pathname c))
	(output-file (car (output-files operation c)))
	(warnings-p nil)
	(failure-p nil))
    (setf (asdf::component-property c 'last-compiled) nil)
    (handler-bind ((warning (lambda (c)
			      (declare (ignore c))
			      (setq warnings-p t)))
		   ;; _not_ (or error (and warning (not style-warning)))
		   (error (lambda (c)
			    (declare (ignore c))
			    (setq failure-p t))))
      (compile-file source-file
		    :output-file output-file))
    ;; rest of this method is as for CL-SOURCE-FILE
    (setf (asdf::component-property c 'last-compiled) (file-write-date output-file))
    (when warnings-p
      (case (asdf::operation-on-warnings operation)
	(:warn (warn "COMPILE-FILE warned while performing ~A on ~A"
		     c operation))
	(:error (error 'compile-warned :component c :operation operation))
	(:ignore nil)))
    (when failure-p
      (case (asdf::operation-on-failure operation)
	(:warn (warn "COMPILE-FILE failed while performing ~A on ~A"
		     c operation))
	(:error (error 'compile-failed :component c :operation operation))
	(:ignore nil)))))

;;;
;;; This is thought to reduce reader-conditionals in the system definition
;;;
(defclass unportable-cl-source-file (cl-source-file) ()
  (:documentation
   "This is for files which contain lisp-system dependent code. Until now those
are marked by a -system postfix but we could later change that to a directory per
lisp-system"))

(defmethod perform ((op load-op) (c unportable-cl-source-file))
  (#+cmu ext:without-package-locks
   #-(or cmu) progn
     (call-next-method)))

(defmethod perform ((op compile-op) (c unportable-cl-source-file))
  (#+cmu ext:without-package-locks
   #-(or cmu) progn
     (call-next-method)))

(defmethod source-file-type ((c unportable-cl-source-file) (s module))
  "lisp")


(defun lisp-system-shortname ()
  #+allegro :allegro #+lispworks :lispworks #+cmu :cmucl
  #+mcl :mcl #+clisp :clisp #+scl :scl #+sbcl :sbcl) ;mcl/openmcl use the same directory

(defmethod component-pathname ((component unportable-cl-source-file))
  (let ((pathname (call-next-method))
        (name (string-downcase (lisp-system-shortname))))
    (merge-pathnames
     (make-pathname :directory (list :relative name))
     pathname)))

;;;; system

#+(and mcl (not openmcl)) (require :ansi-make-load-form)

(defsystem acl-compat
    :name "acl-compat"
    :author "The acl-compat team"
    :version "0.1.1"
    :description
    "A reimplementation of parts of the ACL API, mainly to get
    AllegroServe running on various machines, but might be useful
    in other projects as well."
    :properties
    ((("system" "author" "email") . "portableaserve-discuss@lists.sourceforge.net")
     (("albert" "presentation" "output-dir") . "docs/")
     (("albert" "presentation" "formats") . "docbook")
     (("albert" "docbook" "dtd") . "/Users/Shared/DocBook/lib/docbook/docbook-dtd-412/docbookx.dtd")
     (("albert" "docbook" "template") . "book"))
    :components
    (
     ;; packages
     (:file "packages")
     ;; Our stream class; support for buffering, chunking and (in the
     ;; future) unified stream exceptions
     #-(or lispworks (and mcl (not openmcl)))
     (:file "lw-buffering" :depends-on ("packages"))
     #-(or allegro (and mcl (not openmcl)))
     (:legacy-cl-source-file "chunked-stream-mixin"
                             :depends-on ("packages" "acl-excl"
                                                     #-lispworks "lw-buffering"))
     ;; Multiprocessing
     #+mcl (:unportable-cl-source-file "mcl-timers")
     (:unportable-cl-source-file "acl-mp"
                                 :depends-on ("packages" #+mcl "mcl-timers"))
     ;; Sockets, networking; TODO: de-frob this a bit
     #-mcl
     (:unportable-cl-source-file
      "acl-socket" :depends-on ("packages" "acl-excl"
                                           #-(or allegro (and mcl (not openmcl))) "chunked-stream-mixin"))
     #+(and mcl (not openmcl))
     (:unportable-cl-source-file "acl-socket-mcl" :depends-on ("packages"))
     #+(and mcl (not openmcl) (not carbon-compat)) 
     (:unportable-cl-source-file
      "mcl-stream-fix" :depends-on ("acl-socket-mcl"))
     #+(and mcl openmcl)
     (:unportable-cl-source-file
      "acl-socket-openmcl" :depends-on ("packages" "chunked-stream-mixin"))
     ;; Diverse macros, utility functions
     #-allegro (:file "acl-excl-common" :depends-on ("packages"))
     (:unportable-cl-source-file "acl-excl" :depends-on
                                 #-allegro ("acl-excl-common")
                                 #+allegro ("packages"))
     (:unportable-cl-source-file "acl-sys" :depends-on ("packages"))
     ;; SSL
     #+(and ssl-available (not (or allegro mcl clisp)))
     (:file "acl-ssl" :depends-on ("acl-ssl-streams" "acl-socket"))
     #+(and ssl-available (not (or allegro mcl clisp)))
     (:file "acl-ssl-streams" :depends-on ("packages")))
    ;; Dependencies
    :depends-on (:puri
                 :cl-ppcre
                 #+sbcl :sb-bsd-sockets
                 #+sbcl :sb-posix
                 #+cmu :cmucl-graystream
                 #+(and (or cmu lispworks) ssl-available) :cl-ssl
                 )
    :perform (load-op :after (op acl-compat)
                      (pushnew :acl-compat cl:*features*)))
