;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-USER; -*-

(in-package "CL-USER")

(defun ensure-asdf ()
  #+asdf (return-from ensure-asdf t)
  #+sbcl (require :asdf)
  #-sbcl (let ((asdf-pathname
                (merge-pathnames (make-pathname
                                  :directory '(:relative "libs")
                                  :name "asdf"
                                  :case :local)
                                 *load-truename*)))
           (load asdf-pathname)))

(ensure-asdf)

(progn
  (flet ((find-or-load-system (system path)
           (let ((path (merge-pathnames path *load-truename*)))
             (unless (asdf:find-system system nil)
               (load path)))))
    (find-or-load-system :puri
                         (make-pathname
                          :directory '(:relative "libs" "puri-1.3.1")
                          :name "puri" :type "asd" :case :local))
    (find-or-load-system :cl-ppcre
                         (make-pathname
                          :directory '(:relative "libs" "cl-ppcre")
                          :name "cl-ppcre" :type "asd" :case :local))
    (find-or-load-system :acl-compat
                         (make-pathname
                          :directory '(:relative "acl-compat")
                          :name "acl-compat" :type "asd" :case :local))
    (find-or-load-system :htmlgen
                         (make-pathname
                          :directory '(:relative "aserve" "htmlgen")
                          :name "htmlgen" :type "asd" :case :local))
    (find-or-load-system :aserve
                         (make-pathname
                          :directory '(:relative "aserve")
                          :name "aserve" :type "asd" :case :local))
    (find-or-load-system :webactions
                         (make-pathname
                          :directory '(:relative "aserve" "webactions")
                          :name "webactions" :type "asd" :case :local)))
  ;; Compile and load the ASERVE system
  (asdf:operate 'asdf:load-op :aserve)
  (asdf:operate 'asdf:load-op :webactions)

  ;; Startup multiprocessing.
  ;;
  ;; this isn't strictly necessary, but scheduling feels very coarse
  ;; before the evaluation of (startup-idle-and-top-level-loops) --
  ;; answer delays of about 1s per http request.
  ;;
  ;; KLUDGE: startup-idle-and-top-level-loops can only be evaluated
  ;; once, so we look for something resembling an existing idle loop
  ;; before invoking it.
  #||
  #+mp
  (unless (find-if
           #'(lambda (proc) (string= (mp:process-name proc) "Idle Loop"))
           (mp:all-processes))
    (mp::startup-idle-and-top-level-loops))
  ||#
  ;; DOUBLE KLUDGE: The preceding (commented-out) form caused the
  ;; loading of INSTALL.lisp to abort silently (!), so we do the
  ;; following, pilfered from eclipse the window manager:
  #+(and cmu mp)
  (setf mp::*idle-process* mp::*initial-process*)

  )


#||
;;; To test the installation, evaluate the following:

;;; Load example.lisp in the aserve directory.
(load "aserve:example.cl")

;;; Select example package
(in-package :aserve-example)

;;; This option enables extended debug message output
(net.aserve::debug-on :info)

;;; This option enables to enter the debugger if an error
;;; occurs. (instead of simply logging and ignoring it)
(net.aserve::debug-on :notrap)

;;; Start example server (in multiprocessing) on port 2001
(start-server :port 2001)

;MCL/OpenMCL note: chunking is not yet implemented so use (start-server :port 2001 :chunking nil)
||#
