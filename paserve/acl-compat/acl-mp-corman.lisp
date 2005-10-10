;;; This file implements the process functions for AllegroServe in Corman Lisp.

(require 'mp)

(defpackage :acl-compat-mp
  (:use :common-lisp :mp :sys)
  (:export
   #:process-interrrupt
   #:make-process
   #:make-process-lock
   #:process-add-run-reason
   #:process-kill
   #:process-property-list
   #:process-revoke-run-reason
   #:process-run-function
   #:with-process-lock
   #:with-timeout
   #:without-scheduling
   #:*current-process*
   #:lock
   #:process-allow-schedule
   #:process-name
   #:process-preset
   #:process-run-reasons
   #:process-wait
   #:without-interrupts
   ))

(in-package :acl-compat-mp)

; existing stuff from ccl we can reuse directly
;; The following process-property-list implementation was taken from
;; the acl-mp-scl.lisp implementation.
(defvar *process-plists* (make-hash-table :test #'eq)
  "maps processes to their plists.
See the functions process-plist, (setf process-plist).")

(defun process-property-list (process)
  (gethash process *process-plists*))

(defun (setf process-property-list) (new-value process)
  (setf (gethash process *process-plists*) new-value))

;; Dummy implementation of process-wait
(defun process-wait (whostate function &rest args)
    "This function suspends the current process (the value of sys:*current-process*) 
    until applying function to arguments yields true. The whostate argument must be a 
    string which temporarily replaces the process' whostate for the duration of the wait. 
    This function returns nil."
    (loop until (apply function args) do (sleep 0))
    nil)
    
