;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-USER; -*-
;;;;										;
;;;; (c) 2001 by Jochen Schmidt.
;;;;
;;;; File:            acl-mp-lw.lisp
;;;; Revision:        1.0.0
;;;; Description:     LispWorks implementation for ACL-COMPAT-MP
;;;; Date:            02.02.2002
;;;; Authors:         Jochen Schmidt
;;;; Tel:             (+49 9 11) 47 20 603
;;;; Email:           jsc@dataheaven.de
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;; 1. Redistributions of source code must retain the above copyright
;;;;    notice, this list of conditions and the following disclaimer.
;;;; 2. Redistributions in binary form must reproduce the above copyright
;;;;    notice, this list of conditions and the following disclaimer in the
;;;;    documentation and/or other materials provided with the distribution.
;;;;
;;;; THIS SOFTWARE IS PROVIDED "AS IS" AND THERE ARE NEITHER 
;;;; EXPRESSED NOR IMPLIED WARRANTIES -  THIS INCLUDES, BUT 
;;;; IS NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
;;;; AND FITNESS FOR A PARTICULAR PURPOSE.IN NO WAY ARE THE
;;;; AUTHORS LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;; SPECIAL, EXEMPLARY OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES ;
;;;; LOSS OF USE, DATA, OR PROFITS			; OR BUSINESS INTERRUPTION)
;;;; 
;;;; For further details contact the authors of this software.
;;;;
;;;;  Jochen Schmidt        
;;;;  Zuckmantelstr. 11     
;;;;  91616 Neusitz         
;;;;  GERMANY               
;;;;
;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "comm"))

(in-package :acl-compat-mp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Import equivalent parts from the LispWorks MP package ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(shadowing-import '(
                    mp:*current-process*
                    mp:process-kill
                    mp:process-enable
                    mp:process-disable
                    mp::process-preset
                    mp:process-reset
                    mp:process-interrupt
                    mp::process-name
                    mp:process-wait-function
                    mp:process-run-reasons
                    mp:process-arrest-reasons
                    mp:process-whostate
                    mp:without-interrupts
                    mp:process-wait
		    mp::process-active-p
                    ))

(export '(          *current-process*
                    process-kill
                    process-enable
                    process-disable
                    process-preset
                    process-reset
                    process-interrupt
                    process-name
                    process-wait-function
                    process-run-reasons
                    process-arrest-reasons
                    process-whostate
                    without-interrupts
                    process-wait
	            process-active-p
                    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implement missing (and differing) functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-process (&key (name "Anonymous") reset-action run-reasons arrest-reasons (priority 0) quantum
                          resume-hook suspend-hook initial-bindings run-immediately)
  (declare (ignore priority quantum reset-action resume-hook suspend-hook run-immediately))
  (let ((mp:*process-initial-bindings* initial-bindings))
    (mp:create-process name :run-reasons run-reasons :arrest-reasons arrest-reasons)))

(defun process-run-function (name-or-options preset-function &rest preset-arguments)
  (let ((process (ctypecase name-or-options
                   (string (make-process :name name-or-options))
                   (list (apply #'make-process name-or-options)))))
    (apply #'mp::process-preset process preset-function preset-arguments)
    (push :enable (mp:process-run-reasons process))
    process))

(defun process-property-list (process)
  (mp:process-plist process))

(defun (setf process-property-list) (new-value process)
  (setf (mp:process-plist process) new-value))

(defun process-name-to-process (name &optional abbrev)
  (if abbrev
      (let ((length (length name)))
        (dolist (process (mp:list-all-processes))
          (when (and (>= (length (process-name process)) length)
                     (string= name (process-name process) :end2 length))
            (return process))))
    (mp:find-process-from-name (ctypecase name
                                 (symbol (symbol-name name))
                                 (string name)))))

(defun process-wait-with-timeout (whostate seconds function &rest args)
  (apply #'mp:process-wait-with-timeout whostate seconds function args))

(defun wait-for-input-available (streams &key (wait-function #'socket::stream-input-available) whostate timeout)
  (let ((collected-fds nil))
    (flet ((fd (stream-or-fd)
             (typecase stream-or-fd
               (comm:socket-stream (comm:socket-stream-socket stream-or-fd))
               (socket::passive-socket (socket::socket-os-fd stream-or-fd))
               (fixnum stream-or-fd)))
           (collect-fds ()
             (setf collected-fds
                   (remove-if-not wait-function streams))))
      
      #+unix
      (unwind-protect
          (progn
            (dolist (stream-or-fd streams)
              (mp:notice-fd (fd stream-or-fd)))
            (if timeout
                (mp:process-wait-with-timeout (or whostate "Waiting for input") timeout #'collect-fds)
              (mp:process-wait (or whostate "Waiting for input") #'collect-fds)))
        (dolist (stream-or-fd streams)
          (mp:unnotice-fd (fd stream-or-fd))))
      #-unix
      (if timeout
          (mp:process-wait-with-timeout (or whostate "Waiting for input") timeout #'collect-fds)
        (mp:process-wait (or whostate "Waiting for input") #'collect-fds)))
    collected-fds))

(defmacro without-scheduling (&body forms)
  `(mp:without-preemption ,@forms))

(defun process-allow-schedule (&optional process)
  (declare (ignore process))
  (mp:process-allow-scheduling))

(defun process-revoke-run-reason (process object)
  (mp:without-preemption
   (setf (mp:process-run-reasons process)
         (remove object (mp:process-run-reasons process))))
  (when (and (eq process mp:*current-process*)
             (not mp:*inhibit-scheduling-flag*))
    (mp:process-allow-scheduling)))

(defun process-add-run-reason (process object)
  (setf (mp:process-run-reasons process) (pushnew object (mp:process-run-reasons process))))

;revised version from alain picard
(defun invoke-with-timeout (timeout bodyfn timeoutfn)
  (block timeout
    (let* ((process mp:*current-process*)
           (unsheduled? nil)
           (timer (mp:make-timer
                   #'(lambda ()
                       (mp:process-interrupt process
                                             #'(lambda ()
                                                 (unless unsheduled?
                                                   (return-from timeout
                                                     (funcall timeoutfn)))))))))
      (mp:schedule-timer-relative timer timeout)
      (unwind-protect (funcall bodyfn)
        (without-interrupts
         (mp:unschedule-timer timer)
         (setf unsheduled? t))))))


(defmacro with-timeout ((seconds &body timeout-forms) &body body)
  "Execute BODY; if execution takes more than SECONDS seconds, terminate
and evaluate TIMEOUT-FORMS."
  `(invoke-with-timeout ,seconds #'(lambda () ,@body)
                        #'(lambda () ,@timeout-forms)))

(defun current-process ()
  "The current process."
  mp:*current-process*)

(defun interrupt-process (process function &rest args)
  "Run FUNCTION in PROCESS."
  (apply #'mp:process-interrupt process function args))

(defun make-process-lock (&key name)
  (mp:make-lock :name name))

(defmacro with-process-lock ((lock &key norecursive timeout whostate) &body forms)
  (declare (ignore norecursive))
  `(mp:with-lock (,lock
		  ,@(when whostate (list :whostate whostate))
		  ,@(when timeout (list :timeout timeout)))
    ,@forms))

