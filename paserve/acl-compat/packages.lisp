;;;; -*- mode: lisp -*-
;;;;
;;;; Package definitions for acl-compat.
;;;;
;;;; Package names follow their Allegro CL counterparts -- for an ACL
;;;; package foo, acl-compat defines a package acl-compat.foo
;;;;
;;;; Some packages have nicknames, which were used as package names by
;;;; previous versions of paserve and acl-compat.  The nicknames are
;;;; deprecated, but are kept for the benefit of people using
;;;; acl-compat in other projects.  New projects should use the
;;;; package names starting with "acl-compat.".
;;;;

(in-package :common-lisp-user)

;;; general
(defpackage :acl-compat.excl
  (:use #:common-lisp
        #+cmu #:ext
        #+clisp #:ext
        #+sbcl #:sb-ext #+sbcl #:sb-gray
        #+(or allegro cormanlisp) :excl
        #+mcl :ccl
        )
  #+lispworks (:import-from :common-lisp #:fixnump)
  #+sbcl (:import-from :sb-int #:fixnump)
  #+sbcl (:import-from :sb-ext #:without-package-locks)
  #+cmu (:import-from :ext #:without-package-locks)
  #+allegro (:shadowing-import-from :excl #:filesys-size
	    #:filesys-write-date #:intern* #:filesys-type #:atomically #:fast)
  (:export
   #:if*
   #:*initial-terminal-io*
   #:*cl-default-special-bindings*
   #:filesys-size
   #:filesys-write-date
   #:stream-input-fn
   #:match-regexp
   #:compile-regexp
   #:*current-case-mode*
   #:intern*
   #:filesys-type
   #:errorset
   #:atomically
   #:fast
   #:without-package-locks
   #:fixnump
   #+(or lispworks mcl) #:socket-error
   #+(or allegro lispworks mcl) #:run-shell-command
   #+(or allegro mcl) #:fasl-read
   #+(or allegro mcl) #:fasl-write
   #+(or allegro cmu scl mcl lispworks) #:string-to-octets
   #+(or allegro cmu scl mcl lispworks) #:write-vector
   ))


;; general
(defpackage :acl-compat.mp
  (:use :common-lisp #+cormanlisp :acl-compat-mp #+allegro :mp)
  (:nicknames :acl-mp #-cormanlisp :acl-compat-mp)
  #+allegro (:shadowing-import-from :mp #:process-interrupt #:lock)
  #+allegro (:shadowing-import-from :excl #:without-interrupts)
  (:export 
   #:*current-process*         ;*
   #:process-kill              ;*
   #:process-preset            ;*
   #:process-name              ;*

   #:process-wait-function
   #:process-run-reasons 
   #:process-arrest-reasons
   #:process-whostate
   #:without-interrupts
   #:process-wait
   #:process-enable
   #:process-disable
   #:process-reset
   #:process-interrupt

   #:process-run-function      ;*
   #:process-property-list     ;*
   #:without-scheduling        ;*
   #:process-allow-schedule    ;*
   #:make-process              ;*
   #:process-add-run-reason    ;*
   #:process-revoke-run-reason ;*
   #:process-add-arrest-reason    ;*
   #:process-revoke-arrest-reason ;*
   #:process-allow-schedule    ;*
   #:with-timeout              ;*
   #:make-process-lock         ;*
   #:with-process-lock         ;*
   #:process-lock
   #:process-unlock

   #:current-process
   #:process-name-to-process
   #:process-wait-with-timeout
   #:wait-for-input-available
   #:process-active-p
   ))

(defpackage :de.dataheaven.chunked-stream-mixin
  (:use :common-lisp)
  (:export #:chunked-stream-mixin
           #:output-chunking-p #:input-chunking-p))

;; general
(defpackage acl-compat.socket
  (:use #:common-lisp
        #+(or cmu lispworks scl) #:acl-mp
        #+(or lispworks cmu)#:acl-compat.excl
        #+clisp #:socket
        #+sbcl #:sb-bsd-sockets
        #+(or lispworks cmu) #:de.dataheaven.chunked-stream-mixin
        #+cormanlisp #:socket
        )
  #+cl-ssl (:import-from :ssl #:MAKE-SSL-CLIENT-STREAM #:MAKE-SSL-SERVER-STREAM)
  #+lispworks (:shadow socket-stream stream-error)
  (:export
   #+(or lispworks cmu) #:socket
   #:make-socket
   #:accept-connection
   #:ipaddr-to-dotted
   #:dotted-to-ipaddr
   #:ipaddr-to-hostname
   #:lookup-hostname
   #:remote-host
   #:remote-port
   #:local-host
   #:local-port
   #:socket-control
   #+cl-ssl #:make-ssl-client-stream
   #+cl-ssl #:make-ssl-server-stream
   #+lispworks #:socket-os-fd
   )
  #-cormanlisp (:nicknames #-(or clisp allegro) socket #-allegro acl-socket))


(defpackage acl-compat.system
  (:nicknames :acl-compat.sys)
  (:use :common-lisp) 
  (:export
   #:command-line-arguments
   #:command-line-argument
   #:reap-os-subprocess
   ))

#-cormanlisp
(defpackage :gray-stream
  (:use #:common-lisp)
  (:import-from #+lispworks :stream #+cmu :lisp #+clisp :gray #+cormanlisp :gray-streams
                #+mcl :ccl #+allegro :excl #+sbcl :sb-gray
                #:fundamental-binary-input-stream
                #:fundamental-binary-output-stream
                #:fundamental-character-input-stream
                #:fundamental-character-output-stream
                #:stream-element-type
                #:stream-listen
                #:stream-read-byte
                #:stream-read-char
                #:stream-peek-char
                #:stream-write-byte
                #:stream-write-char
                #:stream-read-char-no-hang
                #:stream-force-output
                #:stream-finish-output
                #:stream-clear-input
                #:stream-clear-output
                #:stream-line-column
                #-(or clisp openmcl) #:stream-read-sequence
                #:stream-unread-char
                #:stream-read-line
                #-(or clisp openmcl) #:stream-write-sequence
                #:stream-write-string
                #+lispworks #:stream-write-buffer
                #+lispworks #:stream-read-buffer
                #+lispworks #:stream-fill-buffer
                #+lispworks #:stream-flush-buffer
                #+lispworks #:with-stream-input-buffer
                #+lispworks #:with-stream-output-buffer)
  (:export 
   #:fundamental-binary-input-stream
   #:fundamental-binary-output-stream
   #:fundamental-character-input-stream
   #:fundamental-character-output-stream
   #:stream-element-type
   #:stream-listen
   #:stream-read-byte
   #:stream-read-char
   #:stream-write-byte
   #:stream-write-char
   #:stream-read-char-no-hang
   #:stream-force-output
   #:stream-finish-output
   #:stream-clear-input
   #:stream-clear-output
   #:stream-line-column
   #-clisp #:stream-read-sequence
   #:stream-unread-char
   #:stream-read-line
   #-clisp #:stream-write-sequence
   #:stream-write-string
   #:stream-write-buffer
   #:stream-read-buffer
   #:stream-fill-buffer
   #:stream-flush-buffer
   #:with-stream-input-buffer
   #:with-stream-output-buffer))

#+cormanlisp
(defpackage :gray-stream
  (:use #:common-lisp :gray-streams)
  (:export 
   #:fundamental-binary-input-stream
   #:fundamental-binary-output-stream
   #:fundamental-character-input-stream
   #:fundamental-character-output-stream
   #:stream-element-type
   #:stream-listen
   #:stream-read-byte
   #:stream-read-char
   #:stream-write-byte
   #:stream-write-char
   #:stream-read-char-no-hang
   #:stream-force-output
   #:stream-finish-output
   #:stream-clear-input
   #:stream-clear-output
   #:stream-line-column
   #:stream-read-sequence
   #:stream-unread-char
   #:stream-read-line
   #:stream-write-sequence
   #:stream-write-string
   #:stream-write-buffer
   #:stream-read-buffer
   #:stream-fill-buffer
   #:stream-flush-buffer
   #:with-stream-input-buffer
   #:with-stream-output-buffer))
