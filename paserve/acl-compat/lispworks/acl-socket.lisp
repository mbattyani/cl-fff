;; This package is designed for LispWorks.  It implements the
;; ACL-style socket interface on top of LispWorks.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "comm"))

#+cl-ssl
(eval-when (:compile-toplevel :load-toplevel :execute)
(ssl-internal::initialize-ssl-library)
)

(in-package acl-compat.socket)

(define-condition stream-error (error)
  ((acl-compat.excl::stream :initarg :stream
           :reader stream-error-stream)
   (acl-compat.excl::action :initarg :action
           :reader stream-error-action)
   (acl-compat.excl::code :initarg :code
         :reader stream-error-code)
   (acl-compat.excl::identifier :initarg :identifier
               :reader stream-error-identifier))
  (:report (lambda (condition stream)
             (format stream "A stream error occured (action=~A identifier=~A code=~A stream=~S)."
                     (stream-error-action condition)
                     (stream-error-identifier condition)
                     (stream-error-code condition)
                     (stream-error-stream condition)))))

(define-condition socket-error (stream-error)
  ()
  (:report (lambda (condition stream)
             (format stream "A socket error occured (action=~A identifier=~A code=~A stream=~S)."
                     (stream-error-action condition)
                     (stream-error-identifier condition)
                     (stream-error-code condition)
                     (stream-error-stream condition)))))

#+unix
(defun %socket-error-identifier (code)
  (case code
    (32 :x-broken-pipe)
    (98 :address-in-use)
    (99 :address-not-available)
    (100 :network-down)
    (102 :network-reset)
    (103 :connection-aborted)
    (104 :connection-reset)
    (105 :no-buffer-space)
    (108 :shutdown)
    (110 :connection-timed-out)
    (111 :connection-refused)
    (112 :host-down)
    (113 :host-unreachable)
    (otherwise :unknown)))

#+win32
(defun %socket-error-identifier (code)
  (case code
    (10048 :address-in-use)
    (10049 :address-not-available)
    (10050 :network-down)
    (10052 :network-reset)
    (10053 :connection-aborted)
    (10054 :connection-reset)
    (10055 :no-buffer-space)
    (10058 :shutdown)
    (10060 :connection-timed-out)
    (10061 :connection-refused)
    (10064 :host-down)
    (10065 :host-unreachable)
    (otherwise :unknown)))

(defun socket-error (stream error-code action format-string &rest format-args)
  (declare (ignore format-string format-args)) ;no valid initargs for this with socket-error
  (let ((code (if (numberp error-code) error-code #+unix(lw:errno-value))))
    (error 'socket-error :stream stream :code code
           :identifier (if (keywordp error-code)
                           error-code
                         (%socket-error-identifier error-code))
           :action action)))


(defclass socket ()
  ((passive-socket :type fixnum
                   :initarg :passive-socket
                   :reader socket-os-fd)))

(defclass passive-socket (socket)
  ((element-type :type (member signed-byte unsigned-byte base-char)
		 :initarg :element-type
		 :reader element-type)
   (port :type fixnum
	 :initarg :port
	 :reader local-port)))

(defclass binary-socket-stream (de.dataheaven.chunked-stream-mixin:chunked-stream-mixin comm:socket-stream) ())
(defclass input-binary-socket-stream (binary-socket-stream)())
(defclass output-binary-socket-stream (binary-socket-stream)())
(defclass bidirectional-binary-socket-stream (input-binary-socket-stream output-binary-socket-stream)())


(defmethod comm::socket-error ((stream binary-socket-stream) error-code format-string &rest format-args)
  (apply #'socket-error stream error-code :IO format-string format-args))


(declaim (inline %reader-function-for-sequence))
(defun %reader-function-for-sequence (sequence)
  (typecase sequence
    (string #'read-char)
    ((array unsigned-byte (*)) #'read-byte)
    ((array signed-byte (*)) #'read-byte)
    (otherwise #'read-byte)))

;; Bivalent socket support for READ-SEQUENCE
(defmethod gray-stream:stream-read-sequence ((stream input-binary-socket-stream) sequence start end)
  (stream::read-elements stream sequence start end (%reader-function-for-sequence sequence)))

;; NDL 2004-06-06 -- without this, emit-clp-entity tries writing a string down a binary stream, and LW barfs
(defmethod gray-stream:stream-write-sequence ((stream output-binary-socket-stream) (sequence string) start end)
  (write-string sequence stream :start start :end end))

;; ACL Gray-Streams Enhancment Generic Functions 

(defmethod stream-input-fn ((stream input-binary-socket-stream))
  (comm:socket-stream-socket stream))

(defmethod stream-output-fn ((stream output-binary-socket-stream))
  (comm:socket-stream-socket stream))

(defmethod socket-os-fd ((socket comm:socket-stream))
  (comm:socket-stream-socket socket))

(defmethod print-object ((passive-socket passive-socket) stream)
  (print-unreadable-object (passive-socket stream :type t :identity nil)
    (format stream "@~d on port ~d" (socket-os-fd passive-socket) (local-port passive-socket))))

(defmethod stream-input-available ((fd fixnum))
  (comm::socket-listen fd))

(defmethod stream-input-available ((stream stream::os-file-handle-stream))
  (stream-input-available (stream::os-file-handle-stream-file-handle stream)))

(defmethod stream-input-available ((stream comm:socket-stream))
  (or (comm::socket-listen (comm:socket-stream-socket stream))
      (listen stream)))

(defmethod stream-input-available ((stream socket::passive-socket))
  (comm::socket-listen (socket::socket-os-fd stream)))


(defmethod accept-connection ((passive-socket passive-socket)
			      &key (wait t))
  (if (or wait (stream-input-available passive-socket))
      (make-instance 'bidirectional-binary-socket-stream
                     :socket (comm::get-fd-from-socket (socket-os-fd passive-socket))
                     :direction :io
                     :element-type (element-type passive-socket))))

(defun %new-passive-socket (local-port)
  (multiple-value-bind (socket error-location error-code)
      (comm::create-tcp-socket-for-service local-port)
    (cond (socket socket)
	  (t (error 'socket-error :action error-location :code error-code :identifier :unknown)))))

(defun make-socket (&key (remote-host "localhost")
			 local-port
			 remote-port 
			 (connect :active)
			 (format :text)
                         (reuse-address t)
			 &allow-other-keys)
  (declare (ignore format))
  (check-type remote-host string)
  (ecase connect 
    (:passive
     (let ((comm::*use_so_reuseaddr* reuse-address))
       (make-instance 'passive-socket
                      :port local-port
                      :passive-socket (%new-passive-socket local-port)
                      :element-type '(unsigned-byte 8))))
    (:active
     (handler-case
         (let ((stream (comm:open-tcp-stream remote-host remote-port
                                             :direction :io
                                             :element-type '(unsigned-byte 8)
                                             :errorp t)))
           (change-class stream 'bidirectional-binary-socket-stream))
       (simple-error (condition) 
                     (let ((code (first (last (simple-condition-format-arguments condition)))))
                       (socket-error condition code
                                     :connect "~A occured while connecting (~?)" (simple-condition-format-arguments condition))))))))


(defmethod close ((passive-socket passive-socket) &key abort)
  (declare (ignore abort))
  (comm::close-socket (socket-os-fd passive-socket)))

;(declaim (ftype (function ((unsigned-byte 32)) (values simple-string))
;		ipaddr-to-dotted))
(defun ipaddr-to-dotted (ipaddr &key values)
  ;(declare (type (unsigned-byte 32) ipaddr))
  (if ipaddr ;sometimes ipaddr is nil in the log call if client has broken the connection
    (let ((a (logand #xff (ash ipaddr -24)))
          (b (logand #xff (ash ipaddr -16)))
          (c (logand #xff (ash ipaddr -8)))
          (d (logand #xff ipaddr)))
      (if values
	(values a b c d)
        (format nil "~d.~d.~d.~d" a b c d)))
    (if values (values 0 0 0 0) "0.0.0.0")))

(defun string-tokens (string)
  (labels ((get-token (str pos1 acc)
                      (let ((pos2 (position #\Space str :start pos1)))
                        (if (not pos2)
                            (nreverse acc)
                          (get-token str (1+ pos2) (cons (read-from-string (subseq str pos1 pos2))
                                                         acc))))))
(get-token (concatenate 'string string " ") 0 nil)))

(declaim (ftype (function (string &key (:errorp t))
                          (values (unsigned-byte 32)))
		dotted-to-ipaddr))
(defun dotted-to-ipaddr (dotted &key (errorp t))
  (declare (string dotted))
  (if errorp
      (let ((ll (string-tokens (substitute #\Space #\. dotted))))
	(+ (ash (first ll) 24) (ash (second ll) 16)
	   (ash (third ll) 8) (fourth ll)))
    (ignore-errors 
      (let ((ll (string-tokens (substitute #\Space #\. dotted))))
	(+ (ash (first ll) 24) (ash (second ll) 16)
	   (ash (third ll) 8) (fourth ll))))))

(defun ipaddr-to-hostname (ipaddr &key ignore-cache)
  (declare (ignore ignore-cache))
  (multiple-value-bind (name)
      (comm:get-host-entry (ipaddr-to-dotted ipaddr) :fields '(:name))
    name))

(defun lookup-hostname (host &key ignore-cache)
  (when ignore-cache
    (warn ":IGNORE-CACHE keyword in LOOKUP-HOSTNAME not supported."))
  (if (stringp host)
      (multiple-value-bind (addr)
	  (comm:get-host-entry host :fields '(:address))
	addr)
    (dotted-to-ipaddr (ipaddr-to-dotted host))))

(defmethod remote-host ((socket comm:socket-stream))
  (comm:socket-stream-peer-address socket))

(defmethod remote-port ((socket comm:socket-stream))
  (multiple-value-bind (host port)
      (comm:socket-stream-peer-address socket)
    (declare (ignore host))
    port))

(defmethod local-host ((socket comm:socket-stream))
  (multiple-value-bind (host port)
      (comm:socket-stream-address socket)
    (declare (ignore port))
    host))

(defmethod local-port ((socket comm:socket-stream))
  (multiple-value-bind (host port)
      (comm:socket-stream-address socket)
    (declare (ignore host))
    port))

(defun socket-control (stream &key (output-chunking nil oc-p) output-chunking-eof (input-chunking nil ic-p))
  (when oc-p
    (when output-chunking
      (de.dataheaven.chunked-stream-mixin::initialize-output-chunking stream))
    (setf (de.dataheaven.chunked-stream-mixin:output-chunking-p stream) output-chunking))
  (when output-chunking-eof
    (de.dataheaven.chunked-stream-mixin::disable-output-chunking stream))
  (when ic-p
    (when input-chunking
      (de.dataheaven.chunked-stream-mixin::initialize-input-chunking stream))
    (setf (de.dataheaven.chunked-stream-mixin:input-chunking-p stream) input-chunking)))

(provide 'acl-socket)
