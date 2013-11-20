(in-package #:interface)

(require "comm")

(defvar *server-ip-port* 8000)
(defvar *server-ip-address* "127.0.0.1")
(defvar *nb-sockets* 0)
(defvar *interface* nil)
(defvar *rpc-socket* nil)
(defvar *remote-interfaces* (make-hash-table)); Id->Interface
(defvar *socket-interfaces* '()); Id->Interface
(hcl:set-hash-table-weak *remote-interfaces* :value)

(eval-when (:load-toplevel :compile-toplevel :execute)
(defconstant +register-interface+ 1); interface-id object-id
(defconstant +rename-interface+ 2); interface-id new-interface-id
(defconstant +register-slot+ 3)
(defconstant +change-slot+ 4)
(defconstant +slot-changed+ 5)
(defconstant +unregister-interface+ 6)
(defconstant +eval+ 7)
(defconstant +fire-click+ 8))

(defclass remote-interface ()
  ((socket-stream :accessor socket-stream :initarg socket-stream)
   (interface-id  :accessor interface-id  :initarg interface-id)
   (object        :accessor object        :initarg object)
   (user          :accessor user          :initarg user)
   (page-root       :accessor page-root)
   (dispatchers   :accessor dispatchers)
   ))

(defmacro with-remote-interface (interface &body body)
  `(let ((*interface* ,interface)
	 (*object*  (object ,interface))
	 (*page-root* (page-root ,interface))
	 (*user*    (user ,interface)))
    ,@body))

(defun make-server-instream (handle)
  (log-message (format nil "open rpc socket~%"))
  (incf *nb-sockets*)
  (bt:make-thread (lambda ()
                    (server-listen (make-instance 'comm:socket-stream :socket handle :direction :io :element-type 'base-char)))))

(defun start-server ()
  (comm:start-up-server :function 'make-server-instream :service *server-ip-port*))

(defun server-listen (*rpc-socket*)
  (let ((*socket-interfaces* '()))
    (unwind-protect
	 (loop for line = (read-line *rpc-socket* nil nil)
	       while line
	       do
	       (process-server-input line))
      (progn (close *rpc-socket*)
	     (map nil 'unregister-interface *socket-interfaces*)
	     (log-message (format nil "close socket~%"))
	     (decf *nb-sockets*)))))

(defun process-server-input (line)
  (log-message (format nil "received ~s~%" line))
;  (handler-case
   (let ((packet (read-from-string line)))
     (let ((function-id (first packet)))
;       (handler-case
       (if (= function-id +register-interface+)
	 (register-interface (second packet))
	 (let ((interface (gethash (second packet) *remote-interfaces*)))
	   (when interface
	     (with-remote-interface interface
	       (ecase function-id
		 (#.+unregister-interface+ (unregister-interface *interface*))
		 ;; (#.+change-slot+ (change-slot *interface* (third packet)(fourth packet)))
		 ;; (#.+fire-click+ (fire-click *interface* (third packet)(fourth packet)))
		 ))))))))
;	  (condition (c) (display-remote-message *interface* (format nil "~a" c)))))))
;   (condition (c) (log-message (format nil "~a" c)))))

(defun send-interface (interface func-id string &optional (flush t))
;  (log-message (format nil "send-interface ~s ~s~%" func-id string))
  (let ((stream (socket-stream interface)))
    (format stream "~d ~d " func-id (interface-id interface))
    (write-string string stream)
    (write-char #\Newline stream)
    (when flush (force-output stream))))

(defun set-interface-id (interface interface-id)
  (send-interface interface 1 (format nil "~a" interface-id)))

(defun push-url-in-view (interface url)
  (send-interface interface 7 (format nil "window.navigate(~s)" url)))

(defun display-remote-message (interface message)
  (send-interface interface 7 (format nil "alert(~s)" message)))

(defun make-new-url-view (interface url)
  (send-interface interface 7 (format nil "window.open(~s)" url)))

(defun register-interface (encoded-params)
  (log-message (format nil "register interface ~%" ))
  (let* ((interface (make-instance 'remote-interface))
	 (dispatchers (make-hash-table :test #'equal))
	 (decoded-params (read-from-string (decode-url-string encoded-params)))
	 (interface-id (random 1000000))
	 (user-id (getf decoded-params :user))
	 (view-id (getf decoded-params :ui))
	 (obj-id (getf decoded-params :obj)))
    (log-message (format nil "decoded params ~s ~%" decoded-params))
    (setf (socket-stream interface) *rpc-socket*)
    (setf (page-root interface) (gethash view-id *all-object-views*))
    (setf (interface-id interface) interface-id)
    (setf (object interface) (meta::load-object obj-id))
    (setf (user interface) user-id) ;(meta::load-object obj-id))
    (push interface *socket-interfaces*)
    (setf (dispatchers interface) dispatchers)
    (set-interface-id interface interface-id)
    (maphash #'(lambda (name item)
		 (setf (gethash name dispatchers)
		       (make-dispatcher interface (object interface) item)))
	     (all-items (page-root interface)))
    (setf (gethash (interface-id interface) *remote-interfaces*) interface)))

#|
(defun change-slot (interface item-name encoded-value)
  (log-message (format nil "change slot ~s ~s~%" item-name encoded-value))
  (let* ((decoded-value (html:decode-base64-string encoded-value))
	 (dispatcher (gethash item-name (dispatchers interface))))
    (log-message (format nil "decoded value ~s ~%" decoded-value))
    (try-change-slot dispatcher (meta::std-string-to-value decoded-value (slot dispatcher)))))

(defun fire-click (interface item-name click-id)
  (log-message (format nil "fire-click ~s ~s~%" item-name click-id))
  (let ((dispatcher (gethash item-name (dispatchers interface))))
    (log-message (format nil "dispatcher ~s~%" dispatcher))
    (when dispatcher (fire-action dispatcher click-id))))
|#

(defun unregister-interface (interface)
  (log-message (format nil "unregister interface ~d~%" (interface-id interface)))
  (maphash #'(lambda (name dispatcher)
	       (when dispatcher
		 (unregister-dispatcher dispatcher)))
	   (dispatchers interface))
  (remhash (interface-id interface) *remote-interfaces*))


