(in-package #:interface)

(defvar *apache-port* 3000)
(defvar *apache-socket* nil)
(defvar *apache-nb-use-socket* 0)
(defvar *apache-nb-requests* 0)
(defvar *close-apache-socket* nil)
(defvar *ignore-errors* nil)

#+nil(defun make-apache-instream (handle)
  (mp:process-run-function (format nil "apache socket ~d" handle) '()
                            'apache-listen (make-instance 'comm:socket-stream :socket handle
							  :direction :io :element-type 'base-char))) ;;fixme, it doesnt support UTF-8!

(defconstant %max-content-length% 2000000)

(defun start-apache-listener (&optional (port *apache-port*))
  ;; (comm:start-up-server :function 'make-apache-instream :service port)
  (usocket:socket-server "127.0.0.1" port #'apache-listen nil :in-new-thread t :reuse-address t :multi-threading t :max-buffer-size %max-content-length%)
  )

(export 'sa)
(defun sa (&optional (port *apache-port*))
  (start-apache-listener port))

(defun apache-listen (*apache-socket*)
  (let ((*close-apache-socket* t))
    (flet ((handle-request ()
	     (loop for command = (get-apache-command)
		   for *apache-nb-use-socket* from 0
		   while command do (process-apache-command command)
		   until *close-apache-socket*)))
      (unwind-protect
	   (if *ignore-errors*
	       (ignore-errors (handle-request))
	       (handle-request))
	(progn
          (log:debug "apache-close-socket" *close-apache-socket*)
          (ignore-errors (usocket:socket-close *apache-socket*)))))))

(defun read-utf8-line (stream)
  ;; (print (read-line stream nil nil))
  (let ((line (loop
                 for ch = (read-byte stream nil nil)
                 if (and ch (/= ch (char-code #\NEWLINE))) collect ch into line
                 else do (return line))))
    (when line
      (babel:octets-to-string (make-array (length line) :element-type '(unsigned-byte 8) :initial-contents line)))))

(defun get-apache-command ()
  (ignore-errors
    (let* ((header (loop for key = (read-utf8-line *apache-socket*)
			 while (and key (string-not-equal key "end"))
                      for value = (read-utf8-line *apache-socket*)
			 collect (cons key value)))
	   (content-length (cdr (assoc "content-length" header :test #'equal)))
	   (content nil))
      (when content-length
	(setf content-length (parse-integer content-length :junk-allowed t))
	(when content-length
	  (if (< content-length %max-content-length%)
	      (progn
		(setf content (make-string (min content-length %max-content-length%)))
		(read-sequence content *apache-socket*)
		(push (cons "posted-content" content) header))
	      (progn
		(push (cons "posted-content-overflow" content-length) header)
		(loop repeat content-length
		  do (read-char *apache-socket*))))))
      header)))

(defun process-apache-command (command)
  (log:debug "process-apache-command ~s~%" command)
  (incf *apache-nb-requests*)
  (util:with-logged-errors () (%process-apache-command% command)))

(defvar *reply-protocol* nil)

(defvar *banned-ips* (make-hash-table :test #'equal))

(defun ban-ip (ip)
  (setf (gethash ip *banned-ips*) ip))

(defun un-ban-ip (ip)
  (remhash ip *banned-ips*))

(defun banned-ip-p (ip)
  (gethash ip *banned-ips*))

(defun %process-apache-command% (command)
  (incf *request-counter*)
  (let* ((request (make-instance 'http-request :command command))
	 (*request-id* (encode-integer *request-counter*)))
    (if (banned-ip-p (%command-param "remote-ip-addr" command))
        (http-message "Blocked IP Address<br>Contact the web site administrator" request)
        (unless (process-http-request request)
          (http-debug-request request)))
    (write-apache-request request *apache-socket*)
    (force-output *apache-socket*)))

;;; a fix to load portable aserve instead of apache
(handler-case
    (progn (intern "FIXNUMP" 'common-lisp)
           (setf (symbol-function 'common-lisp::fixnump) (symbol-function 'lw:fixnump)))
  (error (error)))
