(in-package interface)

(require "comm")

(defvar *apache-port* 3000)
(defvar *apache-socket* nil)
(defvar *apache-nb-use-socket* 0)
(defvar *apache-nb-requests* 0)
(defvar *close-apache-socket* nil)
(defvar *ignore-errors* nil)

(defun make-apache-instream (handle)
  (mp:process-run-function (format nil "apache socket ~d" handle) '()
                            'apache-listen (make-instance 'comm:socket-stream :socket handle
							  :direction :io :element-type 'base-char)))

(defun start-apache-listener (&optional (port *apache-port*))
  (comm:start-up-server :function 'make-apache-instream :service port))

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
	(progn (log-message (format nil "apache-close-socket ~a~%" *close-apache-socket*))
	       (ignore-errors (close *apache-socket*)))))))

(defconstant %max-content-length% 2000000)

(defun get-apache-command ()
  (ignore-errors
    (let* ((header (loop for key = (read-line *apache-socket* nil nil)
			 while (and key (string-not-equal key "end"))
			 for value = (read-line *apache-socket* nil nil)
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
  (log-message (format nil "process-apache-command ~s~%" command))
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
    (write-request request *apache-socket*)
    (force-output *apache-socket*)))
