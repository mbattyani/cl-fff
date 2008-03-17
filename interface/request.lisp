(in-package interface)

(defun write-header-line (socket key value)
  (if *reply-protocol*
      (progn
	(write-string key socket)
	(write-string ": " socket)
	(write-string value socket)
	(write-char #\Return socket)
	(write-char #\NewLine socket))
      (progn
	(write-string key socket)
	(write-char #\NewLine socket)
	(write-string value socket)
	(write-char #\NewLine socket))))

(defclass http-request ()
  ((command :initform nil :accessor command :initarg :command)
   (clear-content :initform nil :accessor clear-content)
   (status :initform "200 OK" :accessor status)
   (content-type :initform "text/html; charset=iso-8859-1" :accessor content-type)
   (content-length :initform nil :accessor content-length)
   (content-value :initform "" :accessor content-value)
   (date :initform (get-universal-time) :accessor date)
   (cache-control :initform "no-cache" :accessor cache-control)
   (keep-alive :initform t :accessor keep-alive)
   (keep-socket :initform t :accessor keep-socket)
   (header-alist :initform nil :accessor header-alist)
   (header-sent :initform nil :accessor header-sent)
   (content-sent :initform nil :accessor content-sent)
   (url :initform nil :accessor url)
   (referer :initform nil :accessor referer)
   (host :initform nil :accessor host)
   (user-agent :initform nil :accessor user-agent)
   (session-params :initform nil :accessor session-params)
   (posted-content :initform nil :accessor posted-content)
   (search-params :initform nil :accessor search-params)
   (user-name :initform nil :accessor user-name)
   (password :initform nil :accessor password)
   (user :initform nil :accessor user)
   (cookie :initform nil :accessor cookie)
   (new-cookie :initform nil :accessor new-cookie)
   (session :initform nil :accessor session)
   ))

(defmethod initialize-instance :after ((request http-request) &rest init-options &key &allow-other-keys)
  (declare (ignore init-options))
  (setf (url request) (cdr (assoc "url" (command request) :test #'string=)))
  (let ((param-pos (position #\? (url request))))
    (when param-pos
      (push (cons "url-params" (subseq (url request) (1+ param-pos)))(command request))
      (setf (url request) (subseq (url request) 0 param-pos))))
  (decode-authentication request)
  (unless (web-robot-request? request)
    (process-cookie request)))

(defun push-header (header value request)
  (setf (header-alist request) (acons header value (header-alist request))))

(defun write-header (request socket)
  (unless (header-sent request)
    (if *reply-protocol*
	(progn 
	  (write-string "HTTP/1.0 " socket)
	  (write-string (status request) socket)
	  (write-char #\Return socket)
	  (write-char #\NewLine socket))
	(write-header-line socket "Status" (status request)))
    (write-header-line socket "Content-Type" (content-type request))
    (write-header-line socket "Connection" (if (keep-alive request) "Keep-Alive" "Close"))
    (when (cache-control request)
      (write-header-line socket "Cache-Control" (cache-control request)))
    (unless *reply-protocol*
      (when (content-length request)
	(write-string "Content-Length" socket)
	(write-char #\NewLine socket)
	(html::fast-format socket "~d" (content-length request))
	(write-char #\NewLine socket))
      (write-header-line socket "Keep-Socket"
			 (if (and (content-length request)(keep-socket request)) "1" "0")))
    (loop for (name . value) in  (header-alist request)
	  do (write-header-line socket name value))
    (if *reply-protocol*
	(progn
	  (write-char #\Return socket)
	  (write-char #\NewLine socket))
	(progn
	  (write-string "end" socket)
	  (write-char #\NewLine socket)))
    (setf (header-sent request) t)))

(defvar *requests* nil)

(defmethod content-length ((obj string))
  (length obj))

(defmethod write-content-value ((obj string) stream)
  (write-string obj stream))

(defconstant +disk-block-size+ 8192)

(defmethod content-length ((obj pathname))
  (let ((length nil))
    (ignore-errors
      (with-open-file (in obj :direction :input :element-type '(unsigned-byte 8))
	(setf length (file-length in))))
    length))

(defmethod write-content-value ((obj pathname) stream)
  (with-open-file (in obj :direction :input :element-type '(unsigned-byte 8))
    (let ((buffer (make-array +disk-block-size+ :element-type '(unsigned-byte 8))))
      (loop for read = (read-sequence buffer in)
	    until (zerop read)
	    do (write-sequence buffer stream :end read)))))

(defun write-request (request socket)
  (when (clear-content request)
    (setf (content-value request) nil
	  (content-length request) 0))
  (write-header request socket)
  (when (and (content-value request)(not (content-sent request)))
    (write-content-value (content-value request) socket)
    (setf *close-apache-socket* (or *reply-protocol*
				    (not (and (content-length request)(keep-socket request)))))
    (setf (content-sent request) t)))

(defvar *request-stream* nil)

(defmacro with-output-to-request ((request &optional (stream '*request-stream*)) &rest forms)
  `(let ((*request* ,request)
	 (*request-views* nil))
    (setf (content-value *request*)
     (with-output-to-string (,stream) ,@forms))
      (when (stringp (content-value *request*))
	(setf (content-length *request*) (length (content-value *request*))))))

(defun decode-authentication (request)
  (let ((authentication (%command-param "Authorization" (command request))))
    (when authentication
      (let* ((first-white (position-if #'(lambda (char) (member char '(#\space #\tab))) authentication))
	     (start-base64 (position-if-not #'(lambda (char) (member char '(#\space #\tab))) authentication :start first-white))
	     (decoded-authorization (html:decode-base64-string authentication :start start-base64))
	     (colon (position #\: decoded-authorization :test #'char-equal)))
	(setf (user-name request) (subseq decoded-authorization 0 colon)
	      (password request) (subseq decoded-authorization (1+ colon)))))))

(defun decode-posted-content  (request)
  (unless (posted-content request)
    (let ((posted-content (%command-param "posted-content" (command request)))
	  (url-params (%command-param "url-params" (command request)))
	  (content-type (%command-param "content-type" (command request))))
      (when content-type
	(setf content-type (html::parse-header-values content-type)))
      (setf (posted-content request)
            (ignore-errors
              (nconc
               (when posted-content (html::parse-posted-content posted-content content-type))
               (when url-params (html::parse-url-posted-content url-params)))))))
  (posted-content request))

(defmacro cache-request-value (slot-name value-name)
  (let ((temp (gensym)))
    `(defmethod ,slot-name :around ((request http-request))
      (let ((,temp (call-next-method)))
	(unless ,temp
	  (setf ,temp (%command-param ,value-name (command request))))
	,temp))))

(cache-request-value host "Host")
(cache-request-value user-agent "User-Agent")
(cache-request-value referer "Referer")

(defun redirect-to (new-url request &key definitive)
  (log-message (format nil "redirect-to ~s ~s ~%" new-url request))
  (setf (status request) (if definitive "302 Moved" "301 Moved"))
  (setf (clear-content request) t)
  (setf (keep-socket request) nil)
  (push-header "Location" new-url request))

(defun set-request-to-file (request filename &key (content-type "application/data"))
  (setf filename (pathname filename))
  (setf (content-length request) (content-length filename))
  (setf (cache-control request) nil)
  (if (content-length request)
      (setf (content-type request) content-type
	    (content-value request) filename)
      (setf (status request) "404 Not Found"
	    (clear-content request) t
	    (keep-socket request) nil)))

(defun web-robot-request? (request)
  (let ((user-agent (user-agent request)))
    (declare (optimize (speed 3)(space 0)(debug 0)(safety 0))
	     #+nil (type simple-base-string user-agent))
    (or (not user-agent)
      (or (search "Google" user-agent)
          (search "Gigabot" user-agent)
          (search "YahooFeed" user-agent)
          (search "Snapbot" user-agent)
	  (search "msnbot" user-agent)
	  (search "Ask Jeeves" user-agent)
	  (search "yacybot" user-agent)
	  (search "ZyBorg" user-agent)
	  (search "MJ12bot" user-agent)
          (search "topicblogs" user-agent)
          (search "Slurp" user-agent)))))

(defmacro with-posted-strings ((request . params) . body)
  (let ((posted-content (gensym)))
    `(let* ((,posted-content (decode-posted-content ,request))
	    ,@(loop for (var-name posted-param) in params
		    collect `(,var-name (cdr (assoc ,posted-param ,posted-content :test 'string=)))))
      ,@body)))

(defun modern-browser (&optional (request *request*))
  (let ((user-agent (user-agent request)))
    (declare (optimize (speed 3)(space 0)(debug 0)(safety 0))
	     (type simple-base-string user-agent))
    (when user-agent
      (search "MSIE " user-agent))))

(defun http-message (message request &key (color "red")(log-error t))
  (with-output-to-request (request)
    (html::html-to-stream *request-stream*
			  "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">"
			  (:html (:head)
				 (:body
				  ((:table "border" "1" "bgcolor" "#c0c0c0")
				   ((:tr "bgcolor" color) (:th "An error occured"))
				   ((:tr "bgcolor" "#F0F0c0") (:td message))))))
    (when log-error
      (push-header "Log-Error" message request))))

(defun http-debug-request (request)
  (log-message (format nil "http-debug-request ~%"))
  (with-output-to-request (request)
    (html::html-to-stream *request-stream*
			  "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">"
			  (:html (:head)
				 (:body
				  ((:table "bgcolor" "#c0c0c0")
				   ((:tr "bgcolor" "yellow") (:th "Key") (:th "Value"))
				   ((:tr "bgcolor" "#F0F0c0")
				    (:td "apache-nb-use-socket")(:td *apache-nb-use-socket*))
				   (loop for (key . value) in (command request)
					 do (html::html ((:tr "bgcolor" "#F0F0c0")
							 (:td key)(:td value))))))))
    (push-header "Log-Error" (html::fast-format nil "request ~s not processed." (url request)) request)
    ))

(defun reload-opener-and-close (request)
  (interface::with-output-to-request (request)
    (html::html-to-stream
     interface::*request-stream*
     "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">"
     (:html
      (:head)
      (:body
       (:jscript "window.opener.location.reload(true);window.close();"))))))

(defun reload-remote-browser-from-push-pull (request)
  (sleep 0.5)
  (interface::with-output-to-request (request)
    (html::html-to-stream
     interface::*request-stream*
     "window.location.href = window.location.href;")))

;window.location.reload(true);