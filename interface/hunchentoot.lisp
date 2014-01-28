(in-package :hunchentoot)

(defmethod acceptor-dispatch-request ((acceptor acceptor) request)
  "Detault implementation of the request dispatch method, generates an
+http-not-found+ error."
  (let ((path (and (acceptor-document-root acceptor)
                   (request-pathname request))))
    (let* ((headers-in (headers-in*))
           ;; (post-parameters (raw-post-data :force-text t)) ; #'post-parameters* returns alist, not string
           (list (list* (cons "url" (request-uri*))
                        (cons "Referer" (referer request))
                        (cons "User-Agent" (cdr (assoc :user-agent headers-in)))
                        (cons "Accept-Language" (cdr (assoc :accept-language headers-in)))
                        (cons "posted-content" (hunchentoot:post-parameters request)) ; post-parameters)
                        (cons "content-type" (cdr (assoc :content-type headers-in)))
                        (loop
                           for (cookie . value) in (cookies-in*)
                           collect
                             (cons "Cookie" (concatenate 'string cookie "=" value))))))
      #+nil(format *error-output* "uri: ~a -> ~a~%post-parameters: ~a -> ~a~%headers-in: ~a -> ~a~%cookies-in: ~a -> ~a~%"
                   uri (request-uri*)
                   post-parameters (post-parameters*)
                   headers-in (headers-in* )
                   cookies-in (cookies-in*))
      (cond
        (path
         (let ((string-path (princ-to-string path)))
           (if (or (= (mismatch string-path "static/") 7)
                   (= (mismatch string-path "fonts/") 6)
                   (= (mismatch string-path "css/") 4)
                   (= (mismatch string-path "js/") 3))
               (handle-static-file
                (merge-pathnames path (acceptor-document-root acceptor)))
               (interface::%process-hunchentoot-command% list request))))
           (t
            (setf (return-code *reply*) +http-not-found+)
            (abort-request-handler))))))

(in-package :interface)

(defun write-hunchentoot-header (request socket)
  (declare (ignore socket))
  (unless (header-sent request)
    (setf (hunchentoot:header-out :Status)
          (format nil "HTTP/1.0 ~a" (status request)))
    (setf (hunchentoot:header-out :Content-Type) (content-type request)
          (hunchentoot:header-out :Connection) (if (keep-alive request)
                                                   "Keep-Alive"
                                                   "Close"))
    (when (cache-control request)
      (setf (hunchentoot:header-out :Cache-Control) (cache-control request)))
    (unless *reply-protocol*
      (when (content-length request)
	(setf (hunchentoot:header-out :Content-Length) (content-length request)))
      (setf (hunchentoot:header-out :Keep-Socket)
            (if (and (content-length request)
                     (keep-socket request)) "1" "0")))
    (loop for (name . value) in  (header-alist request)
       do (setf (hunchentoot:header-out name) value))
    (setf (header-sent request) t)))

(defun write-hunchentoot-request (request socket)
  (when (clear-content request)
    (setf (content-value request) nil
	  (content-length request) 0))
  (write-header request socket)
  (if (and (content-value request)
           (not (content-sent request)))
      (progn
        ;; if not string we need to cal write-content-value on pathname
        (assert (stringp (content-value request)))
        (let ((x (parse-integer (subseq (status request) 0 3)))) ;; temporary hack
          (hunchentoot::start-output x (content-value request)))
        (setf *close-apache-socket* (or *reply-protocol*
                                        (not (and (content-length request)
                                                  (keep-socket request)))))
        (setf (content-sent request) t))
      (when (not (content-sent request))
        (hunchentoot::start-output (parse-integer (subseq (status request) 0 3))))))

(defun %process-hunchentoot-command% (command original-request)
  (incf *request-counter*)
  (let* ((request (make-instance 'http-request :command command :hunchentoot-request original-request :posted-content (cdr (assoc "posted-content" command :test #'string=)))) ;; instead (decode-posted-content request)
	 (*request-id* (encode-integer *request-counter*)))
    (if (banned-ip-p (%command-param "remote-ip-addr" command))
        (http-message "Blocked IP Address<br>Contact the web site administrator" request)
        (unless (process-http-request request)
          (http-debug-request request)))
    (write-request request hunchentoot::*hunchentoot-stream*)
    (force-output hunchentoot::*hunchentoot-stream*)))
