(in-package interface)

(net.aserve:publish-prefix 
 :prefix "/asp"
 :function 'process-aserve-command)

(defun process-aserve-command (req ent)
  (setf (net.aserve:request-reply-strategy req)
	(remove :keep-alive (net.aserve:request-reply-strategy req)))
  (let* ((*apache-socket* (net.aserve:request-socket req))
	 (*reply-protocol* :http))
    (%process-apache-command% 
     (list (cons "url" (puri:uri-path (net.aserve:request-uri req)))
	   (cons "posted-content" (net.aserve:get-request-body req))
	   (cons "content-type" (net.aserve:header-slot-value req :content-type))))))

#+nil
(defun process-aserve-command (req ent)
  (net.aserve:with-http-response (req ent)
    (net.aserve:with-http-body (req ent)
      (html:html-to-stream (net.aserve:request-reply-stream req)
			   "Hello ASP World!" :br
			   (:esc (puri:uri-path (net.aserve:request-uri req)))
			   (:esc (format nil "~s" (net.aserve:get-request-body req)))))))
