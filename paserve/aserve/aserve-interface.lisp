(in-package interface)

(net.aserve:publish-prefix 
 :prefix "/asp"
 :function 'process-aserve-command)

(defun process-aserve-command (req ent)
  (setf (net.aserve:request-reply-strategy req)
	(remove :keep-alive (net.aserve:request-reply-strategy req)))
  (let* ((*apache-socket* (net.aserve:request-socket req))
	 (*reply-protocol* :http))
    (util:with-logged-errors ()
      (%process-apache-command% 
       (list (cons "url" (puri:uri-path (net.aserve:request-uri req)))
	     (cons "posted-content" (net.aserve:get-request-body req))
	     (cons "content-type" (net.aserve:header-slot-value req :content-type)))))))

