;;;; AllegroServe Example

(defpackage "ASERVE-EXAMPLE"
  (:use 
   #:COMMON-LISP 
   #:ACL-COMPAT.EXCL
   #:NET.HTML.GENERATOR
   #:NET.ASERVE)
  (:export
   #:start-server
   #:stop-server
   #:start-simple-server))

(in-package :aserve-example)

(defparameter *hit-counter* 0)

(publish :path "/" 
	 :content-type "text/html"
	 :function
	 #'(lambda (req ent)
	     (with-http-response (req ent)
	       (with-http-body (req ent)
		 (html
		  (:head (:title "Welcome to Portable AllegroServe on " (:princ (lisp-implementation-type))))
		  (:body (:center ((:img :src "aservelogo.gif")))
			 (:h1 "Welcome to Portable AllegroServe on " (:princ (lisp-implementation-type)))
			 (:p "These links show off some of Portable AllegroServe's capabilities. ")
			 (:i "This server's host name is "
			     (:princ-safe (header-slot-value req :host)))
                         #+unix
                         (:i ", running on process " (:princ (net.aserve::getpid)))
                         :br
                         (:princ (incf *hit-counter*)) " hits"
			 :p
			 (:b "Sample pages") :br
			 ((:a :href "apropos") "Apropos") :br
			 ((:a :href "pic") "Sample jpeg") :br
			 ((:a :href "pic-redirect") "Redirect to previous picture") :br
			 ((:a :href "pic-gen") "generated jpeg") "- hit reload to switch images" :br
			 ((:a :href "cookietest") "test cookies") :br
			 ((:a :href "secret") "Test manual authorization")
			 " (name: " (:b "foo") ", password: " (:b "bar") ")"
			 :br
			 ((:a :href "secret-auth") "Test automatic authorization")
			 " (name: "
			 (:b "foo2")
			 " password: "
			 (:b "bar2") ")"
			 :br
			 ((:a :href "local-secret") "Test source based authorization") 
                         " This will only work if you can use "
			 "http:://localhost ... to reach this page" ;:
			 :br
			 ((:a :href "local-secret-auth") 
			  "Like the preceding but uses authorizer objects")
			 :br
			 ((:a :href "timeout") "Test timeout")
			 :br
			 ((:a :href "getfile") "Client to server file transfer")
			 :br
			 ((:a :href "missing-link") "Missing Link")
			 " should get an error when clicked"
			 )
		  
		  )))))

;; a very simple page.  This is so simple it doesn't put out the required
;; tags (like <html>) yet I suspect that most browsers will display it
;; correctly regardless.
(publish :path "/hello"
	 :content-type  "text/html"
	 :function #'(lambda (req ent)
		       (with-http-response (req ent)
			 (with-http-body (req ent)
			   (html "Hello World!")))))

;; this is the "/hello" example above modified to put out the correct
;; html tags around the page.
(publish :path "/hello2"
	 :content-type  "text/html"
	 :function #'(lambda (req ent)
		       (with-http-response (req ent)
			 (with-http-body (req ent)
			   (html 
			    (:html
			     (:body "Hello World!")))))))

;; display a picture from a file.
(publish-file :path "/pic" :file  "ASERVE:examples;prfile9.jpg"
	      :content-type "image/jpeg")

(publish-file :path "/aservelogo.gif" :file "ASERVE:examples;aservelogo.gif"
	      :content-type "image/gif")

(publish :path "/pic-gen"
	 :content-type "image/jpeg"
	 :function
	 (let ((selector 0)) ; chose one of two pictures
	   #'(lambda (req ent)
	       (with-http-response (req ent :format :binary)
		 (with-http-body (req ent)
		   ; here is where you would generate the picture.
		   ; we're just reading it from a file in this example
		   (let ((stream (request-reply-stream req)))
		     (with-open-file (p (nth selector
					     `("ASERVE:examples;prfile9.jpg"
					        "ASERVE:examples;fresh.jpg"))
				      :element-type '(unsigned-byte 8))
		       (setq selector (mod (1+ selector) 2))
		       (loop for val = (read-byte p nil nil)
                             while val do (write-byte val stream)))))))))

(publish :path "/pic-redirect"
	 :content-type "text/html"
	 :function
	 #'(lambda (req ent)
	     (with-http-response (req ent
				      :response *response-moved-permanently*)
	       (setf (reply-header-slot-value req :location) "pic")
	       (with-http-body (req ent)
		 ;; this is optional and most likely unnecessary since most 
		 ;; browsers understand the redirect response
		 (html 
		  (:html
		   (:head (:title "Object Moved"))
		   (:body 
		    (:h1 "Object Moved")
		    "The picture you're looking for is now at "
		    ((:a :href "pic") "This location"))))))))

(publish :path "/tform" 
	 :content-type "text/html"
	 :function
	 (let ((name "unknown"))
	   #'(lambda (req ent)
	       (let ((body (get-request-body req)))
		 (format t "got body ~s~%" body)
		 (let ((gotname (assoc "username"
				       (form-urlencoded-to-query body)
					:test #'equal)))
		   (when gotname
                     (setq name (cdr gotname)))))

	       (with-http-response (req ent)
		 (with-http-body (req ent)
		   (html (:head (:title "test form"))
			 (:body "Hello " (:princ-safe name) ", "
				"Enter your name: "
				((:form :action "/tform"
					:method "post")
				 ((:input :type "text"
					  :maxlength 10
					  :size 10
					  :name "username"))))))))))


;; example of a form that uses that 'get' method
;;
(publish :path "/apropos"
         :content-type "text/html"
         :function
         #'(lambda (req ent)
     
             (let ((lookup (assoc "symbol" (request-query req) :test #'equal)))
               (with-http-response (req ent)
                 (with-http-body (req ent)
                   (html (:head (:title "Allegro Apropos"))
                         ((:body :background "aserveweb/fresh.jpg")
                          "New Apropos of "
                          ((:form :action "apropos"
                                  :method "get")
                           ((:input :type "text"
                                    :maxlength 40
                                    :size 20
                                    :name "symbol")))
                          :p
                          (if lookup
                              (let ((ans (apropos-list (cdr lookup))))
                                (html :hr (:b "Apropos") " of "
                                      (:princ-safe (cdr lookup))
                                      :br
                                      :br)
                                (if (null ans)
                                    (html "No Match Found")
                                    (macrolet ((my-td (str)
                                                 `(html ((:td 
                                                          :bgcolor "blue")
                                                         ((:font :color "white"
                                                                 :size "+1")
                                                          (:b ,str))))))
                                      (html ((:table
                                              :bgcolor "silver"
                                              :bordercolor "blue"
                                              :border 3
                                              :cellpadding 3)
                                             (:tr
                                              (my-td "Symbol")
                                              (my-td "boundp")
                                              (my-td "fboundp"))
                                             (dolist (val ans)
                                               (html (:tr
                                                      (:td (:prin1-safe val))
                                                      (:td (:prin1 (and (boundp val) t)))
                                                      (:td (:prin1 (and (fboundp val) t))))
                                                     :newline)))))))
                              (html "Enter name and type enter")))
                         :newline))))))

;; a preloaded picture file
(publish-file :path "/aserveweb/fresh.jpg"
	      :file "ASERVE:examples;fresh.jpg"
	      :content-type "image/jpeg"
	      :preload t)

;; a preloaded text file
(publish-file :path "/foo"
	      :file "ASERVE:examples;foo.txt"
	      :content-type "text/plain"
	      :preload t)

(publish-file :path "/foo.txt"
	      :file "ASERVE:examples;foo.txt"
	      :content-type "text/plain"
	      :preload nil)

;; some entries for benchmarking
(publish-file :path "/file2000"
	      :file "ASERVE:examples;file2000.txt"
	      :content-type "text/plain"
	      :preload nil)

(publish-file :path "/file2000-preload"
	      :file "ASERVE:examples;file2000.txt"
	      :content-type "text/plain"
	      :preload t)
	

(publish :path "/dynamic-page"
	 :content-type "text/plain"
	 :function #'(lambda (req ent)
		       (with-http-response (req ent)
			 (with-http-body (req ent)
			   (html "This is a dynamic page")))))

;; an example which causes the web browser to put up the
;; name/password box and if you enter the name "foo" and password "bar"
;; then you get access to the secret info.
(publish :path "/secret"
	 :content-type "text/html"
	 :function
	 #'(lambda (req ent)
	     (multiple-value-bind (name password) (get-basic-authorization req)
	       (if (and (string= name "foo") (string= password "bar"))
                   (with-http-response (req ent)
			 (with-http-body (req ent)
			   (html (:head (:title "Secret page"))
				 (:body "You made it to the secret page"))))
                   (with-http-response (req ent
                                            :response *response-unauthorized*)
                     (set-basic-authorization req "secretserver")
                     (with-http-body (req ent)))))))

(publish :path "/local-secret"
	 :content-type "text/html"
	 :function
	 #'(lambda (req ent)
	     (let ((net-address (ash (acl-socket:remote-host
				      (request-socket req))
				     -24)))
	       (if (equal net-address 127)
		   (with-http-response (req ent)
                     (with-http-body (req ent)
                       (html (:head (:title "Secret page"))
                             (:body (:b "Congratulations. ")
                                    "You are on the local network"))))
                   (with-http-response (req ent)
                     (with-http-body (req ent)
                       (html
                        (:html (:head (:title "Unauthorized"))
                               (:body 
                                "You cannot access this page "
                                "from your location")))))))))


(publish :path "/local-secret-auth"
	 :content-type "text/html"
	 :authorizer (make-instance 'location-authorizer
		       :patterns '((:accept "127.0.0.0" 8)
				   (:accept "tiger.franz.com")
				   :deny))
	 :function
	 #'(lambda (req ent)
	     (with-http-response (req ent)
	       (with-http-body (req ent)
		 (html (:head (:title "Secret page"))
		       (:body (:b "Congratulations. ")
			      "You made it to the secret page"))))))

;; these two urls show how to transfer a user-selected file from
;; the client browser to the server.
;; 
;; We use two urls (/getfile to put up the form and /getfile-post to
;; handle the post action of the form).   We could have done it all
;; with one url but since there's a lot of code it helps in the
;; presentation to separate the two.
;;
(publish :path "/getfile"
	 :content-type "text/html; charset=utf-8"
	 :function
	 #'(lambda (req ent)
	     (with-http-response (req ent)
	       (with-http-body (req ent)
		 (html (:head "get file")
		       (:body
			((:form :enctype "multipart/form-data"
				:method "post"
				:action "getfile-got")
			 "Let me know what file to grab"
			 :br
			 ((:input :type "file" 
				  :name "thefile"
				  :value "*.txt"))
			 :br
			 ((:input :type "text" :name "textthing"))
			 "Enter some text"
			 :br
			 ((:input :type "checkbox" :name "checkone"))
			 "check box one"
			 :br
			 ((:input :type "checkbox" :name "checktwo"))
			 "check box two"
			 :br
			 ((:input :type "submit")))))))))


(publish :path "/secret-auth"
	 :content-type "text/html"
	 :authorizer (make-instance 'password-authorizer
		       :allowed '(("foo2" . "bar2")
				  ("foo3" . "bar3")
				  )
		       :realm  "SecretAuth")
	 :function
	 #'(lambda (req ent)
	     (with-http-response (req ent)
	       (with-http-body (req ent)
		 (html (:head (:title "Secret page"))
		       (:body "You made it to the secret page"))))))


;; this called with the file from 
(publish :path "/getfile-got"
	 :content-type "text/html; charset=utf-8"
	 :function
	 #'(lambda (req ent)
	     (with-http-response (req ent)
	       (let (files-written
                     text-strings)
		 (loop for h = (get-multipart-header req)
                       while h
                       ;; we can get the filename from the header if 
                       ;; it was an <input type="file"> item.  If there is
                       ;; no filename, we just create one.
                       do (let ((cd (assoc :content-disposition h :test #'eq))
                                (filename)
                                (sep))
                            (when (and cd (consp (cadr cd)))
                              (setq filename (cdr (assoc "filename" 
                                                         (cddr (cadr cd))
                                                         :test #'equalp)))
                              (when filename
                                ;; locate the part of the filename after
                                ;; the last directory separator.  the
                                ;; common lisp pathname functions are no
                                ;; help since the filename syntax may be
                                ;; foreign to the OS on which the server
                                ;; is running.
                                (setq sep
                                      (max (or (position #\/ filename
                                                         :from-end t) -1)
                                           (or (position #\\ filename
                                                         :from-end t) -1)))
                                (setq filename
                                      (subseq filename (1+ sep)
                                              (length filename)))))
                            (if filename
                                (progn
                                  (push filename files-written)
                                  (with-open-file (pp filename :direction :output
                                                      :if-exists :supersede
                                                      :element-type '(unsigned-byte 8))
                                    (format t "writing file ~s~%" filename)
                                    (let ((buffer (make-array 4096
                                                              :element-type 
                                                              '(unsigned-byte 8))))
                                      ;; note: we could also use
                                      ;; get-all-multipart-data here
                                      (loop for count = (get-multipart-sequence
                                                         req buffer)
                                            while count
                                            do (write-sequence buffer pp :end count)))))
                                ;; no filename, just grab as a text string
                                (let ((buffer (make-string 1024)))
                                  (loop for count = (get-multipart-sequence
                                                     req buffer
                                                     :external-format :utf8-base)
                                        while count
                                        do (push (subseq buffer 0 count) text-strings))))))
		 ;; now send back a response for the browser
		 (with-http-body (req ent :external-format :utf8-base)
		   (html (:html (:head (:title "form example"))
				(:body "-- processed the form, files written --"
				       (dolist (file (nreverse files-written))
					 (html :br "file: "
					       (:b (:prin1-safe file))))
				       :br
				       "-- Non-file items Returned: -- " :br
				       (dolist (ts (reverse text-strings))
					 (html (:princ-safe ts) :br))))))))))


(publish :path "/cookietest"
	 :content-type "text/html"
	 :function
	 #'(lambda (req ent)
	     (with-http-response (req ent)
	       (set-cookie-header req 
				  :name "froba" 
				  :value "vala"
				  :path "/"
				  :expires :never)
	       (set-cookie-header req 
				  :name "frob2" 
				  :value "val2"
				  :path "/"
				  :expires :never)
	       (set-cookie-header req 
				  :name "frob3-loooooooooooooong" 
				  :value "val3-loooooooooooooong"
				  :path "/"
				  :expires :never)
	       (set-cookie-header req
				  :name "the time"
				  :value (net.aserve::universal-time-to-date
					  (get-universal-time))
				  :path "/cookieverify"
				  :expires (+ (get-universal-time)
					      (* 20 60) ; 20 mins
					      )
				  )
				  
	       (with-http-body (req ent)
		 (html (:head (:title "Cookie Test"))
		       (:body "you should have a cookie now."
			      " Go "
			      ((:a :href "cookieverify") "here")
			      " to see if they were saved"))))))

(publish :path "/cookieverify"
	 :content-type "text/html"
	 :function
	 #'(lambda (req ent)
	     (let ((cookie-info (get-cookie-values req)))
	       (with-http-response (req ent)
		 (with-http-body (req ent)
		   (html (:head (:title "Cookie results"))
			 (:body
			  "The following cookies were returned: " 
			  (:prin1-safe cookie-info))))))))

(publish :path "/timeout"
	 :content-type "text/html"
	 :function
	 #'(lambda (req ent)
	     ;; do nothing interesting so that the timeout will
	     ;; occur
	     (with-http-response (req ent :timeout 15)
	       (loop (sleep 5)))))

(publish :path "/long-slow"
	 :content-type "text/plain"
	 :function
	 #'(lambda (req ent)
	     ;; chew up cpu time in a look that blocks 
	     ;; the scheduler from running so this aserve
	     ;; won't accept any more connections and we can
	     ;; demo the multiple process version
	     ; takes 50 secs on a 1.2ghz Athlon
	     (locally (declare (optimize (speed 3) (safety 0)))
	       (dotimes (aa 500)
		 (declare (fixnum aa))
		 (dotimes (j 300)
		   (declare (fixnum j))
		   (dotimes (i 10000) 
		     (declare (fixnum i))
		     (let ((k (+ i j)))
		       (declare (fixnum k))
		       (setf k (- i j))
		       (setf k (+ i j k))
		       (setf k (- i j k)))))))

	     (with-http-response (req ent)
	       (with-http-body (req ent)
		 (html "done")))))


(defun start-server (&rest args &key (port 2001) &allow-other-keys)
  (apply #'net.aserve:start :port port args))

(defun stop-server ()
  (net.aserve:shutdown))

(defun start-simple-server (&key (port 2001))
  (net.aserve:start :port port :chunking nil :keep-alive nil :listeners 0))



#|
(in-package :aserve-example)
(use-package :net.aserve.client)

(setq cookies (make-instance 'cookie-jar))
(do-http-request "http://www.dataheaven.de/" 
	:cookies cookies
	:protocol :http/1.0)
(net.aserve.client::cookie-jar-items cookies)
|#	
