(in-package interface)

;; explanations for fractal.js compression and obfuscation
;; v684 and v683 are time out and watchdog counters
;; F6541() is increment timeout counter
;; F6451() is watchdog functions
;; F6514(str) is send-string-to-server(string)
;; v685 is URL postfix for watchdog
;; v686 is URL pull for watchdog
;; F5641() is set URL pull and http-link-id for watchdog
;; v688 is push URL
;; F5614() is set URL push 
;; v687 is packet-sync
;; v689 is http-link-id
;; F5164() is set packet-sync
;; F5146() is check page-packet-sync <= packet-sync
;; Slk is SendLink
;; Fch is fire_on_change 
;; Fck is fire_on_click
;; Fad is fire_add
;; Fcl is fire_on_call

(defvar *http-links* (make-hash-table :test #'equal)); Id->Interface
(defvar *http-link-timeout* 30) ;in seconds
(defvar *http-link-id* 1000)
(defvar *http-link* nil)

(defvar *action-funcs* (make-hash-table :test #'equal)); action->lambda(link item-name value)

(defun add-action-func (action func)
  (setf (gethash action *action-funcs*) func))

(defclass http-link ()
  ((interface-id   :accessor interface-id  :initform (encode-integer (incf *http-link-id*)))
   (session        :accessor session       :initarg :session)
   (last-access-time :accessor last-access-time :initform *session-timer-time*)
;   (object         :accessor object        :initarg :object)
   (views          :accessor views          :initform nil)
   (registered     :accessor registered :initform nil)
   (dispatchers    :accessor dispatchers :initform nil)
   (dirty-dispatchers :accessor dirty-dispatchers :initform nil)
   (output-queue   :accessor output-queue   :initform '()) ;list of strings more recent first
   (output-sent    :accessor output-sent    :initform (loop repeat 10 collect nil)) ;list of ("packet-sync" . strings-list) more recent first
   (output-counter :accessor output-counter :initform 1)
   (url-push     :accessor url-push)
   (url-pull       :accessor url-pull)))

(defmethod initialize-instance :after ((link http-link) &rest init-options &key views &allow-other-keys)
  (setf (gethash (interface-id link) *http-links*) link)
  (let ((url-values (list :link (interface-id link) :session (id (session link)))))
    (setf (url-push link)(encode-session-url nil (list* :func "lpush" url-values)))
    (setf (url-pull link)(encode-session-url nil (list* :func "lpull" url-values)))
    (setf (views link) (loop for (view . obj) in views
			     collect (list view obj)))))

(defun remove-http-link (link)
  (remhash (interface-id link) *http-links*)
  (unregister-http-link link))

; (defun sync-packets (packet-sync http-link)
;   (when packet-sync
;     (if (eql packet-sync (caar (*output-sent http-link)))
;       (setf (output-sent http-link) '())
;       (let ((sync-pos (position packet-sync (cdr (output-sent http-link)) :key #'first :test #'=)))
; 	(when sync-pos
;	  (rplacd (nthcdr sync-pos (output-sent http-link)) nil))))))

(defun send-packets (http-link)
  (map nil 'update-dispatcher-item (dirty-dispatchers http-link))
  (setf (dirty-dispatchers http-link) nil)
  (let ((queue (output-queue http-link)))
    (setf (output-sent http-link)
	  (nconc (output-sent http-link)
		 (list
		  (if queue
		   (progn
		     (setf (output-queue http-link) '())
		     (incf (output-counter http-link));alert(window.F5146+'  '+ parent.F5146 + x_.F5146);
		     (html:html-to-string (html:ffmt "if(x_.F5146(~d)){" (output-counter http-link))
					  (dolist (string (nreverse queue))
					    (write-string string html:*html-stream*))
					  "};" :crlf))
		   nil)))))
  (when (output-sent http-link)
    (html:html (loop for string in (output-sent http-link)
		     when string do (write-string string html:*html-stream*))
	       (html:ffmt "x_.F5164(~d);x_.F5641(~s, ~s);x_.F5614(~s);"
			  (output-counter http-link) (url-pull http-link)
			  (interface-id http-link) (url-push http-link)) :crlf))
  (pop (output-sent http-link)))

(defun send-to-interface (string &optional (http-link *http-link*))
  (when string
    (push string (output-queue http-link))))

(defun send-message-to-interface (message &optional (http-link *http-link*))
  (send-to-interface (format nil "x_.alert('~a');"
			     (html:quote-javascript-string message))
		     http-link))

(defun send-url-to-interface (url &optional (http-link *http-link*))
  (let ((referer (normalize-referer (referer *request*))))
    (when referer
      (push (cons referer url)(url-history *session*))))
  (send-to-interface (format nil "x_.location.href='~a';" url) http-link))

(defun send-open-new-win-to-interface (url &optional (http-link *http-link*))
  (send-to-interface (format nil "x_.open('~a', '_blank', 'toolbar=yes, location=yes, directories=yes,status=yes,menubar=yes,scrollbars=yes,copyhistory=yes, resizable=yes');"
			     url) http-link))

(defun reload-interface (&optional (http-link *http-link*))
  (send-to-interface "x_.location.reload(true);" http-link))

(defvar *count* 1)
(defvar *execute* nil)
#+ignore(defvar *refresh-prefix* "3")

(defun process-http-link-pull (request &optional no-refresh)
  (incf *count*)
  (setf (content-type request) "text/html; charset=iso-8859-1")
  (let* ((interface-id (getf (session-params request) :link ))
	 (*http-link* (when interface-id (gethash interface-id *http-links*))))
    (log-message (format nil "process-http-link-pull ~s~%" (posted-content request)))
    (if *http-link*
      (with-output-to-request (request s)
	(unless (registered *http-link*) (register-http-link *http-link*))
	(setf (last-access-time *http-link*) *session-timer-time*)
	(if *xml-http*
	    (html:html-to-stream s
	     "{var x_=window;"
	     (send-packets *http-link*)
	     "F6541();"
	     (html:ffmt "F5641(~s, ~s);F5614(~s);}"
			(url-pull *http-link*) interface-id (url-push *http-link*)))
	    (html:html-to-stream s
	     (:html ((:body :optional 
			    (:onload "if (!parent.getxh()) setTimeout('location.reload(true)',2000);"))
		     (:jscript "{var x_=parent;"
			       (send-packets *http-link*)
			       "parent.F6541();"
			       (html:ffmt "parent.F5641(~s, ~s);parent.F5614(~s);}"
					  (url-pull *http-link*) interface-id (url-push *http-link*)))
		     (:p *count* " " (html:ffmt "~s" (url-pull *http-link*)))
		     ))))
	(with-output-to-request (request s)
	  (html:html-to-stream s
	       (:html (:body
		       (:p "no-refresh")
		       (:jscript "parent.location.reload(true);")))))))
    t))

(add-named-func "lpull" 'process-http-link-pull)
;(add-named-func "lpull" #'(lambda (r) (process-http-link-pull r t)))

(defun process-http-link-push (request) 
  (with-posted-strings (request (action "v654")(name "v645")(value "v465")(*xml-http* "v564"))
    (log-message (format nil "process-http-link-push ~s~%" (posted-content request))) (setf %req% request) 
    (let ((action-func (gethash action *action-funcs*)))
      (when action-func
	(let* ((link-id (getf (session-params request) :link ))
	       (*http-link* (when link-id (gethash link-id *http-links*))))
	  (when *http-link*
	    (handler-bind
		((error #'(lambda (e)
			    (let ((bt '())
				  error-string)
			      (mp:map-process-backtrace mp:*current-process*
							#'(lambda (o)
							    (push o bt)))
			      (with-standard-io-syntax
				  (let ((*print-readably* nil))
				    (setf error-string
					  (html:html-to-string
					   (:html
					    (:head)
					    (:body
					     (:fformat "Error : ~a~%" e) :br
					     (:format "Backtrace:<br>~{ ~S <br>~}" (nreverse bt))))))))))))
	      (funcall action-func *http-link* name value)))))
      (process-http-link-pull request t))))

(add-named-func "lpush" 'process-http-link-push)

(defun register-http-link (link)
  (let ((dispatchers (make-hash-table :test #'equal))
        (dispatcher-list ()))
    (log-message (format nil "register-http-link ~s ~%" link))
    (setf (dispatchers link) dispatchers)
    (loop for (view *object*) in (views link)
	  when (and view *object*) do
	  (let ((*user-groups* (dynamic-groups *user* *object* *user-groups*)))
	    (meta::load-object-data *object*)
	    (maphash #'(lambda (name item)
			 (when (visible-p item)
			   (let ((dispatcher (make-dispatcher link *object* item)))
                             (setf (view dispatcher) view)
			     (setf (gethash name dispatchers) dispatcher)
                             (push dispatcher dispatcher-list))))
		     (all-items view))))
  (dolist (dispatcher dispatcher-list)
		     (update-dispatcher-item dispatcher t)))
  (setf (registered link) t))

(defun unregister-http-link (link)
  (when (dispatchers link)
    (maphash #'(lambda (name dispatcher)
		 (when dispatcher
		   (unregister-dispatcher dispatcher)))
	     (dispatchers link))))

(defun change-slot (link item-name value)
  (log-message (format nil "change slot ~s ~s~%" item-name value))
  (let ((dispatcher (gethash item-name (dispatchers link))))
    (unless value (setf value ""))
    (multiple-value-bind (new-value ok) (safely-convert-string-to-value dispatcher value)
      (if ok
	  (try-change-slot dispatcher new-value)
	  (update-dispatcher-item dispatcher t)))))

(add-action-func "4" 'change-slot)

(defun fire-click (link item-name click-id-str)
  (log-message (format nil "fire-click ~s ~s~%" item-name click-id-str))
  (let ((dispatcher (gethash item-name (dispatchers link)))
	(click-id (parse-integer click-id-str :junk-allowed t)))
    (log-message (format nil "dispatcher ~s~%" dispatcher))
    (when dispatcher (fire-action dispatcher click-id click-id-str))))

(add-action-func "8" 'fire-click)

(defun fire-add (link item-name value)
  (log-message (format nil "fire-add ~s ~s~%" item-name value))
  (let ((dispatcher (gethash item-name (dispatchers link))))
    (log-message (format nil "dispatcher ~s~%" dispatcher))
    (let ((new-value (decode-object-id value)))
      (when new-value
	(fire-add-to-list dispatcher new-value)))))

(add-action-func "12" 'fire-add)

(defun http-link-timer ()
  (maphash #'(lambda (id link)
	       (declare (ignore id))
	       (when (and link
			  (> (- *session-timer-time* (last-access-time link)) *http-link-timeout*))
		 (remove-http-link link)))
	   *http-links*))

(defun start-http-link-timer ()
  (let ((timer (mp:make-timer 'http-link-timer)))
    (mp:schedule-timer-relative timer 30 30)))

(defvar *http-link-timer* (start-http-link-timer))

(defun connect-tag (attributes forms)
  (destructuring-bind (&optional views) forms
    `(let ((http-link-url (url-pull (make-instance 'http-link :session *session* :views ,views))))
      (html::optimize-progn
       ,(html::html-gen `(:jscript "window.setInterval('F6451()', 2000);"
			  (html:ffmt "v686=~s;" http-link-url)))
       ,(html::html-gen `((:iframe :id "Lisp1" :name "Lisp1" :frameborder "0"
			   :src http-link-url :scrolling "0" :style "width:1px;height:1px;")))
;       ,(html::html-gen `(:jscript "F6451();"))))
    ))))

;example : :connect
(html::add-func-tag :connect 'connect-tag)

(defun encode-views (views func)
  (encode-session-url nil
     (list* :session (id *session*) :func func
	    (loop for (view . object) in views
		  nconc (list :view (name view) :object (encode-object-id object))))))

(defun process-http-popup (request &optional no-refresh)
(break)
  (let* ((interface-id (getf (session-params request) :link ))
	 (interface (when interface-id (gethash interface-id *http-links*))))
    (log-message (format nil "process-http-link-pull ~s~%" (posted-content request)))
    (if interface
      (with-output-to-request (request s)
	(unless (registered interface) (register-http-link interface))
	(setf (last-access-time interface) *session-timer-time*)
	(html:html-to-stream s
	     (:html (:head ((:meta :http-equiv "Content-Type" :content "text/html; charset=iso-8859-1")))
		    ((:body :optional (:onload (unless no-refresh "if (!getxh()) setTimeout('location.reload(true)',2000);")))
		     (:p "no-refresh "no-refresh)
		     (:jscript (send-packets interface)
			       "parent.F6541();"
			       (html:ffmt "parent.F5641(~s);parent.F5614(~s);" (url-pull interface)(url-push interface)))
		     (:p *count* " " (html:ffmt "~s" (url-pull interface)))
		     ))))
      (redirect-to (url-pull (make-instance 'http-link :session *session* :params (session-params request))) request)))
  t)

(add-named-func "popup" 'process-http-popup)
 