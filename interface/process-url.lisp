(in-package interface)

(defparameter *country-language-ids* '(("en" . :en)("fr" . :fr)("es" . :es)("it" . :it)))

; URL -> Lambda(request) -> t if OK
(defvar *named-urls* (make-hash-table :test #'equal))

; POST func name -> Lambda(request) -> t if OK
(defvar *post-functions* (make-hash-table :test #'equal))

; Page name -> Lambda(request) -> t if OK
(defvar *named-pages* (make-hash-table :test #'equal))

; Func name -> Lambda(request) -> t if OK
(defvar *named-funcs* (make-hash-table :test #'equal))

; web name -> Lambda(request) -> t if OK (used to process bad requests)
(defvar *web-404* (make-hash-table :test #'equal))

(defun add-named-url (url func)
  (setf (gethash url *named-urls*) func))

(defun remove-named-url (url)
  (remhash url *named-urls*))

(defun add-named-page (page-name func)
  (setf (gethash page-name *named-pages*) func))

(defun remove-named-page (page-name func)
  (remhash page-name *named-pages*))

(defun add-named-func (func-name func)
  (setf (gethash func-name *named-funcs*) func))

(defun remove-named-func (func-name func)
  (remhash func-name *named-funcs*))

(defun add-post-func (func-name func)
  (setf (gethash func-name *post-functions*) func))

(defun add-web-404 (web-name func)
  (setf (gethash web-name *web-404*) func))

(defun remove-web-404 (web-name func)
  (remhash web-name *web-404*))

(defun %command-param (name command)
  (cdr (assoc name command :test #'equal)))

(defparameter %accepted-lang% '(("fr" :fr) ("en" :en)))

(defun find-country-lang (command lang session)
  (setf lang (or lang (country-language session)))
  (unless lang
    (setf lang (%command-param "Accept-Language" command))
    (when lang
      (setf lang (first (util:split-sequence #\, (nsubstitute #\, #\; lang))))))
  (when (stringp lang)
    (setf lang (cdr (assoc lang *country-language-ids* :test 'equal))))
  (setf lang (or lang :en))
  (setf (country-language session) lang))

(defmethod groups (user)
  nil)

(defun process-http-request (request)
  (log-message (format nil "process-http-request  ~%" ))
  (let* ((command (command request))
	 (url (url request)))
    (when url
      (let ((url-func (gethash url *named-urls*)))
	(if url-func
	  (funcall url-func request)
	  (let* ((*request* request)
		 (session-params (decode-session-url url))
		 (*session* (get-session request session-params)))
	    (if *session*
;	     (search-params (decode-search-params url))
;	     (post-func (gethash (getf (cdr (assoc :fdata search-params)) :post) *post-functions*))
	      (let* ((*session-id* (id *session*))
		     (*user-name* (user-name request))
		     (*user* (user *session*))
		     (*user-groups* (groups *user*))
		     (*object* (decode-object-id (getf session-params :object)))
		     (*password* (password request))(first (rassoc *country-language* *country-language-ids*))
		     (*country-language* (find-country-lang command (getf session-params :lang) *session*))
		     (*country-language-id* (first (rassoc *country-language* *country-language-ids*))))
		(setf (country-language *session*) *country-language*
		      (country-language-id *session*) *country-language-id*)
					;	(setf (search-params request) search-params)
		(let ((page-func (or (gethash (getf session-params :func) *named-funcs*)
				     (gethash (getf session-params :page) *named-pages*)
				     (gethash  (%command-param "Host" command) *web-404*))))
		(log-message (format nil "process-http-request4 ~s session-params ~s page-func ~s ~%"
				     url session-params page-func))
		    (if page-func
		      (funcall page-func request)
		      (process-asp-url request))
		    ))
	      (let ((session (create-session request)))
		(setf (getf session-params :session) (id session))
		(interface::redirect-to (interface::encode-session-url nil session-params) request)))))))))

(defun process-asp-url (request)
  (let* ((session-params (session-params request))
	 (user-id (getf session-params :user))
	 (view-id (getf session-params :view))
	 (obj-id  (getf session-params :object))
	 (view (gethash view-id *all-object-views*)))
    (if view
      (with-output-to-request (request)
	(write-html view *request-stream*)
	t)
      (if session-params		 
	(http-message (format nil "the view <B>~a</B> does not exists" view-id) request)
	(http-debug-request request)))))

(add-named-url "/asp/fixed.html"
   #'(lambda (request)
	 (setf (content-value request)
	       "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">
<HTML><HEAD></HEAD><BODY><H1>mod_lisp 2.0</H1><P>This is a constant html string sent by mod_lisp 2.0 + Lispworks + apache.  with content length</P></BODY></HTML>")
	 (setf (content-length request) (length (content-value request)))))

(add-named-url "/asp/unfixed.html"
   #'(lambda (request)
	 (setf (content-value request)
	       "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">
<HTML><HEAD></HEAD><BODY><H1>mod_lisp 2.0</H1><P>This is a constant html string sent by mod_lisp 2.0 + Lispworks + apache. No content length</P></BODY></HTML>")
	 (setf (content-length request) nil)
	 t))

(defun change-url-page (new-page params)
  (setf (getf params :page) new-page)
  (apply 'encode-asp-url nil params))
