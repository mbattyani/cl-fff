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

(defmethod process-page-fn (user page-fn request)
  (if page-fn
      (funcall page-fn request)
      (process-asp-url request)))

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
                (unless (equal (cookie *session*) (cookie *request*))
                  (setf (cookie *session*) (cookie *request*)))
		(setf (country-language *session*) *country-language*
		      (country-language-id *session*) *country-language-id*)
					;	(setf (search-params request) search-params)
		(let ((page-fn (or (gethash (getf session-params :func) *named-funcs*)
				   (gethash (getf session-params :page) *named-pages*)
				   (gethash  (%command-param "Host" command) *web-404*))))
		(log-message (format nil "process-http-request4 ~s session-params ~s page-func ~s ~%"
				     url session-params page-fn))
		    (process-page-fn *user* page-fn request)))
	      (let ((session (create-session request)))
		(setf (getf session-params :session) (id session))
		(interface::redirect-to (interface::encode-session-url nil session-params) request)))))))))

(defun process-asp-url (request)
  )

(add-named-url "/asp/fixed.html"
   #'(lambda (request)
	 (setf (content-value request)
	       "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">
<HTML><HEAD></HEAD><BODY><H1>mod_lisp 2.0</H1><P>This is a constant html string sent by mod_lisp 2.0 + Lispworks + apache.  with content length</P></BODY></HTML>")
	 (setf (content-length request) (length (content-value request)))))

(add-named-url "/asp/aserve.html"
   #'(lambda (request)
	 (setf (content-value request)
	       "<head><title>Welcome to Portable AllegroServe on LispWorks</title></head><body><center><img src=\"aservelogo.gif\"></center><h1>Welcome to Portable AllegroServe on LispWorks</h1><p>These links show off some of Portable AllegroServe's capabilities. </p><i>This server's host name is 127.0.0.1:2001</i><br>7 hits<p><b>Sample pages</b><br><a href=\"apropos\">Apropos</a><br><a href=\"pic\">Sample jpeg</a><br><a href=\"pic-redirect\">Redirect to previous picture</a><br><a href=\"pic-gen\">generated jpeg</a>- hit reload to switch images<br><a href=\"cookietest\">test cookies</a><br><a href=\"secret\">Test manual authorization</a> (name: <b>foo</b>, password: <b>bar</b>)<br><a href=\"secret-auth\">Test automatic authorization</a> (name: <b>foo2</b> password: <b>bar2</b>)<br><a href=\"local-secret\">Test source based authorization</a> This will only work if you can use http:://localhost ... to reach this page<br><a href=\"local-secret-auth\">Like the preceding but uses authorizer objects</a><br><a href=\"timeout\">Test timeout</a><br><a href=\"getfile\">Client to server file transfer</a><br><a href=\"missing-link\">Missing Link</a> should get an error when clicked</body>")
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
