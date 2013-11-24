(in-package interface)

(defun gen-localize-html (obj &key home-url)
  (let ((descriptions (localize obj)))
    (if (> (reduce #'+ descriptions :key #'(lambda (x) (length (first x)))) 20)
        (html:html
         ((:table :border "0")
          (:tr ((:td :colspan "20") 
                ((:a :href home-url) (:translate '(:fr "Accueil" :en "Home"))) " &gt"))
          (loop for (description url) in descriptions 
                for i from 1 do
                (html:html
                 (:tr (loop repeat i do (html:html ((:td :width "15px") " ")))
                      ((:td :colspan (format nil "~d" (- 10 i)))
                       ((:a :href url) (:esc description)) " &gt "))))) :br)
        (html:html
         ((:a :href home-url) (:translate '(:fr "Accueil" :en "Home"))) " &gt "
         (loop for ((description url) . next) on descriptions do
               (html:html ((:a :href url) (:esc description)) (:when next " &gt "))) :br))))

(in-package #:meta-web)

(setf (gethash "/" interface::*url-params-aliases*) (list :page "home"))

(defmacro encode-page (page-name)
  (format t "page-name ~a~%" page-name)
  (assert (stringp page-name))
  `(if (interface::new-cookie *request*)
       ,(interface::encode-page page-name)
       ,(concatenate 'string "/" page-name)))

(defun write-translation-block ()
  (unless (eq interface::*country-language* :fr)
    (html:html ((:a :class "countryi" :href
                    (interface::encode-session-values
                     nil
                     (list :page (name *page*) :lang "fr" :func "slang")))
		((:img :border "0" :src "/static/sfr3.gif")) #+nil "&nbsp;Français")))
  (unless (eq interface::*country-language* :en)
    (html:html ((:a :class "countryi" :href
		    (interface::encode-session-values
		     nil
		     (list :page (name *page*) :lang "en" :func "slang")))
		((:img :src "/static/suk3.gif" :border "0"))))))

(defun home-block ()
  (gen-localize-html *object*))

(defclass identified-user ()
  ((clipboard :accessor clipboard :initform (make-instance 'interface::clipboard :store meta::*memory-store*))))

(defmethod interface::clipboard ((user identified-user))
  (clipboard user))

(defun insert-page-tile ()
  (let* ((object-id (getf (interface::session-params *request*) :object))
         (object (interface::decode-object-id object-id)))
    (html:html "Web App Framework" (:when object " - "(:esc (meta::short-description object))))))

(defun write-page (content-func title)
 (let ((authentification-result (check-authentification)))
   (html:html
    (:doctype)
    (:html
     (:head
      (:title (insert-page-tile))
      ((:meta :name "description" :content "The F3 Web Interface"))
      ((:meta :name "keywords" :content ""))
      ((:meta :http "http" :equiv "content-type" :content "text/html; charset=UTF-8"))
      ((:meta :http "http" :equiv "Content-Style-Type" :content "text/css"))
      ((:meta :http "http" :equiv "imagetoolbar" :content "no"))
      ((:meta :http "http" :equiv "content-language" :content "EN"))
      ((:meta :name "robots" :content "index, follow"))
      ((:link :rel "shortcut icon" :href "/static/favicon.ico" :type "image/x-icon"))
      ((:link :rel "stylesheet" :href "/static/css/fcweb.css"))
      ((:link :rel "stylesheet" :href "/static/css/modal.css"))
      (:when-frontends '(:bootstrap)
         ((:link :rel "stylesheet" :href "//netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap.min.css"))
         ((:link :rel "stylesheet" :href "/static/css/modal.css"))
         ((:script :src "//netdna.bootstrapcdn.com/bootstrap/3.0.2/js/bootstrap.min.js")))
      ((:script :src "/static/fgt.js"))
      )
     ((:body)
      (:when-frontends '(:bootstrap) "Using Bootstrap")
      (:when-frontends '(:html) "Using plain HTML")
      :br
      ((:img :border "0" :src "/static/made-with-lisp-logo.jpg"))
      ((:div :style "padding:5px;")
       (home-block)
       (authentification-block authentification-result)
       :use-ui-ws
       (funcall content-func))
      ((:script :src "/static/fractal-ws.js")))))))

(defclass page-desc ()
  ((name :accessor name :initform "" :initarg :name)
   (hidden :accessor hidden :initform nil :initarg :hidden)
   (restricted :accessor restricted :initform nil :initarg :restricted)
   (encoded-name :accessor encoded-name)
   (file-name :accessor file-name :initform "")
   (path :accessor path :initform nil)
   (title :accessor title :initform "" :initarg :title)
   (title-en :accessor title-en :initform "" :initarg :title-en)
   (abrev :accessor abrev :initform "" :initarg :abrev)
   (abrev-en :accessor abrev-en :initform "" :initarg :abrev-en)
   (sub-page-names :accessor sub-page-names :initform () :initarg :sub-pages)
   (sub-pages :accessor sub-pages :initform ())
   (linked-content :accessor linked-content :initform nil :initarg :linked-content)
   (content :accessor content :initform nil :initarg :content)
   (content-en :accessor content-en :initform nil :initarg :content-en)
   (content-func :accessor content-func :initform nil :initarg :content-func)))

(defmethod initialize-instance :after ((page page-desc) &rest init-options &key &allow-other-keys)
  (declare (ignore init-options))
  (setf (encoded-name page) (interface::encode-page (name page)))
  (setf (gethash (name page) *pages*) page)
  (setf (gethash (concatenate 'string "/" (name page)) interface::*url-params-aliases*) (list :page (name page)))
  (interface::add-named-page (name page) 'process-repository-request))

(defun get-page (page-name)
  (gethash page-name *pages*))

(defun process-repository-request (request)
  (let* ((page-name (getf (interface::session-params request) :page))
	 (*page* (gethash page-name *pages*)))
    (when *page*
      (let ((html-func (content-func *page*)))
	(interface::with-output-to-request (request html:*html-stream*)
	  (write-page html-func (concatenate 'string "Ivan : " (translated-title *page*))))))))

(defmethod content-func :around ((page page-desc))
  (let ((func (call-next-method)))
    (if func
	func
	(if (linked-content page)
	    (content-func (get-page (linked-content page)))
	    (let ((french (content page)))
	      (setf (content-func page)
		    (let ((html:*html-insert-file-defaults* *source-pages-default*)
			  (*package* (find-package 'sgna)))
		      (compile nil `(lambda ()
				     (when t #+nil(or (not (restricted ,page))
					       (interface::authentified interface::*session*)
					       (check-authentification ))
				       (cond
					 ((eq interface::*country-language* :fr) ,@(mapcar 'html::html-gen french))
					 (t ,@(mapcar 'html::html-gen (content-en page))))))))))))))

(defvar %unique-user% (make-instance 'identified-user))
(defun ensure-user ()
  (unless *user*
;    (setf (interface::authentified *session*) nil)
;    (switch-user (make-instance 'anonymous-user :store meta::*memory-store*))
    (setf (interface::authentified *session*) t)
    (switch-user %unique-user%)))

(defun switch-user (user)
  (interface::clear-url-history *session*)
  (setf *user* user
        (interface::user *session*) *user*
;        (last-access user) (get-universal-time)
        *user-groups* (interface::groups *user*)))

(defun gen-localize-html (obj)
  nil)
(defun check-authentification ()
  (ensure-user)
  t)
#+nil
(defun check-authentification ()
  (ensure-user)
  (interface::decode-posted-content interface::*request*)
  (setf %c% (interface::posted-content interface::*request*))
  (let* ((posted-content (interface::posted-content interface::*request*))
	 (name (cdr (assoc "name" posted-content :test 'string=)))
	 (password (cdr (assoc "password" posted-content :test 'string=)))
	 (password2(cdr (assoc "password2" posted-content :test 'string=)))
	 (login (cdr (assoc "login" posted-content :test 'string=)))
	 (register (cdr (assoc "register" posted-content :test 'string=)))
         (remember (cdr (assoc "remember" posted-content :test 'string=)))
	 (authorized nil))
    (if name
      (let ((user (find name (user-list *sys-params*) :key 'identifier :test #'string=)))
        (when login
          (if (and user (equal password (password user)))
              (progn
                (switch-user user)
                (setf (interface::authentified *session*) t)
                (when remember 
                  (setf (automatic-login user) t
                        (cookie user) (interface::cookie *session*)))
                (return-from check-authentification t))
              (return-from check-authentification :failed)))
        (when register
          (if user
              (return-from check-authentification :exists-already)
              (if (string= password password2)
                  (let ((user (make-instance 'appli-user :parent *sys-params*)))
                    (switch-user user)
                    (setf (identifier user) name
                          (password user) password)
                    (setf (interface::authentified *session*) t)
                    (push user (user-list *sys-params*))
                    (setf (cookie user) (interface::cookie *session*))
                    (when remember 
                      (setf (automatic-login user) t))
                    (return-from check-authentification t))
                  (return-from check-authentification :passwords-mismatch)))))
      (when (interface::cookie *session*)
        (let ((user (find (interface::cookie *session*) 
                          (user-list *sys-params*) :key 'cookie :test #'string=)))
          (when (and user (automatic-login user))
            (switch-user user)
            (setf (interface::authentified *session*) t)
            t))))))

(defun authentification-block (status)
  (unless (interface::authentified *session*)
    (case status
      (:failed (html:html ((:p :style "color:red;") "Bad identifier and/or password")))
      (:exists-already (html:html ((:p :style "color:red;") "This identifier already exists")))
      (:passwords-mismatch (html:html ((:p :style "color:red;") "Passwords mismatch")))))
    (html:html
     ((:script :src "/sha1.js"))
     (:on-off ((:a :href "#")
               ((:img :height "9" :src "/static/arblue.gif" :width "9" :border "0"))
               (:translate '(:fr " Hide " :en " Hide ")))
              ((:a :href "#")
               ((:img :height "9" :src "/static/arblue.gif" :width "9" :border "0"))
               (:translate '(:fr " Login/Register"
                             :en " Login/Register")))
              ((:form :method "post" :action (interface::url interface::*request*))
               :br
               (:table
                (:tr (:td (:translate '(:fr "Identifiant " :en "Login ")))
                     (:td ((:input :type "text" :name "name"))))
                (:tr (:td (:translate '(:fr "Mot de passe " :en "Password ")))
                     (:td ((:input :type "password" :name "password")))
                     (:td ((:input :type "submit" :name "login"
                                   :onclick "fgt('password').value=CryptoJS.SHA1(fgt('password').value); return true;"
                                   :value (meta::translate '(:fr "Identifier" :en "Login"))))))
                (:tr (:td (:translate '(:fr "Confirmer mot de passe " :en "Confirm password ")))
                     (:td ((:input :type "password" :name "password2")))
                     (:td ((:input :type "submit" :name "register"
                                   :onclick "fgt('password').value=CryptoJS.SHA1(fgt('password').value); fgt('password2').value=CryptoJS.SHA1(fgt('password2').value); return true;"
                                   :value (meta::translate '(:fr "Enregister" :en "Register"))))))
                (:tr (:td (:translate '(:fr "Reconnexion automatique" :en "Remember me")))
                     (:td ((:input :type "checkbox" :name "remember"))))))) :br))

(defun authentification-ng-block (status)
  (unless (interface::authentified *session*)
    (case status
      (:failed (html:html ((:p :style "color:red;") "Bad identifier and/or password")))
      (:exists-already (html:html ((:p :style "color:red;") "This identifier already exists")))
      (:passwords-mismatch (html:html ((:p :style "color:red;") "Passwords mismatch")))))
    (html:html
     (:ng-on-off
      ((:a :href "#")
       ((:img :height "9" :src "/static/arblue.gif" :width "9" :border "0"))
       (:translate '(:fr " Hide " :en " Hide ")))

      ((:a :href "#")
       ((:img :height "9" :src "/static/arblue.gif" :width "9" :border "0"))
       (:translate '(:fr " Login/Register"
                     :en " Login/Register")))

      ((:form :method "post" :action (interface::url interface::*request*))
       :br
       (:table
           (:tr (:td (:translate '(:fr "Identifiant " :en "Login ")))
                (:td ((:input :type "text" :name "name"))))
         (:tr (:td (:translate '(:fr "Mot de passe " :en "Password ")))
              (:td ((:input :type "password" :name "password")))
              (:td ((:input :type "submit" :name "login" 
                            :value (meta::translate '(:fr "Identifier" :en "Login"))))))
         (:tr (:td (:translate '(:fr "Confirmer mot de passe " :en "Confirm password ")))
              (:td ((:input :type "password" :name "password2")))
              (:td ((:input :type "submit" :name "register" 
                            :value (meta::translate '(:fr "Enregister" :en "Register"))))))
         (:tr (:td (:translate '(:fr "Reconnexion automatique" :en "Remember me")))
              (:td ((:input :type "checkbox" :name "remember"))))))) :br))

(defun translated-title (page)
  (cond
    ((eq interface::*country-language* :fr) (title page))
    (t (title-en page))))

(defun translated-abrev (page)
  (cond
    ((eq interface::*country-language* :fr) (abrev page))
    (t (abrev-en page))))

(defun process-switch-lang-request (request)
  (let ((new-params nil))
    (loop for (key val . rest) on (interface::session-params request) by 'cddr
	  unless (or (eq key :func)(find key new-params))
	  do (setf new-params (list* key val new-params)))
    (interface::redirect-to (interface::encode-session-url nil new-params)
			    request)))

(interface::add-named-func "slang" 'process-switch-lang-request)

(defparameter *no-page-fr* '())

(defparameter *no-page-en* '())

(defun process-404-request (request)
  (let ((new-params nil))
    (loop for (key val . rest) on (interface::session-params request) by 'cddr
	  unless (or (eq key :page)(find key new-params))
	  do (setf new-params (list* key val new-params)))
    (setf new-params (list* :page "404" new-params))
    (interface::redirect-to (interface::encode-session-url nil new-params)
			    request)))

(interface::add-web-404 "repository.fractalconcept.com" 'process-404-request)

(defun clear-pages ()
  (maphash #'(lambda (n p)(setf (content-func p) nil)) *pages*))

(defun gen-site-map (&optional (page (gethash *web-root-name* *pages*)) (level 0))
  (loop repeat level
    do (write-string "&nbsp;" html:*html-stream*))
  (incf level 5)
  (html:html ((:a :href (encoded-name page))(html:fmt "~a" (if (eq interface::*country-language* :fr)
							     (title page)
							     (title-en page))) :br))
  (let ((sub-pages
	 (loop for sub-page-name in (sub-page-names page)
	       as sub-page = (gethash sub-page-name *pages*)
	       unless sub-page do (error (format nil "page ~s of page ~s not found" sub-page-name (name page)))
	       collect sub-page)))
    (dolist (sub-page sub-pages)
      (gen-site-map sub-page level))))

