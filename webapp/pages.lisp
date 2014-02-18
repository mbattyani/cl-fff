(in-package webapp)

(defclass page-desc ()
  ((name :accessor name :initform "" :initarg :name)
   (restricted :accessor restricted :initform nil :initarg :restricted)
   (allowed-groups :accessor allowed-groups :initform t :initarg :allowed-groups)
   (hidden :accessor hidden :initform nil :initarg :hidden)
   (title :accessor title :initform "" :initarg :title)
   (content :accessor content :initform nil :initarg :content)
   (content-func :accessor content-func :initform nil :initarg :content-func)))

(defmethod initialize-instance :after ((page page-desc) &rest init-options &key &allow-other-keys)
  (declare (ignore init-options))
  (setf (gethash (name page) *pages*) page)
  (interface::add-url-alias (concatenate 'string "/" (name page))(list :page (name page)))
  (interface::add-named-page (name page) #'(lambda (request)
                                             (process-page-request *app* page request))))

(defun get-page (page-name)
  (gethash page-name *pages*))

(defmethod process-page-request (app *page* request)
  (interface::with-output-to-request (request html:*html-stream*)
    (write-page app *page*)))

(defmethod content-func :around ((page page-desc))
  (let ((func (call-next-method)))
    (if func
	func
	(setf (content-func page)
              (let ((*package* (symbol-package (class-name (class-of *app*)))))
                (compile nil `(lambda ()
                                (when (or (not (restricted ,page))
                                          (interface::authentified interface::*session*)
                                          (check-authentification ))
                                  ,@(mapcar 'html::html-gen (content page))))))))))

(defun clear-pages ()
  (maphash #'(lambda (n p)(setf (content-func p) nil)) *pages*))

(interface::add-named-url "/index.html"
  #'(lambda (request)
      (interface::redirect-to (interface::encode-session-url
			       nil (list :page "home"
					 :session (interface::id (make-instance 'interface::session))))
			      request)
      t))

(interface::add-url-alias "/" '(:page "home"))
(interface::add-url-alias "/index.html" '(:page "home"))

(interface::add-named-url "/debug" #'(lambda (request) nil))

(make-instance 'page-desc
   :name "home"
   :restricted nil
   :title "Home"
   :content '(:progn
              (:when (is-bootstrap *frontend*)
               ((:section)
                ((:div :class "page-header")
                 (:h1 "The app home"))
                ))))

(defun logout-page-fn()
  (setf *user* nil)
  (ensure-user)
  (interface::redirect-to
   (interface::encode-session-url
    nil (list :session (interface::id *session*)
              :lang interface::*country-language-id*
              :page "home"))
   *request*))

(make-instance 'page-desc
   :name "logout"
   :restricted nil
   :title "logout"
   :content '((logout-page-fn)))

(make-instance 'page-desc
   :name "unknown"
   :hidden t
   :title "Unknown Page"
   :content '((:h1 "Page not found")
              (:p "The page you requested does not exists.")
              (:p "You can try to find what you are looking for from the "
               ((:a :href (encode-page "home")) "home page"))))

(make-instance 'page-desc
  :name "object"
  :restricted nil
  :title "Object"
  :content `((:object-view)
             :connect-views))

(defun profile-page-fn ()
  (html:html
   (:object-view :object *user* :name "user-p")
   :connect-views))

(make-instance 'page-desc
   :name "my-profile"
   :restricted nil
   :title "My Profile"
   :content '((profile-page-fn)))

(defun admin-page-fn ()
  (html:html
   (:object-view :object *app* :name "sys-a")
   :connect-views))

(make-instance 'page-desc
   :name "admin"
   :restricted t
   :allowed-groups '(:admin :dev)
   :title "Administration"
   :content '((admin-page-fn)))

(defun status-page-fn ()
  (html:html
    (:object-view :object *app* :name "sys-status")
   :connect-views))

(make-instance 'page-desc
   :name "status"
   :restricted t
   :allowed-groups '(:admin :dev)
   :title "Status"
   :content '((status-page-fn)))
