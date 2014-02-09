(in-package webapp)

(defvar *authenticated* ())
(defvar *auth-status* ())
(defvar *auth-name* ())

(defmethod write-page :around (app page)
  (multiple-value-bind (*auth-status* *auth-name*) (check-authentification app page)
    (let ((*authenticated* (eq *auth-status* t)))
      (call-next-method))))

(defmethod find-user-by-user-name (app user-name)
  )

(defmethod find-user-by-cookie (app cookie)
  )

(defmethod link-user-cookie (user cookie)
  )

(defmethod create-new-user (app user-name)
  )

(defmethod password (user)
  )

(defmethod  (setf password) (password user)
  )

(defmethod  (setf auto-login) (value user)
  )

(defmethod  (setf last-access) (time user)
  )

(defmethod check-authentification (app page)
  (interface::decode-posted-content interface::*request*)
  (setf %c% (interface::posted-content interface::*request*))
  (let* ((posted-content (interface::posted-content interface::*request*)))
    (when (not posted-content)
      (ensure-user app)
      (return-from check-authentification (interface::authentified *session*)))
    (when (interface::authentified *session*)
      (when (find "logout" posted-content :key #'first :test 'string=)
        (setf (interface::authentified *session*) nil)
        (switch-user nil))
      (return-from check-authentification (interface::authentified *session*)))
    (let* ((name (cdr (assoc "name" posted-content :test 'string=)))
           (password (cdr (assoc "password" posted-content :test 'string=)))
           (remember (assoc "remember" posted-content :test 'string=))
           (sign-in (assoc "sign-in" posted-content :test 'string=))
           (register (assoc "register" posted-content :test 'string=))
           (authorized nil))
      (if (and name (or sign-in register))
          (let ((user (find-user-by-user-name *app* name)))
            (when sign-in
              (if (and user (string= password (password user)))
                  (progn
                    (switch-user user)
                    (setf (interface::authentified *session*) t)
                    (when remember
                      (setf (auto-login user) t))
                    (return-from check-authentification t))
                  (return-from check-authentification (values :failed name))))
            (when register
              (if user
                  (return-from check-authentification (values :exists-already name))
                  (let ((user (create-new-user *app* name)))
                    (switch-user user)
                    (setf (password user) password)
                    (setf (interface::authentified *session*) t)
                    (link-user-cookie user (interface::cookie *session*))
                    (when remember
                      (setf (auto-login user) t))
                    (return-from check-authentification t)))))
          (when (interface::cookie *session*)
            (let ((user (find-user-by-cookie *app* (interface::cookie *session*))))
              (when (and user (auto-login user))
                (switch-user user)
                (setf (interface::authentified *session*) t)
                t)))))))

(defun bs-alert (text)
  (html:html
   ((:div :class "alert alert-danger alert-dismissable")
    ((:button :type "button" :class "close" :data-dismiss "alert" :aria-hidden "true") "&times;")
    (:insert-string text))))

(defmethod insert-login-dialog (app page)
  (html:html
   (:when (not *authenticated*)
     ((:div :class "modal fade" :id "SignInModal" :tabindex "-1" :role "dialog"
            :aria-labelledby "SignInModal" :aria-hidden "true")
      ((:div :class "modal-dialog")
       ((:div :class "modal-content")
        ((:div :class "modal-body")
         ((:script :src "/static/sha1.js"))
         (case *auth-status*
           (:failed (bs-alert "Sorry, unknown email or password"))
           (:exists-already (bs-alert "Sorry, this email exists already")))
         ((:h2 :class "form-signin-heading") "Please sign in or register")
         ((:ul :class "nav nav-tabs")
          ((:li :class "active") ((:a :href "#sign-in" :data-toggle "tab") "Sign in"))
          (:li ((:a :href "#register" :data-toggle "tab") "Register")))
         ((:div :class "tab-content")
          ((:div :class "tab-pane active" :id "sign-in")
           ((:form :class "form-signin" :role "form" :method "post" :action (interface::url interface::*request*))
            ((:input :type "text" :class "form-control" :placeholder "Your user name" :required "required"
                     :name "name" :autofocus "autofocus" :value *auth-name*))
            ((:input :type "password" :class "form-control" :placeholder "Password"
                     :required "required" :name "password" :id "password"))
            ((:label :class "checkbox")
             ((:input :type "checkbox" :name "remember")) " Remember me")
            ((:button :class "btn btn-lg btn-primary btn-block" :type "submit" :name "sign-in"
                      :onclick "fgt('password').value=CryptoJS.SHA1(fgt('password').value); return true;")
             "Sign in")))
          ((:div :class "tab-pane" :id "register")
           ((:form :class "form-signin" :role "form" :method "post" :action (interface::url interface::*request*))
            ((:input :type "text" :class "form-control" :placeholder "Your user name" :required "required"
                     :name "name" :autofocus "autofocus" :value *auth-name*))
            ((:input :type "password" :class "form-control" :placeholder "Password"
                     :required "required" :id "passwordr" :name "password"))
            ((:input :type "password" :class "form-control" :placeholder "Confirm Password" :id "passwordr2"))
            ((:label :class "checkbox")
             ((:input :type "checkbox" :name "remember")) " Remember me ")
            ((:button :class "btn btn-lg btn-primary btn-block" :type "submit" :name "register"
                      :onclick "fgt('passwordr').value=CryptoJS.SHA1(fgt('passwordr').value); return true;")
             "Register")))))
        ((:div :class "modal-footer")
         ((:button :type "button" :class "btn btn-default" :data-dismiss "modal") "Close"))))))
   (:when (or (eq *auth-status* :failed) (eq *auth-status* :exists-already))
     (:script "$('#SignInModal').modal();"))))

(defmethod insert-log-inout-button (app page)
  (if *authenticated*
      (html:html ((:form :class "navbar-form navbar-right" :role "form" :method "post"
                         :action (interface::url interface::*request*))
                  ((:button :class "btn" :type "submit" :name "logout") "Log out")))
      (html:html ((:form :class "navbar-form navbar-right")
                  ((:a :class "btn btn-success" :data-toggle "modal" :data-target "#SignInModal") "Sign in")))))

(defun switch-user (user)
  (unless user
    (interface::clear-url-history *session*))
  (setf *user* user
        (interface::user *session*) *user*)
  (if user
    (setf (last-access user) (get-universal-time)
          *user-groups* (interface::groups *user*))
    (ensure-user *app*)))

(defmethod ensure-user (app)
  (unless *user*
    (switch-user (make-instance 'safari-user :store meta::*memory-store*))))
