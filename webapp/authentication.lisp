(in-package webapp)

(defvar *authenticated* ())
(defvar *auth-status* ())
(defvar *auth-name* ())

(defmethod write-page :around (app page)
  (multiple-value-bind (*auth-status* *auth-name*) (check-authentification app page)
    (let ((*authenticated* (eq *auth-status* t)))
      (call-next-method))))

;; from https://github.com/turtl/api/blob/master/lib/crypto.lisp
(defun sha256 (sequence/string)
  "Return a *string* sha256 hash of the given string/byte sequence."
  (string-downcase
    (ironclad:byte-array-to-hex-string
      (ironclad:digest-sequence (ironclad:make-digest 'ironclad:sha256)
                                (if (stringp sequence/string)
                                    (babel:string-to-octets sequence/string)
                                    sequence/string)))))

(defmethod find-user-by-user-name (app user-name)
  )

(defmethod find-user-by-cookie (app cookie)
  )

(defmethod link-user-cookie (user cookie)
  )

(defmethod create-new-user (app user-name)
  )

(defmethod hash-password (app user user-name password)
  (sha256 (concatenate 'string "webapp" password user-name password)))

(defmethod password (user)
  )

(defmethod  (setf password) (password user)
  )

(defmethod  auto-login (user)
  )

(defmethod  send-new-password-email (app user password)
  )

(defmethod  (setf auto-login) (value user)
  )

(defmethod  (setf last-access) (time user)
  )

(defmethod  valid-user-name-p (app user-name)
  t)

(defmethod check-authentification (app page)
  (interface::decode-posted-content interface::*request*)
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
           (forgot-pswd (assoc "forgot-pswd" posted-content :test 'string=))
           (remember (assoc "remember" posted-content :test 'string=))
           (sign-in (assoc "sign-in" posted-content :test 'string=))
           (register (assoc "register" posted-content :test 'string=))
           (authorized nil))
      (setf %debug% (list name password forgot-pswd remember sign-in register))
      (if (and name (or sign-in register))
          (let ((user (find-user-by-user-name app name)))
            (cond
              (forgot-pswd
               (if user
                   (let ((password (interface::make-session-id)))
                     (setf (password user) (hash-password app user name password))
                     (send-new-password-email app user password)
                     (return-from check-authentification (values :forgot-pswd name)))
                   (return-from check-authentification (values :unknown name))))
              (sign-in
                (if (and user (string= (hash-password app user name password) (password user)))
                    (progn
                      (switch-user user)
                      (setf (interface::authentified *session*) t)
                      (when remember
                        (setf (auto-login user) t))
                      (return-from check-authentification t))
                    (return-from check-authentification (values :failed name))))
              (register
                (unless (valid-user-name-p app name)
                  (return-from check-authentification (values :invalid-user-name name)))
                (if user
                    (return-from check-authentification (values :exists-already name))
                    (let ((user (create-new-user app name)))
                      (switch-user user)
                      (setf (password user) (hash-password app user name password))
                      (setf (interface::authentified *session*) t)
                      (link-user-cookie user (interface::cookie *session*))
                      (when remember
                        (setf (auto-login user) t))
                      (return-from check-authentification t))))))
          (when (interface::cookie *session*)
            (let ((user (find-user-by-cookie app (interface::cookie *session*))))
              (when (and user (auto-login user))
                (switch-user user)
                (setf (interface::authentified *session*) t)
                t)))))))

(defun bs-alert (text &optional (style :danger))
  (setf style (getf '(:success "alert alert-success alert-dismissable"
                      :info "alert alert-info alert-dismissable"
                      :warning "alert alert-warning alert-dismissable"
                      :danger "alert alert-danger alert-dismissable")
                    style "alert alert-danger alert-dismissable"))
  (html:html
   ((:div :class style)
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
         (case *auth-status*
           (:forgot-pswd (webapp::bs-alert "A new password has been sent to that email address" :info))
           (:unknown (webapp::bs-alert "Sorry, that user email is not registered"))
           (:failed (bs-alert "Sorry, unknown email or password"))
           (:invalid-user-name (bs-alert "Sorry, this user name is invalid"))
           (:exists-already (bs-alert "Sorry, this user name exists already")))
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
                     :name "password" :id "password"))
            ((:label :class "checkbox")
             ((:input :type "checkbox" :name "forgot-pswd")) "Forgotten password")
            #+nil((:label :class "checkbox") ((:input :type "checkbox" :name "remember")) " Remember me")
            ((:button :class "btn btn-lg btn-primary btn-block" :type "submit" :name "sign-in"
                      :onclick "fgt('password').value=CryptoJS.SHA1(fgt('password').value); return true;")
             "Sign in")))
          ((:div :class "tab-pane" :id "register")
           (:jscript "function disable_register () {
   if ($('#passwordr').val() == $('#passwordr2').val()) {
       $('#register-btn').prop('disabled', false);
       $('#pswd-div').removeClass('has-error');
     }
   else {
      $('#register-btn').prop('disabled', true);
      $('#pswd-div').addClass('has-error');
     }
};")
           ((:form :class "form-signin" :role "form" :method "post" :action (interface::url interface::*request*))
            ((:input :type "email" :class "form-control" :placeholder "Your email @edhec.com" :required "required"
                     :name "name" :autofocus "autofocus" :value *auth-name*))
            ((:input :type "password" :class "form-control" :placeholder "Password"
                     :required "required" :id "passwordr" :name "password" :oninput "disable_register();"))
            ((:div :id "pswd-div")
             ((:input :type "password" :class "form-control" :placeholder "Confirm Password" :id "passwordr2"
                      :oninput "disable_register();")))
            #+nil ((:label :class "checkbox") ((:input :type "checkbox" :name "remember")) " Remember me ")
            ((:button :class "btn btn-lg btn-primary btn-block" :type "submit" :id "register-btn"
                       :name "register" :disabled "true") "Register")))))
        ((:div :class "modal-footer")
         ((:button :type "button" :class "btn btn-default" :data-dismiss "modal") "Close"))))))
   (:when (find *auth-status* '(:failed :exists-already :invalid-user-name :unknown :forgot-pswd))
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
