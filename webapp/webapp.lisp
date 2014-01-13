(in-package #:webapp)

(defclass identified-user ()
  ((clipboard :accessor clipboard :initform (make-instance 'interface::clipboard :store meta::*memory-store*))))

(defmethod interface::clipboard ((user identified-user))
  (clipboard user))

(defvar %unique-user% nil)

(defun ensure-user ()
  (unless *user*
    (unless %unique-user%
      (setf %unique-user% (make-instance 'identified-user)))
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
