(in-package meta-web)

(interface::add-named-url "/index.html"
  #'(lambda (request)
      (interface::redirect-to (interface::encode-session-url
			       nil (list :page "home" 
					 :session (interface::id (make-instance 'interface::session))))
			      request)
      t))

(setf (gethash "/" interface::*url-params-aliases*) (list :page "home"))
(setf (gethash "/index.html" interface::*url-params-aliases*)
      (list :page "home"))

(interface::add-named-url "/debug" #'(lambda (request) nil))

(make-instance 'page-desc
   :name "home"
   :restricted nil
   :title "Accueil"
   :title-en "Home"
   :abrev "Accueil"
   :abrev-en "Home"
   :sub-pages '("object" "404" "hot" "top" "new")
   :content-en '(:progn
                  (:when-frontends '(:bootstrap)
                   ((:section)
                    ((:div :class "page-header")
                     (:h1 "The framework web interface"))
                    (:p "nothing much to do except clicking on this link: "
                   ((:a :href #e"projects") "List of the projects in the database"))))
                 ))

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
   :title "Accueil"
   :title-en "Home"
   :abrev "Accueil"
   :abrev-en "Home"
   :sub-pages '("object" "404" "hot" "top" "new")
   :content '((logout-page-fn))
   :content-en '((logout-page-fn)))

(make-instance 'page-desc
               :name "404"
               :hidden t
               :title "Accueil des pages non trouvées"
               :title-en "Unknown Pages Home"
               :abrev "Page inconnue"
               :abrev-en "Unknown Page"
               :sub-pages '()
               :content '((:h1 "Page inconnue")
                          (:p "La page que vous demandez est inconnue.")
                          (:p "Vous pouvez essayer de trouver l'information que vous cherchez à partir de la "
                           ((:a :href (encode-page "home")) "page d'accueil")))
               :content-en '((:h1 "Page not found")
                             (:p "The page you requested does not exists.")
                             (:p "You can try to find what you are looking for from the "
                              ((:a :href (encode-page "home")) "home page"))))

(make-instance 'page-desc
  :name "object"
  :restricted nil
  :title "Objet"
  :title-en "Object"
  :abrev "Objet"
  :abrev-en "Object"
  :sub-pages 'nil
  :content `(;:use-ui
             (:object-view)
             :connect-views :br)
  :content-en `(;:use-ui
                (:object-view)
                :connect-views
                :br))

(defun profile-page-fn ()
  (html:html
   (:object-view :object *user* :name "user-p")
   :connect-views))

(make-instance 'page-desc
   :name "profile"
   :restricted nil
   :title "My Profile"
   :title-en "My Profile"
   :abrev "My Profile"
   :abrev-en "My Profile"
   :sub-pages '("object" "404")
   :content '((profile-page-fn))
   :content-en '((profile-page-fn)))

(defun admin-page-fn ()
  (html:html
   (:object-view :object *sys-params* :name "sys-a")
   :connect-views))

(make-instance 'page-desc
   :name "admin"
   :restricted nil
   :title "Administration"
   :title-en "Administration"
   :abrev "Administration"
   :abrev-en "Administration"
   :sub-pages '("object" "404")
   :content '((admin-page-fn))
   :content-en '((admin-page-fn)))

(defun config-page-fn ()
  (html:html
   (:object-view :object *sys-params* :name "sys-conf")
   :connect-views))

(make-instance 'page-desc
   :name "config"
   :restricted nil
   :title "Configuration"
   :title-en "Configuration"
   :abrev "Configuration"
   :abrev-en "Configuration"
   :sub-pages '("object" "404")
   :content '((config-page-fn))
   :content-en '((config-page-fn)))


(defun status-page-fn ()
  (html:html
    (:object-view :object *sys-params* :name "sys-status")
   :connect-views))

(make-instance 'page-desc
   :name "status"
   :restricted nil
   :title "Status"
   :title-en "Status"
   :abrev "Status"
   :abrev-en "Status"
   :sub-pages '("object" "404")
   :content '((status-page-fn))
   :content-en '((status-page-fn)))

(defun map-page-fn ()
  (html:html
   (:h1 "Ivan Site Map")
    ))

(make-instance 'page-desc
   :name "map"
   :restricted nil
   :title "Directory Tag Map"
   :title-en "Directory Tag Map"
   :abrev "Directory Tag Map"
   :abrev-en "Directory Tag Map"
   :sub-pages '()
   :content '((map-page-fn))
   :content-en '((map-page-fn)))

(make-instance 'page-desc
               :name "object"
               :restricted t
               :title "Objet"
               :title-en "Object"
               :abrev "Objet"
               :abrev-en "Object"
               :sub-pages 'nil
               :restricted t
               :hidden t
               :content `(:use-ui
                          ,@meta-web::*default-object-page-fr*
                          :connect-views                          )
               :content-en `(:use-ui
                             ,@meta-web::*default-object-page-en*
                             :connect-views                    ))

(make-instance 'page-desc
               :name "projects"
               :title "Projets"
               :title-en "Projects"
               :abrev "Projets"
               :abrev-en "Projects"
               :sub-pages '("object")
               :restricted t
               :content '((:h1 "Meta-Tool: Le générateur d'application web")
                          (:h2 "Liste des projects en cours:")
                          (meta-web::html-project-list)
                          :br :br                          )
               :content-en '(:progn
                             (:when-frontends '(:bootstrap)
                              ((:section)
                               ((:div :class "page-header")
                                (:h1 "Meta-Tool: The web application generator"))
                               (:h2 "Project List:")
                               (meta-web::html-project-list)))
                             :br :br))

(make-instance 'page-desc
               :name "user"
               :title "Utilisateur"
               :title-en "User"
               :abrev "Utilisateur"
               :abrev-en "User"
               :sub-pages '()
               :restricted nil
               :hidden t
               :content '((:h1 "Meta-Tool: Utilisateur")
                          (meta-web::html-user-page)
                          :br :br
                          )
               :content-en '((:h1 "Meta-Tool: User")
                             (meta-web::html-user-page)
                             :br :br))

(make-instance 'page-desc
               :name "inspect"
               :restricted t
               :title "Inspecteur d'objet"
               :title-en "Object Inspector"
               :abrev "Inspecteur d'objet"
               :abrev-en "Object Inspector"
               :sub-pages 'nil
               :content `(:use-ui
                          ,@meta-web::*inspect-object-page-fr*
                          :connect-views
                          )
               :content-en `(:use-ui
                             ,@meta-web::*inspect-object-page-en*
                             :connect-views
                             ))
