(in-package meta-web)

(make-instance 'page-desc
   :name "home"
   :title "Home"
   :content '(:progn
              (:when-frontends '(:bootstrap)
               ((:section)
                ((:div :class "page-header")
                 (:h1 "The framework web interface"))
                (:p "Nothing much to do here except clicking on the "
                    ((:a :href #e"projects") "list of the projects in the database"))))))

(make-instance 'page-desc
               :name "projects"
               :title "Projects"
               :content '(:progn
                          (:when-frontends '(:bootstrap)
                           ((:section)
                            ((:div :class "page-header"))
                            (:h1 "Project List:")
                            (meta-web::html-project-list)))
                          :br))
