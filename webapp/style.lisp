(in-package #:interface)

(defmethod webapp::gen-breadcrumbs (app page obj &key (home-url "/"))
  (let ((descriptions (localize obj)))
    (case *frontend*
      (:bootstrap
       (html:html
        ((:ul :class "breadcrumb")
         (:li ((:a :href home-url) (:translate '(:fr "Accueil" :en "Home"))))
         (loop for ((description url) . next) on descriptions do
              (html:html (:li ((:a :href url) (:esc description))))))))
      (:html
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
                 (html:html ((:a :href url) (:esc description)) (:when next " &gt "))) :br))))))

(in-package #:webapp)

(defmethod insert-page-title (app page)
  (html:html (:insert-string (name page))))

(defmethod insert-html-meta (app page)
  (html:html
   ((:meta :name "viewport" :content "width=device-width, initial-scale=1.0"))
   ((:meta :name "description" :content "The F3 Web Interface"))
   ((:meta :name "keywords" :content ""))
   ((:meta :http "http" :equiv "content-type" :content "text/html; charset=UTF-8"))
   ((:meta :http "http" :equiv "Content-Style-Type" :content "text/css"))
   ((:meta :http "http" :equiv "imagetoolbar" :content "no"))
   ((:meta :http "http" :equiv "content-language" :content "EN"))
   ((:meta :name "robots" :content "index, follow"))))

(defmethod insert-html-head-links (app page)
  (html:html
     ((:link :rel "shortcut icon" :href "/static/favicon.ico" :type "image/x-icon"))
     ((:link :rel "stylesheet" :href "/static/css/fcweb.css"))
     ((:link :rel "stylesheet" :href "/static/css/modal.css"))
     (:when-frontends '(:bootstrap)
                      ((:link :rel "stylesheet" :href "//netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap.min.css"))
                      ((:link :rel "stylesheet" :href "//netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap-theme.min.css"))
                      ((:link :rel "stylesheet" :href "/static/css/modal.css"))
                      ((:script :src "https://code.jquery.com/jquery.js"))
                      ((:script :src "//netdna.bootstrapcdn.com/bootstrap/3.0.2/js/bootstrap.min.js")))
     ((:script :src "/static/fgt.js"))))

(defmethod write-page (app page)
  (html:html
   (:doctype)
   (:html
    (:head
     (:title (insert-page-title app page))
     (insert-html-meta app page)
     (insert-html-head-links app page))
    (:body
     (:when-frontends '(:bootstrap)
                      ((:div :class "container")
                       (gen-breadcrumbs app page *object*)
                       (funcall (content-func page))))
     (:when-frontends '(:html)
                      ((:img :border "0" :src "/static/made-with-lisp-logo.jpg"))
                      ((:div :style "padding:5px;")
                       (gen-breadpp page*object*)
                       :use-ui-ws
                       (funcall (content-func page))))
     ((:script :src "/static/fractal-ws.js"))))))

(defun process-404-request (request)
  (let ((new-params nil))
    (loop for (key val . rest) on (interface::session-params request) by 'cddr
	  unless (or (eq key :page)(find key new-params))
	  do (setf new-params (list* key val new-params)))
    (setf new-params (list* :page "404" new-params))
    (interface::redirect-to (interface::encode-session-url nil new-params)
			    request)))

(interface::add-web-404 "repository.fractalconcept.com" 'process-404-request)

(defun prev-page-link ()
  (let ((previous (interface::get-previous-page interface::*request*)))
    (html:html
     (:when previous
       ((:a :href previous) (:translate '(:fr "Page précédente" :en "Previous Page")))))))

