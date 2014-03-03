(in-package #:interface)

(defmethod webapp::gen-breadcrumbs (app page obj &key (home-url "/"))
  (let ((descriptions (localize obj)))
    (cond
      ((is-bootstrap *frontend*)
       (html:html
        ((:ul :class "breadcrumb")
         (:li ((:a :href home-url) (:translate '(:fr "Accueil" :en "Home"))))
         (loop for ((description url) . next) on descriptions do
              (html:html (:li ((:a :href url) (:esc description))))))))
      (t
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
     ((:link :rel "shortcut icon" :href "/static/favicon.png" :type "image/png"))
     (:when (is-bootstrap *frontend*)
       ((:link :rel "stylesheet" :href "/static/bootstrap/bootstrap.min.css"))
       ((:link :rel "stylesheet" :href "/static/bootstrap/bootstrap-theme.min.css"))
       ((:link :rel "stylesheet" :href "/static/libs/bs-datetimepicker/bootstrap-datetimepicker.min.css"))
       ((:link :rel "stylesheet" :href "/static/css/fcweb-bs.css"))
       ((:script :src "https://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"))
;       (:jscript "window.jQuery || document.write('<script src=\"/static/jquery/jquery.js\"></script>')")
       ((:script :src "/static/bootstrap/bootstrap.min.js"))
       ((:script :src "/static/libs/moment.js/moment.min.js"))
       ((:script :src "/static/libs/bs-datetimepicker/bootstrap-datetimepicker.min.js")))
     ((:script :src "/static/fractal.js"))))

(defmethod insert-global-modal (app page)
  (html:html
   ((:div :class "modal fade" :id "GlobalModal" :tabindex "-1" :role "dialog"
            :aria-labelledby "GlobalModal" :aria-hidden "true")
    ((:div :class "modal-dialog")
     ((:div :class "modal-content")
      ((:div :class "modal-header")
       ((:h4 :class "modal-title")) "Title")
      ((:div :class "modal-body") "Body")
      ((:div :class "modal-footer")
       ((:button :type "button" :class "btn btn-default" :data-dismiss "modal") "Close")))))))

(defmethod insert-page-header (app page)
  )

(defmethod insert-page-footer (app page)
  )

(defmethod write-page (app page)
  (html:html
   (:doctype)
   (:html
    (:head
     (:title (insert-page-title app page))
     (insert-html-meta app page)
     (insert-html-head-links app page))
    (:body
     (insert-global-modal app page)
     (insert-page-header app page)
     (:when (is-bootstrap *frontend*)
       ((:div :class "container")
        (gen-breadcrumbs app page *object*)
        (funcall (content-func page)))
       ((:modal-window :id "global_modal")
        (:body
         ((:iframe :width "250px" :height "280px" :id "global_iframe" :name "global_iframe")))))
     (:when (is-html *frontend* t)
       ((:img :border "0" :src "/static/made-with-lisp-logo.jpg"))
       ((:div :style "padding:5px;")
        (gen-breadcrumbs app page *object*)
        :use-ui-ws
        (funcall (content-func page))))
     (insert-page-footer app page)))))

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
