(in-package interface)

(defun gen-localize-html (obj &key home-url)
  (let ((descriptions (localize obj)))
    (html:html 
     (:on-off ((:a :href "#")
               (:translate '(:fr " Masquer les liens de navigation" :en " Hide navigation links")))
              ((:a :href "#")
               (:translate '(:fr " Afficher les liens de navigation" :en " Show the navigation links")) :br)
              ((:table :border "0")
               (:tr ((:td :colspan "20") 
                     ((:a :href home-url) (:translate '(:fr "Accueil" :en "Home")))))
               (loop for (description url) in descriptions 
                     for i from 1 do
                     (html:html
                      (:tr (loop repeat i do (html:html ((:td :width "15px") " ")))
                           ((:td :colspan (format nil "~d" (- 10 i)))
                                 ((:a :href url) (:esc description)))))))
              :br))))

(defmethod localize (obj)
  (when obj
    (append (localize (meta::parent obj)) 
            (list (list (localize-description obj)
                        (interface::encode-object-url obj))))))

(defmethod localize-description (obj)
  (meta::short-description obj))

