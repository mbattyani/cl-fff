(in-package interface)

(defun gen-localize-html (obj &key home-url)
  (let ((descriptions (localize obj)))
    (html:html 
     ((:table :border "0")
               (:tr ((:td :colspan "20") 
                     ((:a :href home-url) (:translate '(:fr "Accueil" :en "Home")))))
               (loop for (description url) in descriptions 
                     for i from 1 do
                     (html:html
                      (:tr (loop repeat i do (html:html ((:td :width "15px") " ")))
                           ((:td :colspan (format nil "~d" (- 10 i)))
                                 ((:a :href url) (:esc description)))))))
     :br)))

