(in-package meta-web)

(make-instance 'interface::object-view :object-class 'view-info
	       :country-languages '(:fr) :name "vi-fr" :source-code 
   `(((:tab :class "tabf")
      ("Description"
	(:slot-table name user-name description comment special-view country-languages groups))
      ("Source"
       ((:table :class "dvt" :style "width:400px")
	((:tr :class "dvr")
	 ((:td :class "dvcv" :colspan "2")
	  ((:slot-medit source-code :class "dvcve" :rows "30" :cols "20" :style "width:100%"))))))
      )))

(make-instance 'interface::object-view :object-class 'view-info
	       :country-languages '(:en) :name "vi-en" :source-code 
   `(((:tab :class "tabf")
      ("Description"
	(:slot-table name user-name description comment special-view country-languages groups))
      ("Source"
       ((:table :class "dvt" :style "width:400px")
	((:tr :class "dvr")
	 ((:td :class "dvcv" :colspan "2")
	  ((:slot-medit source-code :class "dvcve" :rows "30" :cols "20" :style "width:100%"))))))
      )))

(defun make-view (view-info class-name)
  (list 'make-instance ''interface::object-view
	:object-class (list 'quote class-name)
	:special-view (special-view view-info)
	:country-languages (list 'quote (country-languages view-info))
	:name (name view-info)
	:source-code (read-from-string (concatenate 'string "`(" (source-code view-info) ")"))
	:user-groups (list 'quote (mapcar #'(lambda (g)
					      (intern (string-upcase (name g)) 'keyword))
					  (groups view-info)))))

#+ignore
`(((:table :class "dvt" :style "width:400px")
   ((:tr)
    ((:td )((:slot-edit name :style "width:100px")))
    ((:td )((:slot-edit title :style "width:150px")))
    ((:td )"CA "((:slot-edit ca :style "width:60px")):br
     (:translate '(:fr "Entités " :en "Entities ")) :br
     ((:slot-pick-mval entities)))
    ((:td )"Min "((:slot-edit min-debit  :style "width:50px")):br
     "Max "((:slot-edit max-debit :style "width:50px")))
    ((:td )"SG/SG"((:slot-check-box sg-sg)):br "SG/Non&nbspSG"((:slot-check-box sg-no-sg)))
    ((:td )"SG"((:slot-check-box sg-paid)):br "Non&nbsp;SG"((:slot-check-box no-sg-paid))))))
