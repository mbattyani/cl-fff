(in-package meta-web)

(defun make-translation (tr)
  (list 'make-instance ''meta::translated-string
	:en (english tr) :fr (french tr) :de (german tr) :sp (spanish tr) :it (italian tr)))

(make-instance 'interface::object-view :object-class 'translated-string
	       :country-languages '(:fr) :name "ts" :source-code 
   `(((:tab :class "tabdv")
      ("Français"
       ((:table :class "dvt" :style "width:100%")
	((:tr :class "dvr")
	 ((:td :class "dvcv") ((:slot-edit french :class "dvcve"))))))
      ("Anglais"
       ((:table :class "dvt" :style "width:100%")
	((:tr :class "dvr")
	 ((:td :class "dvcv") ((:slot-edit english :class "dvcve"))))))
      ("Allemand"
       ((:table :class "dvt" :style "width:100%")
	((:tr :class "dvr")
	 ((:td :class "dvcv") ((:slot-edit german :class "dvcve"))))))
      ("Espagnol"
       ((:table :class "dvt" :style "width:100%")
	((:tr :class "dvr")
	 ((:td :class "dvcv") ((:slot-edit spanish :class "dvcve"))))))
      ("Italien"
       ((:table :class "dvt" :style "width:100%")
	((:tr :class "dvr")
	 ((:td :class "dvcv") ((:slot-edit italian :class "dvcve")))))))))


(make-instance 'interface::object-view :object-class 'translated-string
	       :country-languages '(:en) :name "ts-en" :source-code 
   `(((:tab :class "tabdv")
      ("English"
       ((:table :class "dvt" :style "width:100%")
	((:tr :class "dvr")
	 ((:td :class "dvcv") ((:slot-edit english :class "dvcve"))))))
      ("French"
       ((:table :class "dvt" :style "width:100%")
	((:tr :class "dvr")
	 ((:td :class "dvcv") ((:slot-edit french :class "dvcve"))))))
      ("German"
       ((:table :class "dvt" :style "width:100%")
	((:tr :class "dvr")
	 ((:td :class "dvcv") ((:slot-edit german :class "dvcve"))))))
      ("Spanish"
       ((:table :class "dvt" :style "width:100%")
	((:tr :class "dvr")
	 ((:td :class "dvcv") ((:slot-edit spanish :class "dvcve"))))))
      ("Italian"
       ((:table :class "dvt" :style "width:100%")
	((:tr :class "dvr")
	 ((:td :class "dvcv") ((:slot-edit italian :class "dvcve")))))))))

(make-instance 'interface::object-view :object-class 'translated-string
	       :country-languages '(:en) :name "ts-en" ;:frontend '(:bootstrap)
               :source-code 
   `((:tab
      ("English"
       ((:slot-edit english)))
      ("French"
       ((:slot-edit french )))
      ("German"
       ((:slot-edit german )))
      ("Spanish"
       ((:slot-edit spanish)))
      ("Italian"
       ((:slot-edit italian))))))

(defun make-object-help (oh)
  (list 'make-instance ''meta::object-help
	:en (english-tooltip oh) :fr (french-tooltip oh) :de (german-tooltip oh)
	:sp (spanish-tooltip oh) :it (italian-tooltip oh)))

(make-instance 'interface::object-view :object-class 'object-help
	       :country-languages '(:fr :en) :name "oh" :source-code 
   `(((:tab :class "tabdv")
      ("English"
       ((:table :class "dvt" :style "width:400px")
	((:tr :class "dvr")
	 ((:td :class "dvch") "tooltip")
	 ((:td :class "dvcv") ((:slot-edit english-tooltip :class "dvcve"))))
	((:tr :class "dvr")
	 ((:td :class "dvcv" :colspan "2")
	  ((:slot-medit english-help :class "dvcve" :rows "30" :cols "20" :style "width:320px"))))))
      ("French"
       ((:table :class "dvt" :style "width:400px")
	((:tr :class "dvr")
	 ((:td :class "dvch") "tooltip")
	 ((:td :class "dvcv") ((:slot-edit french-tooltip :class "dvcve" :style "width:320px"))))
	((:tr :class "dvr")
	 ((:td :class "dvcv" :colspan "2")
	  ((:slot-medit french-help :class "dvcve" :rows "30" :cols "20" :style "width:320px"))))))
      ("German"
       ((:table :class "dvt" :style "width:400px")
	((:tr :class "dvr")
	 ((:td :class "dvch") "tooltip")
	 ((:td :class "dvcv") ((:slot-edit german-tooltip :class "dvcve"))))
	((:tr :class "dvr")
	 ((:td :class "dvcv" :colspan "2")
	  ((:slot-medit german-help :class "dvcve" :rows "30" :cols "20" :style "width:320px"))))))
      ("Italian"
       ((:table :class "dvt" :style "width:400px")
	((:tr :class "dvr")
	 ((:td :class "dvch") "tooltip")
	 ((:td :class "dvcv") ((:slot-edit italian-tooltip :class "dvcve"))))
	((:tr :class "dvr")
	 ((:td :class "dvcv" :colspan "2")
	  ((:slot-medit italian-help :class "dvcve" :rows "30" :cols "20" :style "width:320px"))))))
      ("Spanish"
       ((:table :class "dvt" :style "width:400px")
	((:tr :class "dvr")
	 ((:td :class "dvch") "tooltip")
	 ((:td :class "dvcv") ((:slot-edit spanish-tooltip :class "dvcve"))))
	((:tr :class "dvr")
	 ((:td :class "dvcv" :colspan "2")
	  ((:slot-medit spanish-help :class "dvcve" :rows "30" :cols "20" :style "width:320px"))))))
      )))

(defmethod meta::short-description ((obj named-object-mixin))
  (if (name obj) (name obj) "(Pas de nom)"))

(defmethod meta::short-description ((obj slot-info))
  (name obj))

(defun make-choice (c)
  `(list ,(read-from-string (choice-value c))
    ,(make-translation (name c))))

(defmethod meta::short-description ((obj choice-value))
    (format nil "~a (~a)" (choice-value obj)(french (name obj))))

(defun get-user-groups (obj)
  (mapcar #'(lambda (x)
	      (list (meta::short-description x) x))
	  (user-groups (project obj))))

(make-instance 'interface::object-view :object-class 'slot-info
	       :country-languages '(:fr) :name "slot-fr" :source-code 
  `(((:tab :class "tabf")
     ("Description"
      (:slot-table name user-name sql-name description comment in-proxy stored accessor initarg initform
		   choices-table choices ))
     ("Valeur"
      (:slot-table value-type object-type other-type list-of-values new-objects-first linked-value
		   nb-decimals unit can-create-new-object create-new-object get-object-fn
		   process-new-object-fn get-value-sql
		   sql-length value-to-sql-func sql-to-value-func))
     ("Vue"
      (:slot-table view-type slot-view-name html-tag-attributes list-format 
		   pathname-filter value-to-string-func string-to-value-func
		   void-link-text dont-display-null-value get-value-html-fn  get-value-title get-value-text 
		   modifiable modifiable-groups can-delete can-delete-groups visible visible-groups))
     ("Règles"
      (:slot-table indexed unique-p null-allowed enable-copy-paste 
		   duplicate-value make-copy-string duplicate-value-fn
		   value-constraint disable-predicate))
     ("Aide"
      (:object-view :object (object-help interface::*object*)))
     )))

(make-instance 'interface::object-view :object-class 'slot-info
	       :country-languages '(:en) :name "slot-en" :source-code 
  `(((:tab :class "tabf")
     ("Description"
      (:slot-table name user-name sql-name description comment in-proxy stored accessor initarg initform
		   choices-table choices ))
     ("Type of value"
      (:slot-table value-type object-type other-type list-of-values new-objects-first linked-value
		   list-format nb-decimals unit can-create-new-object create-new-object get-object-fn
		   process-new-object-fn get-value-sql
		   sql-length value-to-sql-func sql-to-value-func))
     ("Access rights"
      (:slot-table modifiable modifiable-groups can-delete can-delete-groups visible visible-groups))
     ("View"
      (:slot-table view-type slot-view-name html-tag-attributes list-format 
		   pathname-filter value-to-string-func string-to-value-func
		   void-link-text dont-display-null-value get-value-html-fn  get-value-title get-value-text))
     ("Rules"
      (:slot-table null-allowed value-constraint disable-predicate 
		   enable-copy-paste make-copy-string duplicate-value duplicate-value-fn indexed unique-p))
     ("Help"
      (:object-view :object (object-help interface::*object*)))
     )))

(defun safe-read-from-string (str &optional list)
  (let ((*read-eval* nil))
    (ignore-errors (read-from-string (if list (concatenate 'string "( " str " )") str)))))

(defmacro %add-slot-w% (accessor slot &key (keyword (intern (symbol-name accessor) :keyword))
				 translation read quote list)
  (setf *current-slot-attribute* (symbol-name accessor))
  (if translation
    `(when (,accessor ,slot) (list ,keyword (translation (,accessor ,slot))))
    (if read
      (if quote
	`(when (,accessor ,slot) (list ,keyword (list 'quote (safe-read-from-string (,accessor ,slot) ,list))))
	`(when (,accessor ,slot) (list ,keyword (safe-read-from-string (,accessor ,slot) ,list))))
      `(when (,accessor ,slot) (list ,keyword (,accessor ,slot))))))

(defmacro %add-slot% (accessor slot &key (keyword (intern (symbol-name accessor) :keyword))
			       translation read)
  (setf *current-slot-attribute* (symbol-name accessor))
  (if read
    `(list ,keyword (safe-read-from-string (,accessor ,slot)))
    `(list ,keyword (,accessor ,slot))))

(defun get-view-types (object)
  (let* ((type (value-type object))
	 (choices (meta::choices (interface::slot interface::*dispatcher*)))
	 (keywords (cond
		     ((list-of-values object) '(:default :named-slot-view 
						:list :list-val :list2 :list2-val :pick-mval))
		     ((eq type :object) '(:default :named-slot-view :link :embed :embed-val :on-off))
		     ((eq type :string) '(:default :named-slot-view :medit :edit))
		     ((eq type :color) '(:default :named-slot-view :pick-color :edit))
		     (t '(:default :named-slot-view)))))
    (loop for keyword in keywords
	  for position = (position keyword choices :key #'first)
	  for text = (meta::translate (second (assoc keyword choices)))
	  collect (list text position))))

(defparameter *value-types* '(:boolean boolean :string string :date :date :time-of-day :time-of-day 
			      :utime :universal-time :integer integer :decimal :decimal :float float
			      :object :object :other-type :other-type :timestamp :timestamp :symbol symbol
			      :color :color :ip-address :ip-address))

(defun get-value-type (slot-info)
  (case (value-type slot-info)
    (:object (safe-read-from-string (name (object-type slot-info))))
    (:other (safe-read-from-string (other-type slot-info)))
    (t (getf *value-types* (value-type slot-info)))))

(defun make-slot-def (slot-info)
  (setf *current-slot* slot-info)
  (setf *current-slot-attribute* "name")
  `(,(safe-read-from-string (name slot-info))
    ,@(%add-slot-w% accessor slot-info :read t)
    ,@(progn (setf *current-slot-attribute* "value-type") nil)
    :value-type ,(get-value-type slot-info)
    ,@(progn (setf *current-slot-attribute* "user-name") nil)
    :user-name ,(make-translation (user-name slot-info))
    ,@(progn (setf *current-slot-attribute* "initform") nil)
    ,@(if (and (eq (value-type slot-info) :object) (create-new-object slot-info))
	  `(:initform (make-instance ',(safe-read-from-string (name (object-type slot-info)))))
	  (%add-slot-w% initform slot-info :read t))
    ,@(%add-slot-w% description slot-info)
    ,@(progn (setf *current-slot-attribute* "object-help") nil)
    :object-help ,(make-object-help (object-help slot-info))
    ,@(%add-slot-w% sql-name slot-info)
    ,@(when (initarg slot-info) (list :initarg (intern (string-upcase (initarg slot-info)) 'keyword)))
    ,@(progn (setf *current-slot-attribute* "choices") nil)
    :choices ,(cons 'list (mapcar #'make-choice (choices slot-info)))
    ,@(%add-slot% visible slot-info)
    ,@(progn (setf *current-slot-attribute* "visible-groups") nil)
    :visible-groups ',(mapcar #'(lambda (g)
				  (intern (string-upcase (name g)) 'keyword))
			      (visible-groups slot-info))
    ,@(%add-slot% modifiable slot-info)
    ,@(progn (setf *current-slot-attribute* "modifiable-groups") nil)
    :modifiable-groups ',(mapcar #'(lambda (g)
				     (intern (string-upcase (name g)) 'keyword))
				 (modifiable-groups slot-info))
    ,@(%add-slot% stored slot-info)
    ,@(%add-slot% in-proxy slot-info)
    ,@(%add-slot% indexed slot-info)
    ,@(%add-slot% unique slot-info :keyword :unique)
    ,@(%add-slot% null-allowed slot-info)
    ,@(%add-slot% list-of-values slot-info)
    ,@(%add-slot% new-objects-first slot-info)
    ,@(%add-slot% linked-value slot-info)
    ,@(%add-slot% modifiable slot-info)
    ,@(%add-slot-w% unit slot-info)
    ,@(%add-slot% duplicate-value slot-info)
    ,@(%add-slot% make-copy-string slot-info)
    ,@(%add-slot-w% duplicate-value-fn slot-info :read t :quote t)
    ,@(%add-slot-w% get-value-html-fn slot-info :read t :quote t)
    :get-value-title ,(make-translation (get-value-title slot-info))
    :get-value-text ,(make-translation (get-value-text slot-info))
    ,@(%add-slot-w% disable-predicate slot-info :read t :quote t)
    ,@(%add-slot-w% value-constraint slot-info :read t :quote t)
    ,@(%add-slot-w% sql-length slot-info)
    ,@(%add-slot-w% value-to-string-func slot-info :read t :keyword :value-to-string-fn :quote t)
    ,@(%add-slot-w% string-to-value-func slot-info :read t :keyword :string-to-value-fn :quote t)
    ,@(%add-slot-w% value-to-sql-func slot-info :read t :keyword :value-to-sql-fn :quote t)
    ,@(%add-slot-w% sql-to-value-func slot-info :read t :keyword :sql-to-value-fn :quote t)
    ,@(%add-slot-w% nb-decimals slot-info)
    ,@(progn (setf *current-slot-attribute* "void-link-text") nil)
    :void-link-text ,(make-translation (void-link-text slot-info))
    ,@(%add-slot-w% pathname-filter slot-info)
    ,@(%add-slot% can-create-new-object slot-info)
    ,@(%add-slot% create-new-object slot-info)
    ,@(%add-slot-w% get-object-fn slot-info :read t :keyword :get-object-func :quote t)
    ,@(%add-slot-w% process-new-object-fn slot-info :read t :quote t)
    ,@(%add-slot-w% get-value-sql slot-info)
    ,@(%add-slot-w% html-tag-attributes slot-info :read t :quote t :list t)
    ,@(%add-slot% dont-display-null-value slot-info)
    ,@(%add-slot% view-type slot-info)
    ,@(%add-slot-w% slot-view-name slot-info :read t :quote t)
    ,@(%add-slot-w% list-format slot-info)
    ,@(progn (setf *current-slot* nil *current-slot-attribute* nil) nil)
    ))

(defun make-slot-info-from-slot (slot)
  #.(list* 'make-instance ''slot-info :store '*meta-store*
	   :name '(slot-definition-name slot)
	   (loop with key-package = (find-package :keyword)
		 for slot-name in '(user-name description tooltip stored in-proxy indexed
				    unique null-allowed accessor initarg initform choices
				    list-of-values value-type linked-value modifiable visible 
				    unit disable-predicate value-constraint value-to-string-func
				    string-to-value-func sql-length nb-decimals
				    void-link-text pathname-filter can-create-new-object get-object-func
				    dont-display-null-value)
		 as key = (intern (symbol-name slot-name) key-package)
		 nconc `(,key (,slot-name slot)))))

(make-instance 'interface::slot-list-format 
   :name "ot2r" :country-languages '(:fr :en)
   :object-class 'slot-info
   :list-format-fn
   #'(lambda (start objects max-nb total-length)
       (html:html
	 ((:table :class "dvl" :style "border:'0';background-color:#808080;" 
		  :cellpadding "2" :cellspacing "1")
	  (:when objects
	    (:tr
	     ((:td :class "dvcv":style "background-color:#c0c0d0;") "&nbsp;")
	     ((:td :class "dvcv":style "background-color:#c0c0d0;") "Slot name")
	     ((:td :class "dvcv":style "background-color:#c0c0d0;") "List")
	     ((:td :class "dvcv":style "background-color:#c0c0d0;") "Type")
	     ((:td :class "dvcv":style "background-color:#c0c0d0;") "Linked")
             ((:td :class "dvcv":style "background-color:#c0c0d0;") "Stored")
             ((:td :class "dvcv":style "background-color:#c0c0d0;") "Copy")
             ((:td :class "dvcv":style "background-color:#c0c0d0;") "View")
             ((:td :class "dvcv":style "background-color:#c0c0d0;") "Modif")))
	  (loop repeat max-nb
	     for object in objects
	     for index from start
	     for index1 = (1+ index) do
	       (html:html
		 (:tr
		  ((:td :class "dvcv") index1 (interface::std-list-checkbox index start))
		  ((:td :class "dvcv")
		   ((:a :href (interface::encode-object-url object))
		    (html:esc (meta::short-description object))))
		  ((:td :class "dvcv")
		   (:p (:if (list-of-values object) 
			    "list"
			    "&nbsp;&nbsp;&nbsp;&nbsp;")))
		  ((:td :class "dvcv")
		   (:p (html:fmt "~a"
				 (if (and (eq (value-type object) :object)(object-type object))
				     (name (object-type object))
				     (value-type object)))))
		  ((:td :class "dvcv")
		   (:p (:if (linked-value object) "linked" "")))
                  ((:td :class "dvcv")
		   (:p (:if (stored object) "stored" "")))
                  ((:td :class "dvcv")
		   (:p (:if (duplicate-value object) "copy" "")))
                  ((:td :class "dvcv")
		   (:p (:if (visible object)
                            "all"
                            (:format "~{~a~^, ~}" (mapcar 'name (visible-groups object))))))
                  ((:td :class "dvcv")
		   (:p (:if (modifiable object)
                            "all"
                            (:format "~{~a~^, ~}" (mapcar 'name (modifiable-groups object))))
                        )))))))))

