(in-package meta-web)

(defun make-fc-function (f)
  `(make-instance 'meta::fc-function
    :name ',(read-from-string (name f))
    :user-name ,(make-translation (user-name f))
    ,@(%add-slot% visible f)
    :visible-groups ',(mapcar #'(lambda (g)
				  (intern (string-upcase (name g)) 'keyword))
			      (visible-groups f))
    ,@(%add-slot-w% get-value-html-fn f :read t :quote t)
    :get-value-title ,(make-translation (get-value-title f))
    :get-value-text ,(make-translation (get-value-text f))
    ,@(%add-slot-w% get-value-sql f)
    ,@(%add-slot-w% html-tag-attributes f :read t :quote t :list t)
    ,@(%add-slot-w% get-object-fn f :read t  :keyword :get-object-func :quote t)
    ,@(%add-slot-w% disable-predicate f :read t :quote t)
    :object-help ,(make-object-help (object-help f))))

(defun get-project-classes (class-info)
  (let* ((project (project class-info)))
    (when project
      (cons (list (name project))
	    (loop for group in (class-groups project)
		  collect (cons (list (name group))
				(loop for class in (sort (copy-list (classes group))	#'string<= :key 'name)
				      collect (list (list (name class) (interface::encode-object-id class))))))))))

(defun get-project-classes (class-info)
  (let* ((project (project class-info)))
    (when project (sort (mapcan #'(lambda (group) (copy-list (classes group)))
				(class-groups project))
			#'string<=
			:key 'name))))

(defun get-project-classes-tree (class-info)
  (let* ((project (project class-info)))
    (when project
      (cons (list (name project))
	    (loop for group in (class-groups project)
		  collect (cons (list (name group))
				(loop for class in (sort (copy-list (classes group))	#'string<= :key 'name)
				      collect (list (list (name class) (interface::encode-object-id class))))))))))


(defmethod initialize-instance :after ((class-info class-info) &rest init-options &key &allow-other-keys)
  )

(make-instance 'interface::object-view :object-class 'class-info
	       :country-languages '(:fr) :name "class-fr" :source-code 
  `(((:tab :class "tabf")
     ("Description"
      (:slot-table name user-name description comment class-status guid version sql-name
		   direct-superclasses
		   instanciable visible visible-groups use-memory-store short-description))
     ("Slots"
      ((:table :class "dvt")
       ((:tr :class "dvr")
	((:td :class "dvch2" :colspan "2")
	 ((:slot-list direct-slots :height "500px" :col-fn interface::std-list-col-fn :class "dvl")
	  (:table (:tr ((:td :class "dvch2") "Slots"))))))))
     ("Vues"
      ((:table :class "dvt")
       ((:tr :class "dvr")
	((:td :class "dvch2" :colspan "2")
	 ((:slot-list direct-views :height "500px" :col-fn interface::std-list-col-fn :class "dvl")
	  (:table (:tr ((:td :class "dvch2") "Vues"))))))))
     ("Fonctions"
      ((:table :class "dvt")
       ((:tr :class "dvr")
	((:td :class "dvch2" :colspan "2")
	 ((:slot-list direct-functions :height "500px" :col-fn interface::std-list-col-fn :class "dvl")
	  (:table (:tr ((:td :class "dvch2") "Fonctions"))))))))
     ("Règles"
      ((:table :class "dvt")
       ((:tr :class "dvr")
	((:td :class "dvch2" :colspan "2")
	 ((:slot-list direct-rules :height "500px" :col-fn interface::std-list-col-fn :class "dvl")
	  (:table (:tr ((:td :class "dvch2") "Règles"))))))))
     ("Versions"
      (:slot-table current-version)
      ((:table :class "dvt")
       ((:tr :class "dvr")
	((:td :class "dvch2" :colspan "2")
	 ((:slot-list previous-versions :height "500px" :col-fn interface::std-list-col-fn :class "dvl")
	  (:table (:tr ((:td :class "dvch2") "Versions"))))))))
     ("Aide"
      (:object-view :object (object-help interface::*object*)))
     ):br
    (:obj-fn-table)
    :br
    ))

(make-instance 'interface::object-view :object-class 'class-info
	       :country-languages '(:en) :name "class-en" :source-code 
  `(((:tab :class "tabf")
     ("Description"
      (:slot-table name user-name description comment class-status guid version sql-name
		   direct-superclasses
		   instanciable visible visible-groups use-memory-store short-description))
     ("Slots"
      ((:table :class "dvt")
       ((:tr :class "dvr")
	((:td :class "dvch2" :colspan "2")
	 ((:slot-list direct-slots :height "500px" :col-fn interface::std-list-col-fn :class "dvl")
	  (:table (:tr ((:td :class "dvch2") "Slots"))))))))
     ("Views"
      ((:table :class "dvt")
       ((:tr :class "dvr")
	((:td :class "dvch2" :colspan "2")
	 ((:slot-list direct-views :height "500px" :col-fn interface::std-list-col-fn :class "dvl")
	  (:table (:tr ((:td :class "dvch2") "Views"))))))))
     ("Functions"
      ((:table :class "dvt")
       ((:tr :class "dvr")
	((:td :class "dvch2" :colspan "2")
	 ((:slot-list direct-functions :height "500px" :col-fn interface::std-list-col-fn :class "dvl")
	  (:table (:tr ((:td :class "dvch2") "Functions"))))))))
     ("Rules"
      ((:table :class "dvt")
       ((:tr :class "dvr")
	((:td :class "dvch2" :colspan "2")
	 ((:slot-list direct-rules :height "500px" :col-fn interface::std-list-col-fn :class "dvl")
	  (:table (:tr ((:td :class "dvch2") "Rules"))))))))
    ("Versions"
      (:slot-table current-version)
      ((:table :class "dvt")
       ((:tr :class "dvr")
	((:td :class "dvch2" :colspan "2")
	 ((:slot-list previous-versions :height "500px" :col-fn interface::std-list-col-fn :class "dvl")
	  (:table (:tr ((:td :class "dvch2") "Versions"))))))))
     ("Help"
      (:object-view :object (object-help interface::*object*)))
     ):br
    (:obj-fn-table)
    :br
    ))

(defun defclass-info (name supers slot-specifiers options)
  `(make-instance 'class-info :name ,name :direct-superclasses ,supers
	 :direct-slots ,(mapcar 'defslot-info slot-specifiers)
	 ,@options))

(defun make-defclass (class-info)
  (setf *current-class* class-info)
  (let ((*package* (ensure-package (project-package (project class-info))))
	(class-name (read-from-string (name class-info))))
    (list* 'prog1
	   (list 'defclass class-name
		 (mapcar #'(lambda (c) (read-from-string (name c))) (direct-superclasses class-info))
		 (mapcar 'make-slot-def (direct-slots class-info))
		 `(:user-name ,(make-translation (user-name class-info))
		   :guid ,(read-from-string (guid class-info))
		   :object-help ,(make-object-help (object-help class-info))
		   :functions ,(cons 'list (mapcar 'make-fc-function (direct-functions class-info)))
		   ,@(%add-slot% visible class-info)
		   :visible-groups ',(mapcar #'(lambda (g)
						 (intern (string-upcase (name g)) 'keyword))
					     (visible-groups class-info))
		   ,@(%add-slot% instanciable class-info)
		   ,@(%add-slot-w% short-description class-info :read t :quote t)
		   ,@(progn (setf *current-class* nil) nil)))
	   (mapcar #'(lambda (v) (make-view v class-name)) (direct-views class-info)))))

(defun find-meta-class (class-id)
  (let ((class (gethash class-id *loaded-classes-by-id*)))
    (unless class
      (let ((class-info (load-named-object *meta-store* (format nil "class ~x" class-id))))
	(setf class (eval (make-defclass class-info)))))
    class))

(defun make-class-info-from-class (class)
  #.(list* 'make-instance ''class-info :store '*meta-store*
	   :name '(class-name class)
	   :slots '(mapcar 'make-slot-info-from-slot (class-direct-slots class))
	   :direct-superclasses '(class-direct-superclasses class)
	   (loop with key-package = (find-package :keyword)
		   for slot-name in '(guid user-name sql-name description version visible instanciable
				      anonymous short-description direct-views direct-functions direct-rules)
		   as key = (intern (symbol-name slot-name) key-package)
		   nconc `(,key (,slot-name class)))))

(defun compile-class (class-info)
  (let ((*package* (ensure-package (project-package (project class-info))))
	(*current-class* nil)
	(*current-slot* nil)
	(*current-slot-attribute* nil))
    (eval (make-defclass class-info))))

(defun create-temp-object (class-info)
  (let ((*package* (ensure-package (project-package (project class-info)))))
    (compile-class class-info)
    (let ((obj (make-instance (read-from-string (name class-info))
			      :store meta::*memory-store*)))
      (interface::send-open-new-win-to-interface (interface::encode-object-url obj)))))

(defun display-class-graph (projet classes depth)
  (let* ((file-id (interface::make-session-id))
	 (dotfile (concatenate 'string *graph-file-prefix* file-id ".dot"))
	 (psfile  (concatenate 'string *graph-file-prefix* file-id ".ps"))
	 (pdffile (concatenate 'string *graph-file-prefix* file-id ".pdf")))
      (gen-sub-graph projet classes depth dotfile)
      (sys:call-system
       (format nil
	       #+win32 "D:\\Program Files\\ATT\\Graphviz\\bin\\dot.exe -Tps2 ~s -o ~s"
	       #+linux "dot -Tps2 ~s -o ~s"
	       dotfile psfile) :wait t)
      (sys:call-system (format nil "ps2pdf.cmd ~s ~s" psfile pdffile) :wait t)
      (interface::send-open-new-win-to-interface (format nil "/~a.pdf" file-id))))

(defun display-local-class-graph (class-info)
  (display-class-graph (project class-info) (list class-info) 2))

(defun print-class-source (stream class-info)
  (pprint (make-defclass class-info) stream)
  (format stream "~%"))

(defun view-class-source (class-info)
  (let* ((*package* (ensure-package (project-package (project class-info))))
	 (*print-right-margin* 2000)
	 (file-id (interface::make-session-id))
	 (src-file (concatenate 'string *graph-file-prefix* file-id ".lisp")))
    (with-open-file (s src-file :direction :output :if-exists :supersede)
      (format s "(in-package ~s)~%" (package-name *package*))
      (print-class-source s class-info)
      (format s "~%~%" (package-name *package*)))
    (interface::send-open-new-win-to-interface (format nil "/~a.lisp" file-id))))

(defun create-object-in-db (class-info)
  (when (create-project-classes (project class-info))
    (let* ((*package* (ensure-package (project-package (project class-info))))
	   (obj (make-instance (read-from-string (name class-info))
			       :store (store (project class-info)))))
      (interface::send-open-new-win-to-interface (interface::encode-object-url obj)))))

(defun list-objects-in-db (class-info)
  (when (create-project-classes (project class-info))
    (let* ((*package* (ensure-package (project-package (project class-info))))
	   (obj (make-instance (read-from-string (name class-info))
			       :store (store (project class-info)))))
      (interface::send-open-new-win-to-interface (interface::encode-object-url obj)))))

(defun create-class-tables (class-info)
  (let* ((project (project class-info))
	 (*package* (ensure-package (project-package project)))
	 (class-name (read-from-string (name class-info))))
    (compile-class class-info)
    (create-table class-name (store project))))

(defun drop-class-tables (class-info)
  (let* ((project (project class-info))
	 (*package* (ensure-package (project-package project)))
	 (class-name (read-from-string (name class-info))))
  (drop-table class-name (store project))))

(defun dds20 (f p1 p2 n)
  (let ((ratio 0))
    (loop for i below n
      for o1 = (logtest p1 #x80000)
      for o2 = (logtest p2 #x80000)
      do
      (setf p1 (logand (+ p1 f) #xfffff))
      (setf p2 (logand (+ p2 f) #xfffff))
      (unless (eq o1 o2)(incf ratio)))
    (values (float (/ ratio n)) ratio)))

    