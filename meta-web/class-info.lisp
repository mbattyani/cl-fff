(in-package meta-web)

;;; Let's put the translation objects into the parent object file

(defmethod meta::convert-slot-value-to-sexpr ((tr translated-string))
  (list :reader :trans-str :en (english tr) :fr (french tr) :sp (spanish tr)))

(defun read-translated-string (list)
  (apply 'make-instance 'translated-string :parent meta::*parent-object* :store meta::*memory-store* (cddr list)))

(meta::add-ascii-store-slot-reader :trans-str 'read-translated-string)

(defmethod meta::mark-object-as-modified ((object translated-string))
  (meta::mark-object-as-modified (meta::parent object)))

(defmethod meta::save-object-to-store ((store meta::ascii-store) (object translated-string))
  )

(defmethod meta::convert-slot-value-to-sexpr ((obj object-help))
  (list :reader :obj-help :en (english-tooltip obj) :fr (french-tooltip obj) :sp (spanish-tooltip obj)))

(defun read-object-help (list)
  (apply 'make-instance 'object-help :parent meta::*parent-object* :store meta::*memory-store* (cddr list)))

(meta::add-ascii-store-slot-reader :obj-help 'read-object-help)

(defmethod meta::mark-object-as-modified ((object object-help))
  (meta::mark-object-as-modified (meta::parent object)))

(defmethod meta::save-object-to-store ((store meta::ascii-store) (object object-help))
  )

(eval-when (:execute :load-toplevel :compile-toplevel)
  (setf *meta-slot-list* (c2mop:class-slots (find-class 'meta::fc-function))))

(defun make-fc-function (f)
  `(make-instance 'meta::fc-function
                  :name ',(read-from-string (name f))
                  ,@(make-translation :user-name (user-name f))
                  ,@(%add-slot% visible f)
                  ,@(%add-slot-groups% visible-groups f)
                  ,@(%add-slot-w% get-value-html-fn f :read t :quote t)
                  ,@(make-translation :get-value-title (get-value-title f))
                  ,@(make-translation :get-value-text (get-value-text f))
                  ,@(%add-slot-w% get-value-sql f)
                  ,@(%add-slot-w% html-tag-attributes f :read t :quote t :list t)
                  ,@(%add-slot-w% get-object-fn f :read t  :keyword :get-object-func :quote t)
;                  ,@(%add-slot-w% process-new-object-fn f :read t  :keyword :process-object-func :quote t)
                  ,@(%add-slot-w% disable-predicate f :read t :quote t)
                  ,@(make-object-help :object-help (object-help f))))

(make-instance 'interface::object-view :object-class 'function-info
	       :country-languages '(:en) :name "class-en" :source-code
  `(((:tab :class "tabf")
     ("Description"
      (:slot-table name user-name description comment
		   visible visible-groups html-tag-attributes disable-predicate))
     ("Choice Dialog"
      (:slot-table get-object-fn #+nil process-new-object-fn get-value-html-fn get-value-title get-value-text get-value-sql))
     ("Help"
      (:object-view :object (object-help interface::*object*))))))

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
		   other-superclasses direct-superclasses
		   instanciable visible visible-groups use-memory-store short-description))
     ("Slots"
      ((:table :class "dvt")
       ((:tr :class "dvr")
	((:td :class "dvch2" :colspan "2")
	 ((:slot-list direct-slots :height "500px" :class "dvl")
	  (:table (:tr ((:td :class "dvch2") "Slots"))))))))
     ("Vues"
      ((:table :class "dvt")
       ((:tr :class "dvr")
	((:td :class "dvch2" :colspan "2")
	 ((:slot-list direct-views :height "500px" :class "dvl")
	  (:table (:tr ((:td :class "dvch2") "Vues"))))))))
     ("Fonctions"
      ((:table :class "dvt")
       ((:tr :class "dvr")
	((:td :class "dvch2" :colspan "2")
	 ((:slot-list direct-functions :height "500px" :class "dvl")
	  (:table (:tr ((:td :class "dvch2") "Fonctions"))))))))
     ("Versions"
      (:slot-table current-version)
      ((:table :class "dvt")
       ((:tr :class "dvr")
	((:td :class "dvch2" :colspan "2")
	 ((:slot-list previous-versions :height "500px" :class "dvl")
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
		   other-superclasses direct-superclasses
		   instanciable visible visible-groups use-memory-store short-description))
     ("Slots"
      ((:table :class "dvt")
       ((:tr :class "dvr")
	((:td :class "dvch2" :colspan "2")
	 ((:slot-list direct-slots :height "500px" :class "dvl")
	  (:table (:tr ((:td :class "dvch2") "Slots"))))))))
     ("Views"
      ((:table :class "dvt")
       ((:tr :class "dvr")
	((:td :class "dvch2" :colspan "2")
	 ((:slot-list direct-views :height "500px" :class "dvl")
	  (:table (:tr ((:td :class "dvch2") "Views"))))))))
     ("Functions"
      ((:table :class "dvt")
       ((:tr :class "dvr")
	((:td :class "dvch2" :colspan "2")
	 ((:slot-list direct-functions :height "500px" :class "dvl")
	  (:table (:tr ((:td :class "dvch2") "Functions"))))))))
    ("Versions"
      (:slot-table current-version)
      ((:table :class "dvt")
       ((:tr :class "dvr")
	((:td :class "dvch2" :colspan "2")
	 ((:slot-list previous-versions :height "500px" :class "dvl")
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

(eval-when (:execute :load-toplevel :compile-toplevel)
  (setf *meta-slot-list* (c2mop:class-slots (find-class 'meta::fc-class))))

(defun make-defclass (class-info)
  (setf *current-class* class-info)
  (let ((*package* (ensure-package (project-package (project class-info))))
	(class-name (read-from-string (name class-info))))
    (list* 'prog1
	   (list 'defclass class-name
		 (append (read-from-string (concatenate 'string "(" (other-superclasses class-info) ")"))
                         (mapcar #'(lambda (c) (read-from-string (name c))) (direct-superclasses class-info)))
		 (mapcar 'make-slot-def (direct-slots class-info))
		 `(,@(make-translation :user-name (user-name class-info))
                   :guid ,(guid class-info)
		   ,@(make-object-help :object-help (object-help class-info))
		   ,@(when (direct-functions class-info)
                           `(:functions ,(cons 'list (mapcar 'make-fc-function (direct-functions class-info)))))
		   ,@(%add-slot% visible class-info)
                   ,@(%add-slot-groups% visible-groups class-info)
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
	       #+win32 "dot.exe -Tps2 ~s -o ~s"
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
    (with-open-file (s src-file :direction :output :if-exists :supersede :external-format :utf-8)
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

