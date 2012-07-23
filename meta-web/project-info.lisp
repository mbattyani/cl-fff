(in-package meta-web)

(defvar *class-info-to-class* (make-hash-table))

(defun get-source-files (source-file)
  (sort (files (meta::parent source-file)) #'string<= :key #'name))

(defmethod project ((obj project))
  obj)

(defmethod project (obj)
  (when (meta::parent obj)
    (project (meta::parent obj))))

(defmethod project ((obj class-info))
  (project (meta::parent obj)))

(defmethod project ((obj class-group))
  (project (meta::parent obj)))

(defmethod meta::short-description ((obj project))
    (if (plusp (length (name obj))) (name obj) "(no name)"))

(interface::add-named-url "/asp/new-project.html"
  #'(lambda (request)
      (let* ((project (make-instance 'project :store *meta-store*)))
	(interface::redirect-to (interface::encode-object-url project) request))
      t))

(defun project-list()
  (sort (mapcar #'(lambda (x)
                    (meta::load-object (first x) meta-web::*meta-store*))
                (sql:query "select id from project"))
        #'string< :key #'meta-web::name))

(defun html-project-list()
  (html:html
   (loop for project in (project-list)
         do (html:html ((:a :href (interface::encode-object-url project))
                        (:esc (meta::short-description project))) ": "
                        (:i (:insert-string (description project))) :br))
   :br ((:a :href "/asp/new-project.html") "New projet")))

(defparameter *meta-web-classes*
  '(slot-info class-info project choice-value user-group translated-string sql-list
    view-info rule-info function-info object-help class-group
    ))

(defun create-table (class &optional (store *meta-store*))
  (meta::create-class-table store class)
#+ignore
(handler-case (meta::create-class-table store class)
    (condition (c)
      (princ c))))

(defun create-tables (&optional (store *meta-store*))
  (loop for class in *meta-web-classes*
	do (create-table class store)))

(defun drop-table (class &optional (store *meta-store*))
  (meta::drop-class-table store class))

(defun drop-tables (&optional (store *meta-store*))
  (loop for class in *meta-web-classes*
	do (drop-table class store)))

(defun map-class-objects (class function &key (store *meta-store*) (where-clause ""))
  (when (symbolp class)(setf class (find-class class)))
  #+nil
  (sql:with-database (nil nil :pool (meta::db-pool store))
    (sql:do-query ((object-id)
		     (format nil "SELECT id FROM ~a ~a" (meta::sql-name class) where-clause)
		     :types :auto)
      (let ((object (meta::load-object object-id store)))
	(when object
	  (funcall function object))))))

(defun create-project-classes (proj)
  (let ((*package* (ensure-package (project-package proj)))
	(*current-class* nil)
	(*current-slot* nil)
	(*current-slot-attribute* nil)
	(classes ())
	(result nil))
;    (handler-case
       (progn
         (dolist (group (class-groups proj))
	   (loop for class-info in (classes group)
		 for class = (eval (make-defclass class-info))
		 do
		 (setf (gethash class-info *class-info-to-class*) class)
		 (push class classes)))
	 (setf result t))
#+ignore      (condition (c)
         (interface::send-message-to-interface
           (format nil "Error in class ~s, slot ~s, attribut ~s\\nError Text : ~a"
		   (if *current-class* (name *current-class*) "no class")
		   (if *current-slot* (name *current-slot*) "no slot")
		   (if *current-slot-attribute* *current-slot-attribute* "no attribute")
		   c)));)
    (when result classes)))

(defun gen-graph (project filename &key (class-list nil class-list-supplied))
  (with-open-file (s filename :direction :output :if-exists :supersede)
    (format s "digraph G {
size=\"8,11\";
node [shape=box,fontname=Helvetica,fontsize=12];
edge [shape=box,fontname=Helvetica,fontsize=12];
rankdir=LR;
")
    (let ((classes (get-project-classes project))
	  (old-classes ()))
      (loop for class in classes do
	    (unless (or (member class old-classes)
			(and class-list-supplied (not (member class class-list))))
	      (let ((members-by-value-class (make-hash-table)))
		(push class old-classes)
		(format s "~s [shape=box,URL=~s];~%" (name class)
			(interface::encode-object-url class :absolute t))
		(loop for super in (direct-superclasses class) do
		      (unless (and class-list-supplied (not (member super class-list)))
			(format s "~s -> ~s [color=blue,labelfontcolor=blue,label=\"subclass\"];~%"
				(name super) (name class))))
		(loop for slot in (direct-slots class) 
		      for object-type = (object-type slot) do
		      (when (and (eq (value-type slot) :object)
				 (not (and class-list-supplied (not (member object-type class-list)))))
			(push slot (gethash object-type members-by-value-class))))
		(maphash #'(lambda(val-class slots)
			     (format s "~s -> ~s [label=\"~{~a~^\\n~}\"];~%"
				     (name class)(name val-class)(mapcar 'name slots)))
			 members-by-value-class)))))
    (format s "}~%")))

(defun gen-sub-graph (project classes depth filename)
  (let ((all-classes (get-project-classes project))
	(new-classes classes)
	(next-new-classes ()))
    (loop while new-classes
          repeat depth do
	  (loop for class in all-classes
		unless (member class classes) do
		(loop for super in (direct-superclasses class) do
		      (when (member super new-classes)
			(push class next-new-classes)
			(push class classes)))
		(loop for slot in (direct-slots class)
		      for object-type = (object-type slot) do
		      (when (and (eq (value-type slot) :object)
				  (member object-type new-classes))
			(push class next-new-classes)
			(push class classes))))
	  (loop for class in new-classes do
		(loop for super in (direct-superclasses class) do
		      (unless (member super classes)
			(push super next-new-classes)
			(push super classes)))
		(loop for slot in (direct-slots class)
		      for object-type = (object-type slot) do
		      (when (and (eq (value-type slot) :object)
				 (not (member object-type classes)))
			(push object-type next-new-classes)
			(push object-type classes))))
	  (setf new-classes next-new-classes
		next-new-classes ())))
  (gen-graph project filename :class-list classes))

(defun class-group-graph (class-group)
  (display-class-graph (project class-group) (classes class-group) 1))

(defun project-class-graph (project)
  (display-class-graph project (get-project-classes project) 0))

(defun make-identifier ()
  (let ((id (make-string 9)))
    (loop for i from 0 below 9
	  do (setf (aref id i) (code-char (+ 48 (random 10)))))
    id))

#+nil
(defun html-user-page()
  (unless (interface::user interface::*session*)
    (interface::decode-posted-content interface::*request*)
    (let* ((posted-content (interface::posted-content interface::*request*))
	   (name (cdr (assoc "name" posted-content :test 'string=)))
	   (password (cdr (assoc "password" posted-content :test 'string=))))
      (when (and name password)
	#-no-psql(sql:with-database (nil nil :pool *database-pool*)
	  (let* ((id (caar (sql:query (format nil "select id from meta_user where identifier='~a'" name)
					:types :auto)))
		 (user (and id (meta::load-object id *meta-store*))))
	    (when (and user (string= password (password user)))
	      (setf (interface::authentified interface::*session*) t)
	      (setf (interface::user interface::*session*) user)
	      ))))))
    (if (interface::user interface::*session*)
      (interface::redirect-to (interface::encode-object-url (interface::user interface::*session*))
			      interface::*request*)
       (html:html
	((:form :method "post" :action (interface::url interface::*request*))
	 (:h1 "Vous devez vous authentifier pour accéder à cette page.")
	 (:p "Nom  " ((:input :type "text" :name "name")) :br
	     "code " ((:input :type "password" :name "password")))
	 ((:input :type "submit" :name "submit" :value "Envoyer"))))))

(defun ensure-package (package)
  (let* ((name (safe-read-from-string package))
	 (found-package (find-package name)))
    (if found-package
	found-package
	(let ((package (make-package name)))
	  (import '(interface::*object*) package)
	  package))))

(defun view-all-classes-source (proj)
  (let* ((*package* (ensure-package (project-package proj)))
	 (*print-right-margin* 2000)
	 (file-id (interface::make-session-id))
	 (src-file (concatenate 'string *graph-file-prefix* file-id ".lisp")))
    (with-open-file (s src-file :direction :output :if-exists :supersede)
      (format s "(in-package ~s)~%" (package-name *package*))
      (dolist (group (class-groups proj))
	(dolist (class (classes group))
	  (print-class-source s class)))
      (format s "~%" (package-name *package*)))
    #+nil(interface::send-open-new-win-to-interface (format nil "/~a.lisp" file-id))))

(defun view-group-classes-source (group &optional pathname)
  (let* ((*package* (ensure-package (project-package (project group))))
	 (*print-right-margin* 2000)
	 (file-id (interface::make-session-id))
	 (src-file (or pathname (concatenate 'string *graph-file-prefix* file-id ".lisp"))))
    (with-open-file (s src-file :direction :output :if-exists :supersede)
      (format s "(in-package ~s)~%" (package-name *package*))
      (format s "
(defvar *~a-classes-list* '~a)

(defun create-~a-classes (store)
  (dolist (class *~a-classes-list*)
     (meta::create-class-table store (find-class class))))

"
	      (name group)
	      (mapcar 'name (remove-if-not 'instanciable (classes group)))
	      (name group)
              (name group))
      (dolist (class (classes group))
	(print-class-source s class))
      (format s "~%"))
      (unless pathname
        (interface::send-open-new-win-to-interface (format nil "/~a.lisp" file-id)))))

(defun make-update-file (proj)
  (make-update-project-fn proj nil (concatenate 'string	(sources-directory proj) "upgrade-database.lisp")))
    
(defmethod gen-lisp-files ((group class-group) &optional (make-update t))
  (view-group-classes-source
   group
   (concatenate 'string
		(or (and (> (length (sources-directory group)) 0)
			 (sources-directory group))
		    (sources-directory (meta::parent group)))
		(name group) "-classes.lisp"))
  (when make-update
    (make-update-file (meta::parent group))))

(defmethod gen-lisp-files ((proj project) &optional (make-update t))
  (map nil #'(lambda (g) (gen-lisp-files g nil))(class-groups proj))
  (when make-update
    (make-update-file proj)))

(defun get-project-values-tables (obj)
  (values-tables (project obj)))

(defun open-database (proj)
  #+nil(setf (db-connection proj)
	(clsql:find-or-create-connection-pool
	 (list (database-ip proj) (database proj) "lisp" "") :postgresql))
  (setf (store proj) (make-instance 'meta::psql-store :db-pool (db-connection proj))))

(defun create-database (proj)
  (meta::create-psql-database *database-pool* (database proj))
  (open-database proj)
  (meta::initialize-store (store proj))
  (dolist (class (create-project-classes proj))
    (when (meta::instanciable class)
      (create-table class (store proj)))))

(defun drop-database (proj)
  #+nil
  (when (db-connection proj)
    (clsql:disconnect-pooled (db-connection proj))
    (setf (db-connection proj) nil))
  (meta::drop-psql-database *database-pool* (database proj)))

(defun map-all-classes (proj fn)
  (dolist (group (class-groups proj))
    (dolist (class (classes group))
      (funcall fn class))))

(make-instance 'interface::object-view :object-class 'project
	       :country-languages '(:en :fr) :name "proj-v" :source-code 
  `((:table
     (:tr
      (:td ((:tab :class "tabf")
	    ("Description"
	     (:slot-table name project-package description version project-version version-date
			  sources-directory application-ip application-port class-groups))
	    ("User&nbsp;Groups"
	     ((:table :class "dvt")
	      ((:tr :class "dvr")
	       ((:td :class "dvch2" :colspan "2")
		((:slot-list user-groups :height "500px" :class "dvl")
		 (:table (:tr ((:td :class "dvch2") "User groups"))))))))
	    ("Files"
	     (:slot-table print-source-files used-lisp-modules)
	     ((:table :class "dvt")
	      ((:tr :class "dvr")
	       ((:td :class "dvch2" :colspan "2") "ASDF directives"))
	      ((:tr :class "dvr")
	       ((:td :class "dvcv" :colspan "2")
		((:slot-medit asdf-directives :class "dvcve" :rows "4" :cols "100" :style "width:440px"))))
	      ((:tr :class "dvr")
	       ((:td :class "dvch2" :colspan "2")
		((:slot-list files :height "400px" :class "dvl")
		 (:table (:tr ((:td :class "dvch2") "Source files"))))))
	      ((:tr :class "dvr")
	       ((:td :class "dvch2" :colspan "2")
		((:slot-list other-documents :height "200px" :class "dvl")
		 (:table (:tr ((:td :class "dvch2") "Other Documents"))))))))
	    ("SQL&nbsp;Lists"
	     ((:table :class "dvt")
	      ((:tr :class "dvr")
		   ((:td :class "dvch2" :colspan "2")
		    ((:slot-list sql-lists :height "500px" :class "dvl")
		     (:table (:tr ((:td :class "dvch2") "SQL lists"))))))))))
      (:td (:obj-fn-table))))))

(defun gen-asdf-file (project)
  (with-open-file (s (format nil "~a~a.asd1"
			     (sources-directory project)
			     (string-downcase (name project)))
		     :direction :output :if-exists :supersede)
    (format s ";;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package asdf) 

(defsystem :~a
    :name ~s
    :description ~s
    :long-description ~s
    :components ("
    (string-downcase (name project))
    (name project)
    (description project)
    (description project))
    (dolist (file (files project))
      (when (in-asdf file)
	(format s "~%                 (:file ~s"
		(pathname-name (pathname (name file))))
	(when (dependances file)
	  (format s "~%                        :depends-on ~s"
		  (mapcar #'(lambda (f) (pathname-name (pathname (name f))))(dependances file))))
	(format s ")")))
    (format s ")~%")
    (write-string (asdf-directives project) s)
    (format s "    :depends-on (~a))~%" (used-lisp-modules project))))
