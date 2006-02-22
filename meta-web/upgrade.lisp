(in-package meta-web)

(defvar *project-version* -1)
(defvar *from-project-version* -1)

(defun make-new-class-version (class-info)
  (unless (= *project-version* (version class-info))
    (let ((old-class (meta::duplicate-object class-info :parent class-info)))
      (setf (name old-class) (format nil "~a version ~d" (name class-info)(version class-info)))
      (setf (version class-info) *project-version*)
      (setf (current-version class-info) t)
      (setf (current-version old-class) nil)
      (push old-class (previous-versions class-info)))))

(defun find-current-class-version (class-info)
  (if (or (current-version class-info)
          (not (typep (meta::parent class-info) 'class-info)))
      class-info
      (find-current-class-version (meta::parent class-info))))

(defun find-class-version (class-info version)
  (if (= version (version class-info))
      class-info
      (let ((current-version (find-current-class-version class-info)))
	(if (= (version current-version) version)
	    current-version
	    (find version (previous-versions current-version) :key #'version)))))

(defvar *effective-stored-slots* nil)

(defun effective-stored-slots (class-info)
  (let ((*effective-stored-slots* nil))
    (effective-stored-slots% class-info (version class-info))
    *effective-stored-slots*))

(defun effective-stored-slots% (class-info version)
  (map nil #'(lambda (class)
	       (effective-stored-slots% (find-class-version class version) version))
       (direct-superclasses class-info))
  (dolist (slot (direct-slots class-info))
    (when (stored slot)
      (pushnew slot *effective-stored-slots*))))

(defun get-sql-name (obj)
  (or (and (string/= (sql-name obj) "")(sql-name obj))
      (meta::convert-name-to-sql-name (name obj))))

(defun make-update-class-fn (base-class-info &optional stream)
  (let ((class-info nil))
    (loop for class in (previous-versions base-class-info)
	  do (when (= *from-project-version* (version class))
	       (setf class-info class)(return)))
    (when (and class-info (instanciable class-info))
      (let* ((obj (unless stream (make-instance 'update-sql-table-function :store meta::*memory-store*)))
	     (parent-class base-class-info)
	     (*package* (find-package "META-WEB"));(ensure-package (project-package (project class-info))))
	     (class-name (read-from-string (name parent-class))))
	(if (current-version class-info)
	    (unless stream (setf (description obj) "it is already the latest version of the class"))
	    (let* ((current-slots (effective-stored-slots parent-class))
		   (old-slots (effective-stored-slots class-info))
		   (version (version class-info))
		   (fn-name (read-from-string (format nil "update-class-~a-~d-to-~d"
						      (name parent-class)(version class-info)
						      (version parent-class))))
		   (added-slots ())
		   (modified-slots ())
		   (removed-slots ())
		   source)
	      (loop for slot in current-slots
		    for old-slot = (find (name slot) old-slots :test 'string-equal :key 'name)
		    do
		    (if old-slot
			(when (or (not (eq (value-type slot)(value-type old-slot)))
				  (not (eq (list-of-values slot)(list-of-values old-slot))))
			  (push slot modified-slots))
			(push slot added-slots)))
	      (dolist (slot old-slots)
		(unless (find (name slot) old-slots :test 'string-equal :key 'name)
		  (push slot removed-slots)))
	      (unless stream
		(setf (description obj)
		      (format nil "convertion of the sql table for the ~s class from version ~d to ~d"
			      (name class-info)(version class-info)(version parent-class)))
		(setf (project obj) (project parent-class)))
	      (setf source
		    (and (or added-slots removed-slots modified-slots)
		    `((let* ((*package* (find-package ,(package-name *package*)))
			     (class (find-class ',class-name)))
			(when (eq pass :modify-tables)
                          (format t "Modifying tables for ~a~%" ',class-name)
			  ,@(loop for slot in added-slots
				  collect `(meta::add-slot-to-class-table store class
					    ',(read-from-string (name slot))))
			  ,@(loop for slot in removed-slots
				  collect `(meta::remove-slot-from-class-table store class
					    ,(get-sql-name slot)
					    ,(list-of-values slot)))
			  ,@(loop for slot in modified-slots
				  collect `(meta::rename-slot-in-class-table store class
					    ,(get-sql-name slot)
					    ,(format nil "~a_~d"
						     (get-sql-name slot)
						     version)
					    ,(list-of-values slot)))
			  ,@(loop for slot in modified-slots
				  collect `(meta::add-slot-to-class-table store class
					    ',(read-from-string (name slot)))))
			(when (eq pass :modify-data)
                          (format t "Modifying data for ~a~%" ',class-name)
			  (clsql:with-database (nil nil :pool *database-pool*)
			    (clsql:do-query ((object-id
					      ,@(mapcar #'(lambda(x) (read-from-string (name x))) modified-slots))
					     ,(format nil "SELECT id ~{,~a~} FROM ~a"
						      (mapcar #'(lambda(x)
								  (format nil "~a_~d" (get-sql-name x) version))
							      modified-slots)
						      (get-sql-name parent-class))
					     :types :auto)
			    (let ((object (meta::load-object object-id store)))
			      (when object
				(let ((data-object (meta::load-object-data object)))
				  ,@(loop for slot in added-slots
					  if (in-proxy slot)
					  collect `(slot-makunbound obj ',(read-from-string (name slot)))
					  else
					  collect `(slot-makunbound data-object ',(read-from-string (name slot)))
					  )
				  ,@(loop for slot in modified-slots
					  collect `(setf (,(read-from-string (name slot)) object)
						    (funcall ',(meta::default-sql-to-value-fn (get-value-type slot) nil)
						     ,(read-from-string (name slot)))))
				  (meta::initialize-unbound-slots object)
				  (meta::silent-mark-object-as-modified object))))))))
		      (meta::save-modified-objects store))))
	      (when source
		(if stream
		    (progn
		      (pprint (list* 'defun fn-name '(store pass) source) stream)
		      (format stream "~%~%"))
		    (progn
		      (setf (lambda-form obj) (compile nil (list* 'lambda '(store) source)))
		      (setf (fn-source obj)
			    (with-output-to-string (s)
			      (format s "(in-package ~s)~%" (package-name *package*))
			      (pprint (list* 'defun fn-name '(store pass) source) s)
			      (format s "~%~%")))
		      (interface::send-open-new-win-to-interface (interface::encode-object-url obj)))))
	      (and (or added-slots removed-slots modified-slots) fn-name)))))))


(defun perform-class-table-update (update-fn)
  (funcall (lambda-form update-fn) (store (project update-fn))))

(defun make-new-project-version  (project)
  (setf *save-database* nil)
  (incf (version project))
  (let ((*project-version* (version project)))
    (map-all-classes project 'make-new-class-version))
  (setf *save-database* t))


(defun make-update-project-fn (proj &optional from-version pathname)
  (let* ((*package* (ensure-package (project-package proj)))
	 (*print-right-margin* 150)
	 (*project-version* (version proj))
	 (*from-project-version* (or from-version (1- *project-version*)))
	 (fn-names ())
	 (classes ())
	 (file-id (interface::make-session-id))
	 (src-file (or pathname (concatenate 'string *graph-file-prefix* file-id ".lisp"))))
    (with-open-file (s src-file :direction :output :if-exists :supersede)
      (format s "(in-package ~s)~%" (package-name *package*))
      (map-all-classes proj #'(lambda (class)
				(if (and (find-class-version class *from-project-version*)
					 (= (mismatch (name class)(name (first (previous-versions class))))
					    (length (name class))))
				    (let ((fn-name (make-update-class-fn class s)))
				      (when fn-name (push fn-name fn-names)))
				    (push class classes))))
      (format s "~%(defun convert-base-from-version-~d-to-~d (store)
  (dolist (class '~a)
     (meta::create-class-table store (find-class class)))"
	      *from-project-version*
	      *project-version*
	      (mapcar 'name (remove-if-not 'instanciable classes)))
      (format s "~%~{(~a store :modify-tables)~%~}~%" fn-names)
      (format s "~%~{(~a store :modify-data)~%~})~%" fn-names))
    #+nil(interface::send-open-new-win-to-interface (format nil "/~a.lisp" file-id))))
