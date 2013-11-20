(in-package #:meta)

(export 'psql-store)
(defclass psql-store (store psql-database)
  ((broken-objects :initform nil :accessor broken-objects)))

(defmethod initialize-instance :after ((store psql-store) &rest init-options &key &allow-other-keys)
;  (hcl:set-hash-table-weak (loaded-objects store) :value)
  (open-store store))

(defmethod open-store ((store psql-store))
  (with-psql-store-db (store)
    ; just try to connect to the database
    ))

(defmethod initialize-store ((store psql-store))
  (with-psql-store-db (store)
    (sql-execute-command
     "create table ObjectNames (name text primary key, objectid BIGINT NOT NULL) WITHOUT OIDS;")
    (sql-execute-command "CREATE SEQUENCE object_id_seq START 1;")))

(defmethod create-new-object-id ((store psql-store) class-id)
  (with-psql-store-db (store)
    (dpb class-id (byte 32 32) (caar (sql-query "select nextval('object_id_seq');")))))

(defmethod read-object-proxy-from-store ((store psql-store) id)
  (let ((found (gethash id (loaded-objects store))))
    (if found
        found
        (let ((*default-store* store))
          (create-proxy-object id (ldb (byte 32 32) id))))))

(defmethod save-object-to-store ((store psql-store) object)
  (when (modified object)
    (with-psql-store-db (store)
      (psql-with-transaction
	(let ((new-original-lists ())
	      (sql-write-object (gen-sql-write-object object)))
	  (when sql-write-object
	    (sql-execute-command sql-write-object))
	  (loop with data-object = (data-object object)
		with class = (class-of object)
		with class-name = (sql-name class)
		for slot in (class-slots class)
		do
		(when (and (stored slot)(list-of-values slot))
		  (let* ((slot-name (sql-name slot))
			 (new-list (slot-value data-object (slot-definition-name slot)))
			 (old-list (cdr (find slot (original-lists object) :key #'first))))
		    (unless (equal new-list old-list)
		      (sql-execute-command
		       (format nil "DELETE FROM ~a_~a WHERE parentid = ~d"
			       class-name slot-name (id object)))
		      (if (subtypep (value-type slot) 'root-object)
			  (loop for linked-obj in new-list for order from 1 do
				(sql-execute-command
				 (format nil "INSERT INTO ~a_~a VALUES (~d, ~d, ~d)"
					 class-name slot-name (id object)(id linked-obj) order)))
			  (loop with value-to-sql-fn = (value-to-sql-fn slot)
				for value in new-list for order from 1 do
				(sql-execute-command
				 (format nil "INSERT INTO ~a_~a VALUES (~d, ~d, ~d)"
					 class-name slot-name (id object)
					 (with-output-to-string (s)(funcall value-to-sql-fn value s))
					 order)))))
		    (push (cons slot (copy-list new-list)) new-original-lists))))
	  (setf (original-lists object) new-original-lists
		(modified object) nil
		(new-object object) nil))))))


(defmethod read-object-from-store ((store psql-store) id)
  (let ((found (gethash id (loaded-objects store))))
    (if found
      found
      (let ((object (read-object-proxy-from-store store id)))
	(when object
	  (load-object-data object)
	  object)))))

(defmethod load-named-object ((store psql-store) name)
  (with-psql-store-db (store)
    (let ((id (caar (sql-query
		     (format nil "select objectid from ObjectNames where name = '~a'" name)))))
      (when id
	(load-object id store)))))

(defmethod register-named-object ((store psql-store) object name)
  (with-psql-store-db (store)
    (sql-execute-command (format nil "insert into ObjectNames (name, objectid) values ('~a', ~d)"
				   name (id object)))))

(defmethod load-or-create-named-object ((store psql-store) name class-id)
  (let ((object (load-named-object store name)))
    (unless object
      (setf object (make-instance (find-meta-class class-id) :store store))
      (register-named-object store object name))
    object))

(defmethod delete-object-from-store ((store psql-store) object)
  (with-psql-store-db (store)
    (sql-execute-command (format nil "delete from ~a where id = ~d"
				   (sql-name (class-of object))(id object))))
  (remhash (id object) (loaded-objects store)))

(defmethod gen-slot-aux-tables-sql (class slot)
  (with-output-to-string (s)
    (when (or (list-of-values slot)(indexed slot))
      (format s "Informations supplémentaires pour le slot ~a~%~%" (slot-definition-name slot)))
    (if (list-of-values slot)
	(progn
	  (format s "CREATE TABLE ~a_~a (~%     parentid BIGINT NOT NULL,~%     ~a ~a NOT NULL,~%     ordernb INTEGER) WITHOUT OIDS~%~%~%"
		  (sql-name class)(sql-name slot)
		  (if (subtypep (value-type slot) 'root-object) "id" "value")
		  (compute-sql-type slot))
	  (format s "CREATE INDEX ~a_~a_idx ON ~a_~a(parentid)"
		  (sql-name class)(sql-name slot)(sql-name class)(sql-name slot)))
	(when (indexed slot)
	  (format s "CREATE ~a INDEX ~a_~a_vidx ON ~a(~a)~%~%"
		  (if (unique slot) "UNIQUE" "")
		  (sql-name class)(sql-name slot)(sql-name class)(sql-name slot))))))

(defmethod add-slot-aux-tables ((store psql-store) class slot)
  (if (list-of-values slot)
      (progn
	(sql-execute-command
	 (format nil "CREATE TABLE ~a_~a (parentid BIGINT NOT NULL, ~a ~a NOT NULL, ordernb INTEGER) WITHOUT OIDS"
		 (sql-name class)(sql-name slot)
		 (if (subtypep (value-type slot) 'root-object) "id" "value")
		 (compute-sql-type slot)))
	(sql-execute-command
	 (format nil "CREATE INDEX ~a_~a_idx ON ~a_~a(parentid)"
		 (sql-name class)(sql-name slot)(sql-name class)(sql-name slot))))
      (when (indexed slot)
	(sql-execute-command
	 (format nil "CREATE ~a INDEX ~a_~a_vidx ON ~a(~a)"
		 (if (unique slot) "UNIQUE" "")
		 (sql-name class)(sql-name slot)(sql-name class)(sql-name slot))))))

(defmethod drop-slot-aux-tables ((store psql-store) class slot)
  (when (and (stored slot)(list-of-values slot))
    (sql-execute-command
     (format nil "DROP TABLE ~a_~a" (sql-name class)(sql-name slot)))))

(defmethod create-class-table ((store psql-store) class)
  (when (symbolp class)(setf class (find-class class)))
  (format t "Create tables for class: ~a~%" (class-name class))
  (with-psql-store-db (store)
    (sql-execute-command (gen-sql-create-table class))
    (loop for slot in (class-slots class)
	  when (stored slot) do
	  (add-slot-aux-tables store class slot))))

(defmethod add-slot-to-class-table ((store psql-store) class slot)
  (when (symbolp class)(setf class (find-class class)))
  (setf slot (ensure-slot-def class slot))
  (format t "Add slot ~a to class ~a~%" (c2mop:slot-definition-name slot)(class-name class))
  (when (stored slot)
    (with-psql-store-db (store)
      (sql-execute-command
       (with-output-to-string (s)
	 (format s "ALTER TABLE ~a ADD " (sql-name class))
	 (gen-sql-for-slot slot s)))
      (add-slot-aux-tables store class slot))))

(defmethod remove-slot-from-class-table ((store psql-store) class sql-slot-name list-of-values)
  (when (symbolp class)(setf class (find-class class)))
  (format t "Remove slot ~a from class ~a~%" sql-slot-name (class-name class))
  (with-psql-store-db (store)
    #+drop-col-not-supported-in-psql
    (sql-execute-command
     (with-output-to-string (s)
       (format s "ALTER TABLE ~a DROP COLUMN ~a" (sql-name class) sql-slot-name)))
    #+drop-col-not-supported-in-psql
    (sql-execute-command
     (with-output-to-string (s)
       (format s "ALTER TABLE ~a rename COLUMN ~a TO ~a_del" (sql-name class) sql-slot-name sql-slot-name)))
    (when list-of-values
      (sql-execute-command
       (format nil "DROP TABLE ~a_~a" (sql-name class) sql-slot-name)))))

(defmethod rename-slot-in-class-table ((store psql-store) class old-sql-slot-name new-sql-slot-name list-of-values)
  (when (symbolp class)(setf class (find-class class)))
  (format t "Rename slot ~a to ~a in class ~a~%" old-sql-slot-name new-sql-slot-name (class-name class))
  (with-psql-store-db (store)
    (sql-execute-command
     (with-output-to-string (s)
       (format s "ALTER TABLE ~a rename COLUMN ~a TO ~a" (sql-name class) old-sql-slot-name new-sql-slot-name)))
    (when list-of-values
      (sql-execute-command
       (format nil "ALTER TABLE rename to ~a_~a" (sql-name class) new-sql-slot-name)))))

(defmethod drop-class-table ((store psql-store) class)
  (when (symbolp class)(setf class (find-class class)))
  (format t "Drop tables for class: ~a~%" (class-name class))
  (with-psql-store-db (store)
    (sql-execute-command (format nil "DROP TABLE ~a" (sql-name class)))
    (loop for slot in (class-slots class) do
	  (drop-slot-aux-tables store class slot))))

(defmethod close-store ((store psql-store))
  )

(defmethod update-object-parent-in-store (object (store psql-store))
  (let ((cmd (gen-sql-change-parent object)))
    (when cmd
      (with-psql-store-db (store)
        (sql-execute-command cmd)))))

#+nil
(setf *pool* (psql-create-db-pool "127.0.0.1" "MetaFractal2" "lisp"))
#+nil
(setf *default-store* (make-instance 'psql-store :db-pool *pool*))
