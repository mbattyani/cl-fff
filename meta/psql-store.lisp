(in-package meta)

(export 'psql-store)

(defvar *forced-db-connection* nil)

(defmacro with-store-db ((store) &body body)
  (let ((forced (gensym "forced-"))
	(db (gensym "db-")))
    `(let* ((,forced *forced-db-connection*)
	    (,db (or ,forced (clsql:connect nil :pool (db-pool ,store)))))
      (unwind-protect
	   (let ((clsql:*default-database* ,db)) ,@body)
	(unless ,forced (clsql:disconnect :database ,db))))))

(defclass psql-store (store)
  ((db-pool :initform nil :accessor db-pool :initarg :db-pool)
   ))

(defmethod initialize-instance :after ((store psql-store) &rest init-options &key &allow-other-keys)
;  (hcl:set-hash-table-weak (loaded-objects store) :value)
  (open-store store))

(defmethod open-store ((store psql-store))
  )

(defun create-psql-database (db-pool database-name)
  (clsql:with-database (nil nil :pool db-pool)
    (clsql:execute-command (format nil "CREATE DATABASE ~a" database-name))))

(defun drop-psql-database (db-pool database-name)
  (clsql:with-database (nil nil :pool db-pool)
    (clsql:execute-command (format nil "DROP DATABASE ~a" database-name))))

(defmethod initialize-store ((store psql-store))
  (open-store store)
  (with-store-db (store)
    (clsql:create-sequence "object_id_seq")
    (clsql:execute-command
"CREATE TABLE ObjectNames (
  name text primary key,
  objectid BIGINT NOT NULL)
 WITHOUT OIDS")))

(defmethod create-new-object-id ((store psql-store) class-id)
  (with-store-db (store)
    (dpb class-id (byte 32 32) (clsql:sequence-next "object_id_seq"))))

(defmethod save-object-to-store ((store psql-store) object)
  (when (modified object)
    (with-store-db (store)
      (clsql:with-transaction ()
	(let ((new-original-lists ())
	      (sql-write-object (gen-sql-write-object object)))
	  (when sql-write-object
	    (clsql:execute-command sql-write-object))
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
		      (clsql:execute-command
		       (format nil "DELETE FROM ~a_~a WHERE parentid = ~d"
			       class-name slot-name (id object)))
		      (if (subtypep (value-type slot) 'root-object)
			  (loop for linked-obj in new-list for order from 1 do
				(clsql:execute-command
				 (format nil "INSERT INTO ~a_~a VALUES (~d, ~d, ~d)"
					 class-name slot-name (id object)(id linked-obj) order)))
			  (loop with value-to-sql-fn = (value-to-sql-fn slot)
				for value in new-list for order from 1 do
				(clsql:execute-command
				 (format nil "INSERT INTO ~a_~a VALUES (~d, ~d, ~d)"
					 class-name slot-name (id object)
					 (with-output-to-string (s)(funcall value-to-sql-fn value s))
					 order)))))
		    (push (cons slot (copy-list new-list)) new-original-lists))))
	  (setf (original-lists object) new-original-lists
		(modified object) nil
		(new-object object) nil))))))

(defmethod save-modified-objects ((store psql-store))
  (when (modified-objects store)
    (with-store-db (store)
      (let ((*forced-db-connection* clsql:*default-database*))
	(clsql:with-transaction ()
	  (dolist (object (modified-objects store))
	    (save-object-to-store store object))
	  (setf (modified-objects store) ()))))))

(defmethod read-object-proxy-from-store ((store psql-store) id)
  (let ((found (gethash id (loaded-objects store))))
    (if found
      found
      (let ((*default-store* store))
	(create-proxy-object id (ldb (byte 32 32) id))))))

(defmethod read-object-data-from-store ((store psql-store) object)
  (with-store-db (store)
    (let* ((*default-store* store)
	   (sql-data (clsql:query (gen-sql-read-object object) :types :auto))
	   (result (cdar sql-data))
	   (parent-id (pop result))
	   (class (class-of object))
	   (data-object (create-data-object object))
	   (id (id object)))
      (unless sql-data (format t "~%object not found ~a (~a)~%" id (class-name (class-of object)))
	      (loop for obj = (meta::parent object) then (meta::parent obj)
		    while obj do (format t "  ~s" (ignore-errors (short-description obj)))))
      (when sql-data
	(when parent-id
	  (setf (parent object) (read-object-from-store store parent-id)))
	(loop for slot in (class-slots class)
	      for sql-to-value-fn = (sql-to-value-fn slot)
	      do
	      (when (stored slot)
		(if (list-of-values slot)
		    (let ((list nil))
		      (if (subtypep (value-type slot) 'root-object)
			  (clsql:do-query ((child-id)
					   (format nil "SELECT id FROM ~a_~a WHERE parentid = ~d ORDER BY ordernb DESC"
						   (sql-name class)(sql-name slot)(id object))
					   :types :auto)
			    (let ((proxy (read-object-proxy-from-store store child-id)))
			      (when proxy
;				(read-object-data-from-store store proxy)
				(setf (parent proxy) object)
				(push proxy list))))
			  (clsql:do-query ((value)
					   (format nil "SELECT value FROM ~a_~a WHERE parentid = ~d ORDER BY ordernb DESC"
						   (sql-name class)(sql-name slot)(id object))
					   :types :auto)
			    (push (funcall sql-to-value-fn value) list)))
		      (push (cons slot (copy-list list))(original-lists object))
		      (setf (slot-value data-object (slot-definition-name slot)) list))
		    (setf (slot-value data-object (slot-definition-name slot))
			  (funcall sql-to-value-fn (pop result))))))
	(initialize-disable-predicates object)))))

(defmethod read-object-from-store ((store psql-store) id)
  (let ((found (gethash id (loaded-objects store))))
    (if found
      found
      (let ((object (read-object-proxy-from-store store id)))
	(when object
	  (load-object-data object)
	  object)))))

(defmethod load-named-object ((store psql-store) name)
  (with-store-db (store)
    (let ((id (caar (clsql:query
		     (format nil "select objectid from ObjectNames where name = '~a'" name) :types :auto))))
      (when id
	(load-object id store)))))

(defmethod register-named-object ((store psql-store) object name)
  (with-store-db (store)
    (clsql:execute-command (format nil "insert into ObjectNames (name, objectid) values ('~a', ~d)"
				   name (id object)))))

(defmethod load-or-create-named-object ((store psql-store) name class-id)
  (let ((object (load-named-object store name)))
    (unless object
      (setf object (make-instance (find-meta-class class-id) :store store))
      (register-named-object store object name))
    object))

(defmethod delete-object-from-store ((store psql-store) object)
  (with-store-db (store)
    (clsql:execute-command (format nil "delete from ~a where id = ~d"
				   (sql-name (class-of object))(id object))))
  (remhash (id object) (loaded-objects store)))

(defmethod add-slot-aux-tables ((store psql-store) class slot)
  (if (list-of-values slot)
      (progn
	(clsql:execute-command
	 (format nil "CREATE TABLE ~a_~a (parentid BIGINT NOT NULL, ~a ~a NOT NULL, ordernb INTEGER) WITHOUT OIDS"
		 (sql-name class)(sql-name slot)
		 (if (subtypep (value-type slot) 'root-object) "id" "value")
		 (compute-sql-type slot)))
	(clsql:execute-command
	 (format nil "CREATE INDEX ~a_~a_idx ON ~a_~a(parentid)"
		 (sql-name class)(sql-name slot)(sql-name class)(sql-name slot))))
      (when (indexed slot)
	(clsql:execute-command
	 (format nil "CREATE ~a INDEX ~a_~a_vidx ON ~a(~a)"
		 (if (unique slot) "UNIQUE" "")
		 (sql-name class)(sql-name slot)(sql-name class)(sql-name slot))))))

(defmethod drop-slot-aux-tables ((store psql-store) class slot)
  (when (and (stored slot)(list-of-values slot))
    (clsql:execute-command
     (format nil "DROP TABLE ~a_~a" (sql-name class)(sql-name slot)))))

(defmethod create-class-table ((store psql-store) class)
  (when (symbolp class)(setf class (find-class class)))
  (with-store-db (store)
    (clsql:execute-command (gen-sql-create-table class))
    (loop for slot in (class-slots class)
	  when (stored slot) do
	  (add-slot-aux-tables store class slot))))

(defmethod add-slot-to-class-table ((store psql-store) class slot)
  (when (symbolp class)(setf class (find-class class)))
  (setf slot (ensure-slot-def class slot))
    (when (stored slot)
      (with-store-db (store)
	(clsql:execute-command
	 (with-output-to-string (s)
	   (format s "ALTER TABLE ~a ADD " (sql-name class))
	   (gen-sql-for-slot slot s)))
	(add-slot-aux-tables store class slot))))

(defmethod remove-slot-from-class-table ((store psql-store) class sql-slot-name list-of-values)
  (when (symbolp class)(setf class (find-class class)))
  (with-store-db (store)
    #+drop-col-not-supported-in-psql
    (clsql:execute-command
     (with-output-to-string (s)
       (format s "ALTER TABLE ~a DROP COLUMN ~a" (sql-name class) sql-slot-name)))
    #+drop-col-not-supported-in-psql
    (clsql:execute-command
     (with-output-to-string (s)
       (format s "ALTER TABLE ~a rename COLUMN ~a TO ~a_del" (sql-name class) sql-slot-name sql-slot-name)))
    (when list-of-values
      (clsql:execute-command
       (format nil "DROP TABLE ~a_~a" (sql-name class) sql-slot-name)))))

(defmethod rename-slot-in-class-table ((store psql-store) class old-sql-slot-name new-sql-slot-name list-of-values)
  (when (symbolp class)(setf class (find-class class)))
  (with-store-db (store)
    (clsql:execute-command
     (with-output-to-string (s)
       (format s "ALTER TABLE ~a rename COLUMN ~a TO ~a" (sql-name class) old-sql-slot-name new-sql-slot-name)))
    (when list-of-values
      (clsql:execute-command
       (format nil "ALTER TABLE rename to ~a_~a" (sql-name class) new-sql-slot-name)))))

(defmethod drop-class-table ((store psql-store) class)
  (when (symbolp class)(setf class (find-class class)))
  (with-store-db (store)
    (clsql:execute-command (format nil "DROP TABLE ~a" (sql-name class)))
    (loop for slot in (class-slots class) do
	  (drop-slot-aux-tables store class slot))))

(defmethod drop-store ((store psql-store))
  (with-store-db (store)
    (clsql:drop-sequence "object_id_seq")
    (clsql:execute-command "DROP TABLE ObjectNames")))

(defmethod close-store ((store psql-store))
  )

;(setf *pool* (clsql:find-or-create-connection-pool '("213.11.22.163" "Test" "lisp" "") :postgresql))
;(setf *default-store* (make-instance 'psql-store :db-pool *pool*))
;(setf clsql-uffi::*clsql-uffi-library-filename* "/cvs/clsql/interfaces/clsql-uffi/clsql-uffi.dll")
;(clsql-uffi::load-uffi-foreign-library)