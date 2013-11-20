(in-package #:meta)

(defun init-psql ()
#+nil  (setf postmodern:*database* (postmodern:connect "MetaFractal" "lisp" "" "127.0.0.1" :pooled-p t))
)

(defvar *forced-psql-connection* nil)
(defvar *psql-transaction-level* nil)
(defvar *psql-transaction-status* nil)
(defvar *psql-transaction-commit-hooks* nil)
(defvar *psql-transaction-rollback-hooks* nil)

(cl:defclass psql-db-pool ()
  ((connection-args :initform nil :accessor connection-args :initarg :connection-args)
   (lock :initform (bt:make-recursive-lock (format nil "pool-lock~d" (random 1000000)))
         :accessor lock)
   (free-connections :initform nil :accessor free-connections)
   (used-connections :initform nil :accessor used-connections)))

(defun psql-create-db-pool (host database-name user &optional pwd other-params)
  (make-instance 'psql-db-pool
                 :connection-args (list* :host host :user user :dbname database-name
                                         :pwd pwd other-params)))

(defun get-psql-connection (pool)
  (bt:with-lock-held ((lock pool))
    (unless (free-connections pool)
      (let ((connection-args (connection-args pool)))
        (setf postmodern:*database* (postmodern:connect (getf connection-args :dbname)
                                                        (getf connection-args :user)
                                                        (getf connection-args :pwd)
                                                        (getf connection-args :host) :pooled-p t))
        (push postmodern:*database* (free-connections pool))))
    (let ((conn (pop (free-connections pool))))
      (push conn (used-connections pool))
      conn)))

(defun release-psql-connection (conn pool)
  (bt:with-lock-held ((lock pool))
    (setf (used-connections pool) (delete conn (used-connections pool)))
    (push conn (free-connections pool))
    #+nil(postmodern:disconnect-toplevel)))

(cl:defclass psql-database ()
  ((db-pool :initform nil :accessor db-pool :initarg :db-pool)))

(defmacro with-psql-store-db ((store) &body body)
  (ut:with-gensyms (forced pool conn)
    `(let* ((,forced *forced-psql-connection*)
            (,pool (db-pool ,store))
	    (,conn (or *forced-psql-connection* (get-psql-connection ,pool))))
      (unwind-protect
	   (let ((postmodern:*database* ,conn)
                 (*psql-transaction-level* 0) ;;fixme
                 (*psql-transaction-status* nil)
                 )
             ,@body)
	(unless ,forced (release-psql-connection ,conn ,pool))))))

(defmacro sql-execute-command (command)
  `(postmodern:execute ,command))

(defmacro sql-query (query)
  `(postmodern:query ,query))

; #+nil
(defmacro sql-do-query ((&rest args) query &body body)
  `(postmodern:doquery ,query ,args
     ,@body))

(defun psql-start-transaction ()
  (when (= (incf *psql-transaction-level*) 1)
    (setf *psql-transaction-status* nil)
    (postmodern:execute "BEGIN;")))

(defun psql-end-transaction ()
  (if (> *psql-transaction-level* 0)
    (when (zerop (decf *psql-transaction-level*))
      (if (eq *psql-transaction-status* :commited)
          (progn
	    (postmodern:execute "COMMIT;")
	    (map nil #'funcall *psql-transaction-commit-hooks*))
	  (unwind-protect ;status is not :commited
	       (postmodern:execute "ROLLBACK;")
	    (map nil #'funcall *psql-transaction-rollback-hooks*))))
    (warn "Continue without commit."
	  'simple-error
	  :format-control "Cannot commit transaction because there is no transaction in progress.")))


(defun sql-add-transaction-commit-hook (commit-hook)
    (push commit-hook *psql-transaction-commit-hooks*))

(defun sql-add-transaction-rollback-hook (rollback-hook)
    (push rollback-hook *psql-transaction-rollback-hooks*))

(defun sql-rollback-transaction ()
  (setf *psql-transaction-status* :rolled-back))

(defun sql-commit-transaction ()
  (when (not *psql-transaction-status*)
    (setf *psql-transaction-status* :commited)))

(defmacro psql-with-transaction (&rest body)
  `(unwind-protect
        (progn
          (psql-start-transaction)
          ,@body
          (sql-commit-transaction))
     (psql-end-transaction)))


(defmacro psql-with-transaction (&rest body)
  `(postmodern:with-transaction ()
     ,@body))


;; store

#+nil(defmethod read-object-data-from-store ((store psql-store) object)
  (with-psql-store-db (store)
    (let* ((*default-store* store)
	   (sql-data (sql-query (gen-sql-read-object object)))
	   (result (cdar sql-data))
	   (parent-id (pop result))
	   (class (class-of object))
	   (data-object (create-data-object object))
	   (id (id object)))
      (unless sql-data
	(utility:with-logged-errors (:ignore-errors t)
	  (error (with-output-to-string (s)
		   (format s "~%object not found ~a (~a)" id (class-name (class-of object)))
		   (loop for obj = (meta::parent object) then (meta::parent obj)
			 while obj do (format s " =>  ~a (~a, ~s)"
					      (meta::id obj)
                                              (ignore-errors (class-name (class-of obj)))
					      (ignore-errors (short-description obj))))
                   (when *preloaded-objects-stack*
                     (format s " load-path: ")
                     (loop for obj in (reverse *preloaded-objects-stack*)
                           do (format s " =>  ~a (~a, ~s)"
                                      (meta::id obj)
                                      (ignore-errors (class-name (class-of obj)))
                                      (ignore-errors (short-description obj)))))
                   (format s "~%")))))
      (when sql-data
	(when parent-id
	  (setf (parent object) (read-object-from-store store parent-id)))
	(loop for slot in (class-slots class)
	      for sql-to-value-fn = (sql-to-value-fn slot)
	      for linked-value = (linked-value slot)
	      do
	      (when (stored slot)
		(if (list-of-values slot)
		    (let ((list nil))
		      (if (subtypep (value-type slot) 'root-object)
			  (sql-do-query
                              (child-id)
                               (:ORDER-BY
                                (:select 'id :from (format nil "~a_~a" (sql-name class) (sql-name slot)) :where (:= 'parentid (id object)))
                                (:DESC 'ordernb))
                            (let ((proxy (read-object-proxy-from-store store child-id)))
                              (when proxy
                                (unless linked-value
                                  (setf (parent proxy) object))
                                (push proxy list))))
                            (sql-do-query
                                 (value)
                                 (:ORDER-BY
                                   (:SELECT 'value :FROM (format nil "~a_~a" (sql-name class)(sql-name slot)) :WHERE (:= 'parentid (id object)))
                                   (:Desc 'ordernb))
                                (push (funcall sql-to-value-fn value) list)))
		      (push (cons slot (copy-list list))(original-lists object))
		      (setf (slot-value data-object (slot-definition-name slot)) list))
		    (setf (slot-value data-object (slot-definition-name slot))
			  (funcall sql-to-value-fn (pop result))))))
        (initialize-unbound-slots object)
	(initialize-disable-predicates object)))))
