(in-package meta)

;;; a postgresql interface layer for lispworks Common SQL

(require "postgresql")

(defun init-psql ()
  (fli:set-locale-encodings :utf-8  'fli::unicode-wchar #-unix :unicode))

(defvar *forced-psql-connection* nil)
(defvar *psql-transaction-level* nil)
(defvar *psql-transaction-status* nil)
(defvar *psql-transaction-commit-hooks* nil)
(defvar *psql-transaction-rollback-hooks* nil)


(cl:defclass psql-db-pool ()
  ((connection-args :initform nil :accessor connection-args :initarg :connection-args)
   (lock :initform (mp:make-lock :name (format nil "pool-lock~d" (random 1000000)))
         :accessor lock)
   (free-connections :initform nil :accessor free-connections)
   (used-connections :initform nil :accessor used-connections)))

(defun psql-create-db-pool (host database-name user &optional pwd other-params)
  (make-instance 'psql-db-pool
                 :connection-args (list* :host host :user user :dbname database-name
                                         :pwd pwd other-params)))

(defun get-psql-connection (pool)
  (mp:with-lock ((lock pool))
    (unless (free-connections pool)
      (push (sql:connect (connection-args pool)) (free-connections pool)))
    (let ((conn (pop (free-connections pool))))
      (push conn (used-connections pool))
      conn)))

(defun release-psql-connection (conn pool)
  (mp:with-lock ((lock pool))
    (setf (used-connections pool) (delete conn (used-connections pool)))
    (push conn (free-connections pool))))

(cl:defclass psql-database ()
  ((db-pool :initform nil :accessor db-pool :initarg :db-pool)))

(defmacro with-psql-store-db ((store) &body body)
  (lw:with-unique-names (forced pool conn)
    `(let* ((,forced *forced-psql-connection*)
            (,pool (db-pool ,store))
	    (,conn (or *forced-psql-connection* (get-psql-connection ,pool))))
      (unwind-protect
	   (let ((sql:*default-database* ,conn)
                 (*psql-transaction-level* 0)
                 (*psql-transaction-status* nil))
             ,@body)
	(unless ,forced (release-psql-connection ,conn ,pool))))))

(defmacro sql-execute-command (command)
  `(sql:execute-command ,command))

(defmacro sql-query (query)
  `(sql:query ,query))

(defmacro sql-do-query (&rest args-and-body)
  `(sql:do-query ,@args-and-body))

(defun psql-start-transaction ()
  (when (= (incf *psql-transaction-level*) 1)
    (setf *psql-transaction-status* nil)
    (sql:execute-command "BEGIN;")))

(defun psql-end-transaction ()
  (if (> *psql-transaction-level* 0)
    (when (zerop (decf *psql-transaction-level*))
      (if (eq *psql-transaction-status* :commited)
          (progn
	    (sql:execute-command "COMMIT;")
	    (map nil #'funcall *psql-transaction-commit-hooks*))
	  (unwind-protect ;status is not :commited
	       (sql:execute-command "ROLLBACK;")
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
