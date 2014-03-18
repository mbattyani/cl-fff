(in-package meta-web)

(defun make-sql-list (sql-list)
  `(make-instance 'meta::sqlsql-list-function
    :name ',(read-from-string (name f))
    :user-name ,(make-translation nil (user-name f))
    :object-help ,(make-object-help (object-help f))))
