(in-package interface)

(defvar *web-contexts* (make-hash-table))
(defvar *context-id* 100)

(defun get-context-id ()
  (incf *context-id*))

(defclass web-context ()
  ((id :accessor id :initform (get-context-id))))

