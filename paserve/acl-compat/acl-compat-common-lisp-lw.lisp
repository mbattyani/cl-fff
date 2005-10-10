(defpackage acl-compat-common-lisp
  (:use common-lisp)
  (:shadow make-hash-table)
  (:export make-hash-table))

(in-package :acl-compat-common-lisp)

(defun make-hash-table (&rest args &key test size rehash-size rehash-threshold (hash-function nil h-f-p) 
                              (values t) weak-keys)
  (declare (ignore hash-function))
  (when h-f-p (error "User defined hash-functions are not supported."))
  (let ((table (apply #'cl:make-hash-table :allow-other-keys t args)))
    (hcl:set-hash-table-weak table
                             (if weak-keys
                                 (if (eq values :weak)
                                     :both
                                   :key)
                               (if (eq values :weak)
                                   :value
                                 nil)))
    table))