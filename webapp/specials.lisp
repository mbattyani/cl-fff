(in-package #:webapp)

(defvar *app* nil "The 'root' application object")

(defparameter *page* nil "The current page")
(defparameter *pages* (make-hash-table :test #'equal) "The pages dictionary")

