(in-package #:html)

;;; The frontend classes

(defclass frontend ()
  ((browser :accessor browser :initform nil :initarg :browser)
   (browser-version :accessor browser-version :initform nil :initarg :browser-version)
   (version :accessor version :initform nil :initarg :version)
   (screen-width  :accessor screen-width :initform nil :initarg :screen-width)
   (screen-height  :accessor screen-height :initform nil :initarg :screen-height)))

(defclass phone-mixin ()
  ())

(defmethod is-phone (fe) nil)
(defmethod is-phone ((fe phone-mixin)) t)

(defclass tablet-mixin ()
  ())

(defmethod is-tablet (fe) nil)
(defmethod is-tablet ((fe tablet-mixin)) t)

(defclass computer-mixin ()
  ())

(defmethod is-computer (fe) nil)
(defmethod is-computer ((fe computer-mixin)) t)

(defclass robot-crawler (frontend)
  ())

(defmethod is-robot (fe) nil)
(defmethod is-robot ((fe robot-crawler)) t)

(defclass html (frontend)
  ())

(defmethod is-html (fe &optional strict) nil)
(defmethod is-html ((fe html) &optional strict)
  (if strict
      (eq (type-of fe) 'html)
      t))

(defclass bootstrap (html)
  ())

(defmethod is-bootstrap (fe &optional strict) nil)
(defmethod is-bootstrap ((fe bootstrap) &optional strict)
  (if strict
      (eq (type-of fe) 'bootstrap)
      t))

(defvar *frontend* (make-instance 'bootstrap) "The current frontend") ; ugly hack for now
(defparameter *default-frontend* *frontend*) ; yeah I knpw...
