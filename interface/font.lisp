(in-package interface)

(defclass font-object ()
 ((face-name :initform "arial" :accessor face-name :initarg :face-name)
  (height :initform -10 :accessor height :initarg :height)
  (bold :initform nil :accessor bold :initarg :bold)
  (italic :initform nil :accessor italic :initarg :italic)))

(defclass font-table ()
 ((face-name :accessor face-name :initarg :face-name)
  (sizes :initform (make-hash-table) :accessor sizes))) ;size -> (sizes<normal> sizes<bold>  sizes<italic> sizes<bold italic>)

(defvar *font-tables* (make-hash-table :test #'equal))
  
(defvar *control-default-font* (make-instance 'font-object :height 12 :face-name "arial"))

(defun text-size (text &optional (font *control-default-font*))
  (let ((font-table (gethash (face-name font) *font-tables*))
	(dy 10)
	(dx 10)
	(idx 1))
    (when (bold font) (incf idx))
    (when (italic font) (incf idx 2))
    (when font-table
      (let* ((sizes (gethash (height font) (sizes font-table)))
	     (size-array (elt sizes idx)))
	(declare (type (array 256 (unsigned-byte 8)) sizes))
	(when sizes
	  (setf dy (first sizes))
	  (setf dx (loop for c across text sum (aref size-array (char-code c)))))))
    (values dx dy)))