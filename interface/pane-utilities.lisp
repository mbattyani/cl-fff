(in-package interface)

(defconstant +default-x-spacing+ 5)
(defconstant +default-x-spacing-min+ 2)
(defconstant +default-x-spacing-max+ 10)
(defconstant +default-y-spacing+ 3)
(defconstant +default-y-spacing-min+ 2)
(defconstant +default-y-spacing-max+ 10)

(defun basic-space ()
  (make-instance 'pane
		 :x-value +default-x-spacing+ :x-max +default-x-spacing-max+ :x-min +default-x-spacing-min+ :x-compressibility 10.0
		 :y-value +default-y-spacing+ :y-max +default-y-spacing-max+ :y-min +default-y-spacing-min+ :y-compressibility 10.0))

(export 'make-matrix-layout-pane)
(defun make-matrix-layout-pane (pane-list &key nx ny nb (x-spacing +default-x-spacing+) (y-spacing +default-y-spacing+) x-layout-first)
;  (dolist (p pane-list) (restore-elastics p))
  (let ((list-length (length pane-list)))
    (unless nb (setf nb list-length))
    (unless nx
	    (if (not ny)
		(setf nx (if (<= nb 2) nb (ceiling nb (isqrt nb))))
	      (setf nx (ceiling nb ny))))
    (unless ny (setf ny (ceiling nb nx)))
    (setf nb (* nx ny))
    (unless (= nb list-length)
	    (let ((l ()))
	      (dotimes (p (- nb list-length))
		       (push (basic-space) l))
	    (setf pane-list (append pane-list l))
	    )))
  (let ((matrix (make-array `(,nx ,ny)))
	(x 0)
	(y 0))
    (if x-layout-first
	(dotimes (y ny)
		 (dotimes (x nx)
			  (setf (aref matrix x y) (pop pane-list))))
      (dotimes (x nx)
	       (dotimes (y ny)
			(setf (aref matrix x y) (pop pane-list)))))
    (multiple-value-bind (col-x-elastics row-y-elastics)
			 (%compute-matrix-border-elastics% matrix)
			 (%propagate-matrix-border-elastics% matrix col-x-elastics row-y-elastics)
			 (values (%make-result-pane% matrix x-spacing y-spacing) nx ny))))

;; return 2 vectors containing the cols x-elastics and the rows y-elastics
(defun %compute-matrix-border-elastics% (pane-matrix)
  (let* ((nx (array-dimension pane-matrix 0))
	(ny (array-dimension pane-matrix 1))
	(col-elastics (make-array nx))
	(row-y-elastics (make-array ny))
	(x 0)
	(y 0))
    (dotimes (x nx)
	(let ((list ()))
	  (dotimes (y ny)
	     (push (x-elastic (aref pane-matrix x y)) list))
	  (setf (aref col-elastics x) (make-parallel-elastic list))))
    (dotimes (y ny)
	(let ((list-x ())
	      (list-y ()))
	  (dotimes (x nx)
		   (push (y-elastic (aref pane-matrix x y)) list-y))
	  (setf (aref row-y-elastics y) (make-parallel-elastic list-y))))
    (values col-elastics row-y-elastics)))

(defun %propagate-matrix-border-elastics% (pane-matrix col-elastics row-elastics)
  (let* ((nx (array-dimension pane-matrix 0))
	(ny (array-dimension pane-matrix 1))
	(x 0)
	(y 0))
    (dotimes (x nx)
	     (let ((elastic (aref col-elastics x)))
	       (dotimes (y ny)
			(let ((el (x-elastic (aref pane-matrix x y))))
			  (setf (initial-value el) (initial-value elastic))
			  (setf (max-value el) (max-value elastic))
			  (setf (elasticity el) (elasticity elastic))
;			  (setf (compressibility el) (compressibility elastic))
	       ))))
    (dotimes (y ny)
	     (let ((elastic (aref row-elastics y)))
	       (dotimes (x nx)
			(let ((el (y-elastic (aref pane-matrix x y))))
			  (setf (initial-value el) (initial-value elastic))
			  (setf (max-value el) (max-value elastic))
			  (setf (elasticity el) (elasticity elastic))
;			  (setf (compressibility el) (compressibility elastic))
			  ))))))

(defun %make-result-pane% (pane-matrix x-spacing y-spacing)
  (let ((nx (array-dimension pane-matrix 0))
	(ny (array-dimension pane-matrix 1))
	(panes (list (basic-space)))
	(x 0)
	(y 0)
	(x-max-spacing (* x-spacing 3))
	(y-max-spacing (* y-spacing 3)))
    (dotimes (y ny)
	     (push (make-instance 'pane :layout :horizontal
				  :sub-panes (let ((row-panes (list (basic-space))))
					       (dotimes (x nx)
							(push (aref pane-matrix (- nx x 1) (- ny y 1)) row-panes)
							(push (basic-space) row-panes))
					       row-panes))
		   panes)
	     (push (basic-space) panes))
    (make-instance 'pane :layout :vertical :sub-panes panes)))

(defclass hlabel-value-pane (pane)
 ((label-pane :initform () :accessor label-pane :initarg :label-pane)
  (value-pane :initform () :accessor value-pane :initarg :value-pane))
 (:default-initargs :layout :horizontal))

(defmethod initialize-instance :after ((p hlabel-value-pane) &rest init-options &key &allow-other-keys)
  (declare (ignore init-options))
  (setf (sub-panes p) (list (basic-space)
			    (label-pane p)
			    (basic-space)
			    (value-pane p)
			    (basic-space)))
  (setf (need-to-recompute p) t)
  (recompute-elastics p))

(defun optimize-hlabel-value-layouts (panes)
  (when panes
	(let ((global-label-elastic (make-parallel-elastic
				     (mapcar #'(lambda(p)
						 (x-elastic (label-pane p)))
					     panes)))
	      (global-value-elastic (make-parallel-elastic
				     (mapcar #'(lambda(p)
						 (x-elastic (value-pane p)))
					     panes))))
	  (dolist (p panes)
		  (setf (x-elastic (label-pane p)) (duplicate-elastic global-label-elastic)
		  (x-elastic (value-pane p)) (duplicate-elastic global-value-elastic))))))
  
(defclass vertical-layout-pane (pane)
  ()
  (:default-initargs :layout :vertical))

(defmethod initialize-instance :after ((p vertical-layout-pane) &rest init-options &key pane-list no-spaces (align :top) &allow-other-keys)
  (setf (sub-panes p) (if no-spaces
			pane-list
			(make-vertical-layout-list pane-list align))))
  
;(defun make-vertical-layout-pane (pane-list &key (align :top))
;  (make-instance 'vertical-layout-pane :pane-list pane-list :align align))

(defun make-vertical-layout-list (pane-list &optional (align :top))
  (let ((top-pane (basic-space))
	(sub-panes ()))
    (when (member align '(:bottom :center))
	  (let ((el (y-elastic top-pane)))
	    (setf (elasticity el) 100.0
		  (max-value el) 10000.0)))
    (push top-pane sub-panes)
    (dolist (pane pane-list)
	    (push pane sub-panes)
	    (push (basic-space) sub-panes))
    (when (member align '(:top :center))
	  (let ((el (y-elastic (first sub-panes))))
	    (setf (elasticity el) 100.0
		  (max-value el) 10000.0)))
    (nreverse sub-panes)))

(defclass horizontal-layout-pane (pane)
  ()
  (:default-initargs :layout :horizontal))

(defmethod initialize-instance :after ((p horizontal-layout-pane) &rest init-options &key pane-list (align :left) &allow-other-keys)
  (setf (sub-panes p) (make-horizontal-layout-list pane-list align)))

;(defun make-horizontal-layout-pane (pane-list &optional (align :left))
;  (make-instance 'horizontal-layout-pane :pane-list pane-list :align align))

(defun make-horizontal-layout-list (pane-list &optional (align :left))
  (let ((left-pane (basic-space))
	(sub-panes ()))
    (when (member align '(:right :center))
	  (let ((el (x-elastic left-pane)))
	    (setf (elasticity el) 100.0
		  (max-value el) 10000.0)))
    (push left-pane sub-panes)
    (dolist (pane pane-list)
	    (push pane sub-panes)
	    (push (basic-space) sub-panes))
    (when (member align '(:left :center))
	  (let ((el (x-elastic (first sub-panes))))
	    (setf (elasticity el) 100.0
		  (max-value el) 10000.0)))
    (nreverse sub-panes)))

(export 'map-all-panes)
(defun map-all-panes (function panes)
  (cond
   ((null panes) nil)
   ((listp panes) (dolist (p panes) (map-all-panes function p)))
   (t (funcall function panes)
      (map-all-panes function (sub-panes panes)))))

(defmethod optimize-layouts ((p pane))
  (map-all-panes #'optimize-layouts (sub-panes p))
  p)

(defmethod optimize-layouts ((p vertical-layout-pane))
  (when (sub-panes p)
	(optimize-hlabel-value-layouts (remove-if-not #'(lambda (x) (typep x 'hlabel-value-pane)) (sub-panes p)))
	p))

(defun h-space (value &key min max compressibility elasticity)
  (make-instance 'pane :x-value value :x-min min :x-max max :x-compressibility compressibility :x-elasticity elasticity))

(defun v-space (value &key min max compressibility elasticity)
  (make-instance 'pane :y-value value :y-min min :y-max max :y-compressibility compressibility :y-elasticity elasticity))

