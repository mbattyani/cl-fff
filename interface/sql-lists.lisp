(in-package interface)

(defclass html-list ()
  ((limit  :initform 50 :accessor limit :initarg :limit)
   (offset :initform 0 :accessor offset)
   (total-count  :initform 0 :accessor total-count)
   (html-fn :initform nil :accessor html-fn)
   ))

(defclass html-lisp-list (html-list)
  ((get-data-fn :accessor get-data-fn :initarg :get-data-fn)
   (set-data-fn :accessor set-data-fn :initarg :set-data-fn)
   ))

(defclass sql-list (html-list)
  ((query-fn  :accessor query-fn :initarg :query-fn)
   (result :initform nil :accessor result)))

(defclass sql-object-list (sql-list)
  ())

(defmethod initialize-instance :after ((sql-list sql-list) &rest init-options &key &allow-other-keys)
  (setf (total-count sql-list) (funcall (query-fn sql-list) nil nil :count-only t))
  (fetch-values sql-list))

(defun fetch-values (sql-list &optional offset limit)
  (when offset (setf (offset sql-list)(if (< offset 0) 0 offset)))
  (when limit (setf (limit sql-list)(if (< limit 1) 1 limit)))
  (setf (result sql-list) nil)
  (setf (result sql-list)
	(funcall (query-fn sql-list)(offset sql-list)(limit sql-list))))

(defun fetch-first-values (sql-list &optional (limit (limit sql-list)))
  (fetch-values sql-list 0 limit))

(defun fetch-prev-values (sql-list &optional (limit (limit sql-list)))
  (fetch-values sql-list (- (offset sql-list) limit) limit))

(defun fetch-next-values (sql-list &optional limit)
  (fetch-values sql-list (+ (offset sql-list) (limit sql-list)) limit))

(defun fetch-last-values (sql-list &optional limit)
  (fetch-values sql-list (- (total-count sql-list) (limit sql-list)) limit))

(defclass html-sql-list (html-item)
  ((query-fn   :initform nil :accessor query-fn :initarg :query-fn)
   (action-func  :initform nil :accessor action-func :initarg :action-func)
   (col-funcs    :initform nil :accessor col-funcs)
   (table-class  :initform "" :accessor table-class)))

(defclass html-sql-list-dispatcher (slot-dispatcher)
  ((sql-list :accessor sql-list :initarg :sql-list)))

(defmethod make-dispatcher (interface object (item html-sql-list))
  (make-instance 'html-sql-list-dispatcher :interface interface :object object :item item
		 :sql-list (make-instance 'sql-list :query-fn (query-fn item))))

(defmethod make-set-value-javascript ((item html-sql-list) param1 param2)
  (let* ((sql-list (sql-list *dispatcher*)))
    (html:fast-format nil "parent.document.all.~a.innerHTML='~a';" (name item)
		      (html:quote-javascript-string
		       (html:html-to-string
			((:table :class (table-class item))
			 (loop for values in (result sql-list)
			       for index from (offset sql-list) do
			       (html:html (:tr 
					   (loop for value in values
						 for col-fn in (col-funcs item)
						 do (html:html (funcall col-fn index value))))))))))))

(defun sql-list-action-func (object click-id)
  (let ((sql-list (sql-list *dispatcher*)))
    (case click-id
      (0 (fetch-first-values sql-list))
      (1 (fetch-prev-values sql-list))
      (2 (fetch-next-values sql-list))
      (3 (fetch-last-values sql-list)))
    (send-to-interface (make-set-value-javascript (item *dispatcher*) nil nil)
		       (interface *dispatcher*))))

;;;css-classes suffixes:
;;; "" =  global
;;; "h" = header-table, "hr" = header row, "h1"..."hxx" = column xx header
;;; "t" = table, "r" = row, "c1"..."cxx" = column xx td style
;;; "b" = buttons 

(defun sql-list-tag (attributes forms)
  (destructuring-bind (&key (height 100) (width 500) (class "stdlist") query-fn
			    (title-height 25)(buttons-height 25)(buttons-height 25)) attributes
    (let ((item (make-instance 'html-sql-list :action-func 'sql-list-action-func
			       :query-fn query-fn)))
      (loop for (title width . form) in forms
	    for i from 1
	    collect (compile nil
		      `(lambda (index value)
			(html:html ((:td :class ,(format nil "~ac~d" class i)
					 :style ,(format nil "width:~dpx;" width))
				    ,@form)))) into funcs
				    finally (setf (col-funcs item) funcs))
      (setf (table-class item) (concatenate 'string class "t"))
	`(html:html
	  ((:div :class ,class :style ,(format nil "width:~dpx;height:~dpx;" width height))
	   ((:table :class ,(concatenate 'string class "h")
		    :style ,(format nil "width100%;height:~dpx;" title-height))
	    (:tr ,@(loop for (title width . form) in forms
			 for i from 1
			 collect `(html:html ((:th :class ,(format nil "~ah~d" class i)
					       :style ,(format nil "width:~dpx;" width))
					      ,title)))))
	   ((:div :class ,(concatenate 'string class "h")
		  :style ,(format nil "width:100%;height:~dpx;overflow:auto;"
				  width (- height title-height buttons-height))
		  :id ,(name item))  "list" :br)
	   ((:div  :style ,(format nil "height:~dpx;width:100%" buttons-height)) " "
	    ((:input :type "submit" :value "first" :class ,(concatenate 'string class "b")
		     :onclick ,(format nil "fire_onclick('~a', 0)" (name item)))) " "
	    ((:input :type "submit" :value "prev 25" :class ,(concatenate 'string class "b")
		     :onclick ,(format nil "fire_onclick('~a', 1)" (name item)))) " "
	    ((:input :type "submit" :value "next 25" :class ,(concatenate 'string class "b")
		     :onclick ,(format nil "fire_onclick('~a', 2)" (name item)))) " "
	    ((:input :type "submit" :value "last" :class ,(concatenate 'string class "b")
		     :onclick ,(format nil "fire_onclick('~a', 3)" (name item))))
	    ))))))

(html:add-func-tag :sql-list 'sql-list-tag)
;;;syntax ((:sql-list :class "css" :width width :height height :query-fn func)
;;;         ("col-title" col-width lhtml-col-forms)

