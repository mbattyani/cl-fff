(in-package interface)

(defvar *layout-stream* t)
(defvar *root-item* nil)
(defvar *all-object-views* (make-hash-table :test #'equal))
(defvar *all-list-formats* (make-hash-table :test #'equal))
(defvar *current-class* nil)

(defun find-object-view (name &optional instance)
  (gethash name *all-object-views*))

(defmethod modifiable-p (obj)
  t)

(defmethod modifiable-p ((obj html-item))
  (if (slot obj)
      (modifiable-p (slot obj))
      t))

(defmethod modifiable-p ((slot meta::fc-slot-definition-mixin))
  (or (meta::modifiable slot)
      (intersection *user-groups* (meta::modifiable-groups slot))))

(defmethod visible-p (obj)
  t)

(defmethod visible-p ((slot meta::fc-slot-definition-mixin))
  (or (meta::visible slot)
      (intersection *user-groups* (meta::visible-groups slot))))

(defmethod visible-p ((class meta::fc-class))
  (or (meta::visible class)
      (intersection *user-groups* (meta::visible-groups class))))


(defmethod visible-p ((slot meta::fc-function))
  (or (meta::visible slot)
      (intersection *user-groups* (meta::visible-groups slot))))

(defmethod dynamic-groups (user object user-groups)
  user-groups)

(defclass object-view ()
  ((name   :initform ""  :accessor name :initarg :name)
   (object-class :initform nil  :accessor object-class :initarg :object-class)
   (special-view   :initform nil  :accessor special-view :initarg :special-view)
   (country-languages  :initform nil :accessor country-languages :initarg :country-languages)
   (user-groups  :initform nil :accessor user-groups :initarg :user-groups)
   (all-items :initform (make-hash-table :test #'equal) :accessor all-items)
   (html-func :initform nil :accessor html-func)
   (source-code :initform nil :accessor source-code :initarg :source-code)
   (other-instances :initform (make-array 5 :fill-pointer 0 :adjustable t) :accessor other-instances)
   (root-view :initform nil :accessor root-view)
   (generated :initform nil :accessor generated :initarg :generated)
   ))

(defmethod initialize-instance :after ((view object-view) &rest init-options
				       &key root-view instance-id &allow-other-keys)
  (if root-view
    (progn
      (setf (name view) (format nil "~a~d" (name root-view) instance-id)
	    (object-class view)(object-class root-view)
	    (country-languages view)(country-languages root-view)
	    (user-groups view)(user-groups root-view)
	    (source-code view)(source-code root-view)
	    (root-view view) root-view)
      (vector-push-extend view (other-instances root-view)))
    (progn
      (setf (object-class view) (ensure-class (object-class view)))
      (pushnew view (meta::direct-views (object-class view)))
      (vector-push-extend view (other-instances view))
      (setf (root-view view) view)
      (setf (gethash (name view) *all-object-views*) view))))

(defun get-view-instance (view)
  (let ((instance-id (count view *request-views* :key #'(lambda (x) (root-view (first x))))))
    (if (zerop instance-id)
      view
      (if (<= (length (other-instances view)) instance-id)
	(make-instance 'object-view :root-view view :instance-id instance-id)
	(aref (other-instances view) instance-id)))))

(defun ensure-class (class)
  (if (symbolp class) (find-class class) class))

(defun ensure-slot (slot class)
  (if (symbolp slot)
    (find slot (clos:class-slots class)
	  :test #'eql :key #'clos:slot-definition-name)
    slot))

(defmethod html-func :around ((view object-view))
  (let ((func (call-next-method)))
    (unless func
      (let* ((*current-class* (object-class view))
;	     (*country-language* (country-language view))
	     (*root-item* view))
	(clrhash (all-items view))
	(setf func (compile
		    nil `(lambda ()
			  (if (visible-p ,*current-class*)
			      (progn ,@(mapcar 'html::html-gen (source-code view)))
			      (html:html (:translate
					  '(:en "You do not have the rights to access this object."
					    :fr "Vous n'avez pas les droits d'accés pour cet objet.")))))))
	(setf (html-func view) func)))
    func))

;;; CSS classes dv => default view + t = table, t2 = sub obj table,
;;; r = row, ch = slot name column, cv = value column

(defun make-std-object-slots-view (class slots no-table)
  (setf class (ensure-class class))
  (unless slots (setf slots (clos::class-slots class)))
    (list
     (cons (if no-table :progn '(:table :class "dvt"))
	   (loop for slot-name in slots
		 for slot = (ensure-slot slot-name class)
		 for user-name = (list :translate (meta::user-name slot))
		 for unit = (when (meta::unit slot) (concatenate 'string " (" (meta::unit slot) ")"))
		 when (or (meta::visible slot)(meta::visible-groups slot))
		 collect
		 (append (if (meta::visible slot) '(:progn) `(:when (visible-p ,slot)))
			 (list
		 (cond
		   ((meta::list-of-values slot)
		    (case (meta::view-type slot)
		      (:list-val
		       `((:tr :class "dvr")((:td :class "dvch") ,user-name ,unit)
			 ((:td :class "dvcv")
			  ((:slot-list ,(clos:slot-definition-name slot)
				       ,@(html:merge-attributes
					  (meta::html-tag-attributes slot)
					  '(:class "dvl" :height "60px")))))))
		      (:pick-mval
		       `((:tr :class "dvr")((:td :class "dvch") ,user-name ,unit)
			 ((:td :class "dvcv")
			  ((:slot-pick-mval ,(clos:slot-definition-name slot)
					    ,@(html:merge-attributes
					       (meta::html-tag-attributes slot)
					       '(:class "dvcv")))))))
		      (t
		       `((:tr :class "dvr")
			 ((:td :class "dvch2" :colspan "2")
			  ((:slot-list ,(clos:slot-definition-name slot)
				       ,@(html:merge-attributes
					  (meta::html-tag-attributes slot)
					  '(:class "dvl")))
			   (:table (:tr ((:td :class "dvch2") ,user-name ,unit)))))))))
		   ((meta::fc-class-p (meta::value-type slot))
		    (case (meta::view-type slot)
		      ((:embed t)
		       `((:tr :class "dvr") ((:td :class "dvch2" :colspan "2")
					     ((:table :class "dvt2") (:tr ((:td :class "dvch2") ,user-name))
					      (:tr (:td (:object-view :object (,(meta::accessor slot) interface::*object*))))))))
		      (:embed-val
		       `((:tr :class "dvr") ((:td :class "dvch") ,user-name)
			 ((:td :class "dvcv") (:object-view :object (,(meta::accessor slot) interface::*object*)))))
		      (t
		       `((:tr :class "dvr") ((:td :class "dvch") ,user-name)
			 ((:td :class "dvcv")((:slot-obj-link ,(clos:slot-definition-name slot) :class "dvcv")))))))
		   ((eq (meta::value-type slot) :color)
		    `((:tr :class "dvr") ((:td :class "dvch") ,user-name)
		      ((:td :class "dvcv") ((,(if (eq (meta::view-type slot) :edit) :slot-edit :slot-pick-color)
					      ,(clos:slot-definition-name slot) :class "dvcve"
					      ,@(meta::html-tag-attributes slot))))))
		   ((meta::get-object-func slot)
		    `((:tr :class "dvr") ((:td :class "dvch") ,user-name ,unit)
		      ((:td :class "dvcv")((:slot-pick-val ,(clos:slot-definition-name slot) :class "dvcved"
							   ,@(meta::html-tag-attributes slot))))))
		   ((meta::choices slot)
		    `((:tr :class "dvr") ((:td :class "dvch") ,user-name ,unit)
		      ((:td :class "dvcv") ((:slot-combo ,(clos:slot-definition-name slot)
							 ,@(meta::html-tag-attributes slot))))))
		   ((eq (meta::value-type slot) 'string)
		    `((:tr :class "dvr") ((:td :class "dvch") ,user-name ,unit)
		      ((:td :class "dvcv") ((,(if (eq (meta::view-type slot) :medit) :slot-medit :slot-edit)
					      ,(clos:slot-definition-name slot) :class "dvcve"
					      ,@(meta::html-tag-attributes slot))))))
		   ((eq (meta::value-type slot) :date)
		    `((:tr :class "dvr") ((:td :class "dvch") ,user-name)
		      ((:td :class "dvcv") ((:slot-date-edit ,(clos:slot-definition-name slot) :class "dvcve"
							     ,@(meta::html-tag-attributes slot))))))
		   ((eq (meta::value-type slot) :universal-time)
		    `((:tr :class "dvr") ((:td :class "dvch") ,user-name)
		      ((:td :class "dvcv") ((:slot-date-edit ,(clos:slot-definition-name slot)
							     :show-time t :class "dvcve"
							     ,@(meta::html-tag-attributes slot))))))
		   ((eq (meta::value-type slot) 'boolean)
		    `((:tr :class "dvr") ((:td :class "dvch") ,user-name)
		      ((:td :class "dvcv") ((:slot-check-box ,(clos:slot-definition-name slot))))))
		   (t `((:tr :class "dvr") ((:td :class "dvch") ,user-name ,unit)
			((:td :class "dvcv") ((:slot-edit ,(clos:slot-definition-name slot) :class "dvcve"
							  ,@(meta::html-tag-attributes slot)))))))))))))

(defun ensure-function (function class)
  (if (symbolp function)
    (find function (meta::effective-functions class) :key 'meta::name)
    function))

(defun make-std-object-functions-view (class &optional functions)
  (setf class (ensure-class class))
  (unless functions (setf functions (meta::effective-functions class)))
  (loop for function in functions
	do (setf function (ensure-function function class))
	when function
	collect `(:when (visible-p ,function)
		  :br ((:fn-link ,function) (:translate ,(meta::user-name function))))))

(defun make-std-object-view (class country-language user-group)
  (setf class (ensure-class class))
  (let* ((full-class-name (util:full-symbol-name (class-name class)))
	 (view-name (format nil "~a-std-~a-~a" full-class-name country-language user-group))
	 (view (find view-name (meta::direct-views class) :key 'name :test 'string=)))
    (if view
      view
      (make-instance 'object-view :object-class class :generated t
		     :name view-name :country-languages (list country-language)
		     :user-group (list user-group)
		     :source-code (nconc (make-std-object-slots-view class nil nil)
					 (make-std-object-functions-view class))))))

(defun clean-direct-views (class)
  (setf (meta::direct-views class)
	(remove-if 'generated (meta::direct-views class)))
  (dolist (view (meta::direct-views class))
    (setf (html-func view) nil)))

(defun clean-all-views ()
  (loop with classes = ()
	for view being the hash-values of *all-object-views*
	do (pushnew (object-class view) classes)
	finally (map nil 'clean-direct-views classes)))

(meta::add-finalization-class-hook 'clean-direct-views)

(defun find-best-view (class country-language user-groups)
  (let ((view nil))
    (loop for v in (meta::direct-views class)
	  do (when (and (not (special-view v))
			(find country-language (country-languages v))
			(intersection user-groups (user-groups v)))
	       (return-from find-best-view v)))
    (loop for v in (meta::direct-views class)
	  do (when (and (not (special-view v))
			(find country-language (country-languages v))
			(or (not (user-groups v))
			    (intersection user-groups (user-groups v))))
	       (return-from find-best-view v)))
    (make-std-object-view class country-language user-groups)))

(defun %process-view (name object)
  (setf name (or name (getf (session-params *request*) :view)))
  (let* ((*object* object)
	 (*user-groups* (dynamic-groups *user* object *user-groups*))
	 (view (when name (gethash name *all-object-views*))))
    (meta::load-object-data *object*)
    (when (and view (not (eq (class-of object) (object-class view))))
      (setf view nil))
    (unless view
      (setf view (find-best-view (class-of object) *country-language* *user-groups*)))
    (when view
      (setf view (get-view-instance view))
      (push (cons view object) *request-views*)
      (funcall (html-func view)))))

(defun object-view-tag (attributes form)
  (destructuring-bind (&key name object-tag object) form
    (when object-tag (setf object `(get-application-data *session* ,object-tag)))
    (unless object (setf object '*object*))
    `(%process-view ,name ,object)))

;syntax (:object-view :name "view-name" :object object) if no name, will choose view according to object language etc...)
(html:add-func-tag :object-view 'object-view-tag)

(defun connect-views-tag (attributes form)
  (html::html-gen `(:when *request-views* (:connect *request-views*))))

;syntax :connect-views
(html:add-func-tag :connect-views 'connect-views-tag)

;;;**** list-format ***************************
;;;

(defclass slot-list-format ()
  ((name   :initform ""  :accessor name :initarg :name)
   (object-class :initform nil  :accessor object-class :initarg :object-class)
   (special-format :initform nil  :accessor special-format :initarg :special-format)
   (country-languages  :initform nil :accessor country-languages :initarg :country-languages)
   (user-groups  :initform nil :accessor user-groups :initarg :user-groups)
   (list-format-fn :initform nil :accessor list-format-fn :initarg :list-format-fn)))

(defmethod initialize-instance :after ((format slot-list-format) &rest init-options
				       &key &allow-other-keys)
  (setf (gethash (name format) *all-list-formats*) format)
  (setf (object-class format)(when (object-class format) (ensure-class (object-class format)))))

(defun find-list-format (name)
  (gethash name *all-list-formats*))

(defun find-best-list-format (class country-language user-groups)
  (when class (setf class (ensure-class class)))
  (let ((formats (iterate (for (name format) in-hashtable *all-list-formats*) 
			  (when (eq class (object-class format)) (collect format)))))
    (loop for f in formats
	  do (when (and (not (special-format f))
			(find country-language (country-languages f))
			(intersection user-groups (user-groups f)))
	       (return-from find-best-list-format f)))
    (loop for f in formats
	  do (when (and (not (special-format f))
			(find country-language (country-languages f))
			(or (not (user-groups f))
			    (intersection user-groups (user-groups f))))
	       (return-from find-best-list-format f)))
    (find-list-format "default-format")))

(defun std-list-checkbox (index start-index)
  (when (modifiable-p *dispatcher*)
    (decf index start-index)
    (html::html ((:input :type "checkbox" 
			 :fformat (:name "~aC~d" (name (item *dispatcher*)) index)
			 :optional (:checked (and (find index (selected-objects-idx *dispatcher*)) "true")))))))

(defun std-list-format-fn (start objects max-nb total-length)
  (html:html
    ((:table :class (table-class (item *dispatcher*)))
     (loop repeat max-nb 
           for object in objects
   	   for index from start
   	   for index1 = (1+ index) do
	  (html:html
	    (:tr
	     ((:td :class "dvcv") index1)
	     (:when (modifiable-p *dispatcher*)
	       ((:td :class "dvcv") (std-list-checkbox index start)))
	     ((:td :class "dvcv")
	      ((:a :href (encode-object-url object))
	       (html:esc (meta:list-description object *object*))))))))))

(make-instance 'slot-list-format :name "default-format"
	       :header-fn #'(lambda (container-obj))
	       :list-format-fn 'std-list-format-fn)

