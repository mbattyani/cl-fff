(in-package meta)

;release 7.0

(defvar *default-store* nil)
(defvar *memory-store* nil)
(defvar *duplication-root* t)

(export 'root-object)
(defclass root-object ()
  ((id :accessor id :in-proxy t :stored nil :visible nil :duplicate-value nil) ;0 for anonymous objects
   (data-object  :accessor data-object :in-proxy t :visible nil :stored nil :duplicate-value nil)
   (object-store :accessor object-store :in-proxy t :visible nil :stored nil :duplicate-value nil)
   (new-object   :accessor new-object :in-proxy t :visible nil :stored nil :initform t :duplicate-value nil)
   (modified     :accessor modified :in-proxy t :visible nil :stored nil :duplicate-value nil)
   (listeners    :accessor listeners :in-proxy t :visible nil :stored nil :duplicate-value nil)
   (parent       :accessor parent :initform nil :in-proxy t :visible nil :stored nil :duplicate-value nil)
   (disabled-slots :accessor disabled-slots :initform nil :in-proxy t :visible nil :stored nil
		   :duplicate-value nil)
   (original-lists :accessor original-lists :initform nil :in-proxy t :visible nil :stored nil
		   :duplicate-value nil))
;   (frozen-version :initform nil :visible nil :duplicate-value nil)
;   (persistant-listeners :initform () :visible nil :duplicate-value nil))
   (:guid #xC47EBE30E23B11d3AB36000086398D33))


;instance creation cases :
;1 creating new object (option parent store)
; 1.1 named object (option anonymous nil)
; 1.2 anonymous object (option anonymous t)
;2 creating in-memory instance of existing object that can be anonymous (option force-id) => no data 

(defmethod initialize-instance :around ((object root-object) &rest init-options
					&key (parent *parent-of-root-object-initialized*)
					anonymous store force-id no-init-forms &allow-other-keys)
  (setf (data-object object) nil)
  (unless store
    (setf store (if parent (object-store parent) *default-store*)))
  (setf (object-store object) store)
  (setf (listeners object) nil)
  (setf (parent object) parent)
  (setf (disabled-slots object) nil)
  (setf (modified object) nil)
  (if force-id
    (setf (id object) (if anonymous 0 force-id)
	  (new-object object) nil
	  (original-lists object) ())
    (let ((*parent-of-root-object-initialized* object))
      (when (anonymous (class-of object))(setf anonymous t))
      (setf (new-object object) t)
      (if anonymous
	(setf (id object) 0)
	(let ((id (create-new-object-id store (id (class-of object)))))
	  (when (gethash id (loaded-objects store)) (error "Object is already there loaded"))
	  (setf (id object) id)))
      (setf (data-object object) (apply 'make-instance (data-class (class-of object)) (append init-options '(:allow-other-keys t))))
      (unless no-init-forms
	(call-next-method object)
	(initialize-disable-predicates object))
      (mark-object-as-modified object)))
  (unless anonymous (setf (gethash (id object) (loaded-objects store)) object))
  (hcl:flag-special-free-action object))

(defmethod update-instance-for-different-class :before ((old-object root-object) (new-object root-object)
						       &rest initargs &key &allow-other-keys)
  (when (slot-boundp old-object 'data-object)
    (load-object-data old-object)))
  
(defmethod update-instance-for-different-class :after ((old-object root-object) (new-object root-object)
						       &rest initargs &key &allow-other-keys)
  (when (and (slot-boundp instance 'data-object)(data-object new-object))
    (change-class (data-object new-object) (data-class (class-of new-object)))
    (initialize-unbound-slots new-object)
    (initialize-disable-predicates new-object)))

(defmethod update-instance-for-redefined-class :before ((instance root-object)
							added-slots discarded-slots property-list
							&rest initargs &key &allow-other-keys)
  (when (slot-boundp instance 'data-object)
    (load-object-data instance)))

(defmethod update-instance-for-redefined-class :after ((instance root-object)
						       added-slots discarded-slots property-list
						       &rest initargs &key &allow-other-keys)
  (when (slot-boundp instance 'data-object)
    (initialize-unbound-slots instance)
    (initialize-disable-predicates instance)))

(defun %free-object% (object)
  (when (typep object 'root-object)
;	(format t "~%free ~S ~S~%" (class-name (class-of object))(id object))
    (save-object object)))

(hcl:add-special-free-action '%free-object%)

(defmethod duplicate-object (object &key parent store)
  (load-object-data object)
  (let* ((class (class-of object))
	 (new-object (make-instance (class-of object) :no-init-forms t
				    :parent (or parent *parent-of-root-object-initialized* (parent object))
				    :store store))
	 (*parent-of-root-object-initialized* new-object)
	 (data-object (data-object object))
	 (new-data-object (data-object new-object)))
    (loop for slot in (class-slots (class-of object))
	  for slot-name = (slot-definition-name slot)
	  do
	  (when (duplicate-value slot)
	    (let ((value (if (in-proxy slot)
			     (slot-value object slot-name)
			     (slot-value data-object slot-name)))
		  new-value)
	      (if (duplicate-value-fn slot)
		  (setf new-value (funcall (duplicate-value-fn slot) object slot value))
		  (setf new-value (duplicate-slot-value object slot value)))
	      (if (in-proxy slot)
		  (setf (slot-value new-object slot-name) new-value)
		  (setf (slot-value new-data-object slot-name) new-value)))))
      (initialize-unbound-slots new-object)
      new-object))

(defmethod duplicate-slot-value (object slot value)
  (let ((*duplication-root* nil))
    (etypecase value
      (boolean value)
      (integer value)
      (pathname (merge-pathnames value ""))
      (float   value)
      (string  (if (and *duplication-root* (make-copy-string slot))
		   (format nil "copy (~d) of ~a" 1 value)
		   (copy-seq value)))
      (symbol  value)
      (array   (let ((new-array (make-array (array-dimensions value)))
		     (total-size (array-total-size value)))
		 (dotimes (i total-size)
		   (setf (row-major-aref new-array i)
			 (duplicate-slot-value object slot (row-major-aref value i))))
		 new-array))
      (list (mapcar #'(lambda (v) (duplicate-slot-value object slot v)) value))
      (root-object (if (linked-value slot)
		       value
		       (duplicate-object value))))))

(defmethod duplicate-object-for-paste (object &key parent store)
  (duplicate-object object :parent parent :store store))

