(in-package interface)

(defclass dispatcher ()
  ((interface :accessor interface :initarg :interface)
   (item :accessor item :initarg :item)
   (item-state  :accessor item-state :initform nil)
   (item-state2 :accessor item-state2 :initform nil)))

(defclass object-dispatcher (dispatcher)
  ((object :accessor object :initarg :object)
   (disabled  :accessor disabled :initform nil)
   (dirty-status :accessor dirty-status :initform nil)))

(defclass slot-dispatcher (object-dispatcher)
  ((object :accessor object :initarg :object)
   (modifiable-p :accessor modifiable-p :initform nil)
   (slot  :accessor slot :initform nil)
   (get-value-fn :accessor get-value-fn :initform nil)
   (set-value-fn :accessor set-value-fn :initform nil)
   (dirty-value  :accessor dirty-value :initform t)))

(defmethod initialize-instance :after ((dispatcher slot-dispatcher) &rest init-options &key &allow-other-keys)
  (let* ((item (item dispatcher))
	 (object (object dispatcher))
	 (slot (slot item))
	 (*object* object))
    (setf (slot dispatcher) slot)
    (setf (modifiable-p dispatcher)(modifiable-p item))
    (setf (get-value-fn dispatcher) (fdefinition (meta::accessor slot)))
    (setf (set-value-fn dispatcher) (fdefinition (list 'setf (meta::accessor slot))))
    (push (list slot
		#'(lambda (action value)
		    (case action
		      (:value-changed  (mark-dirty-value dispatcher))
		      (:status-changed (mark-dirty-status dispatcher value))))
		dispatcher)
	  (meta::listeners object))
    (setf (disabled dispatcher) (meta::slot-disabled-p object slot))))

(defmethod mark-dirty-value ((dispatcher slot-dispatcher))
  (unless (or (dirty-value dispatcher)(dirty-status dispatcher))
    (push dispatcher (dirty-dispatchers (interface dispatcher))))
    (setf (dirty-value dispatcher) t))

(defmethod mark-dirty-status ((dispatcher slot-dispatcher) disabled)
  (unless (or (dirty-value dispatcher)(dirty-status dispatcher))
    (push dispatcher (dirty-dispatchers (interface dispatcher))))
  (setf (dirty-status dispatcher) t)
  (setf (disabled dispatcher) disabled))

(defmethod mark-dirty-status ((dispatcher object-dispatcher) disabled)
  (unless (dirty-status dispatcher)
    (push dispatcher (dirty-dispatchers (interface dispatcher))))
  (setf (dirty-status dispatcher) t)
  (setf (disabled dispatcher) disabled))

(defmethod update-dispatcher-item ((dispatcher object-dispatcher) &optional force)
  (let* ((*dispatcher* dispatcher)
	 (*object* (object dispatcher))
	 (item (item dispatcher))
	 (interface (interface dispatcher)))
    (when (or force (dirty-status dispatcher))
      (send-to-interface
       (make-set-status-javascript item (disabled dispatcher) nil) interface)
      (setf (dirty-status dispatcher) nil))))

(defmethod update-dispatcher-item ((dispatcher slot-dispatcher) &optional force)
  (let* ((*dispatcher* dispatcher)
	 (*object* (object dispatcher))
	 (item (item dispatcher))
	 (interface (interface dispatcher))
	 (slot (slot dispatcher)))
    (when (or force (dirty-status dispatcher))
      (send-to-interface
       (make-set-status-javascript item (disabled dispatcher) slot) interface)
      (setf (dirty-status dispatcher) nil))
    (when (or force (dirty-value dispatcher))
      (send-to-interface
       (make-set-value-javascript item (funcall (get-value-fn dispatcher) *object*) slot) interface)
      (setf (dirty-value dispatcher) nil))))

(defmethod unregister-dispatcher (dispatcher)
  )

(defmethod unregister-dispatcher ((dispatcher object-dispatcher))
  (let ((object (object dispatcher)))
    (setf (meta::listeners object) (remove dispatcher (meta::listeners object) :key #'third))))

(defmethod safely-convert-string-to-value (dispatcher value)
  (meta::safe-string-to-value value (slot dispatcher)))

(defmethod try-change-slot :around ((dispatcher slot-dispatcher) value)
  (let ((*dispatcher* dispatcher)
	(effective-value (call-next-method)))
    (unless (equal value effective-value)
      (send-to-interface (make-set-value-javascript (item dispatcher) effective-value (slot dispatcher))
			 (interface dispatcher)))))

(defmethod try-change-slot ((dispatcher slot-dispatcher) value)
  (funcall (set-value-fn dispatcher) value (object dispatcher)))

(defmethod fire-action (dispatcher value click-str)
  (let* ((*dispatcher* dispatcher)
	 (*object* (object dispatcher))
	 (function (action-func (item dispatcher))))
    (when function (funcall function *object* value click-str))))

(defmethod fire-add-to-list ((dispatcher slot-dispatcher) value)
  (let ((object (object dispatcher)))
    (when (meta::process-new-object-fn (slot dispatcher))
      (setf value (funcall (meta::process-new-object-fn (slot dispatcher)) value object)))
    (funcall (set-value-fn dispatcher) (cons value (funcall (get-value-fn dispatcher) object)) object)))

(defclass button-group-dispatcher (slot-dispatcher)
  ())

(defmethod try-change-slot ((dispatcher button-group-dispatcher) value)
  (funcall (set-value-fn dispatcher) (value (find (util::atoi value) (sub-items (item dispatcher))
						  :key #'(lambda(item)
							   (when (typep item 'checked-button)
							     (index item)))))
	   (object dispatcher)))

(defmethod make-dispatcher (interface object item)
  (if (slot item)
    (make-instance 'slot-dispatcher :interface interface :object object :item item)
    (make-instance 'object-dispatcher :interface interface :object object :item item)))

;(defmethod make-dispatcher (interface object (item edit))
;  (make-instance 'dispatcher :interface interface :object object :item item))

(defmethod make-dispatcher (interface object (item button-group))
  (make-instance 'button-group-dispatcher :interface interface :object object :item item))

