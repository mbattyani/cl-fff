(in-package #:meta)

;;;********************************************************************************
;;; Mongo Store
;;;********************************************************************************

(defclass mongo-store (store)
  ((database-name :initform "" :accessor database-name :initarg :database-name)
   (collection-name :initform "" :accessor collection-name :initarg :collection-name)
   (next-id :initform 10000 :accessor next-id :initarg :next-id)))

(defmethod initialize-instance :after ((store mongo-store) &rest init-options)
  #+lispworks
  (hcl:set-hash-table-weak (loaded-objects store) :value)
  (open-store store))

(defun get-next-mongo-id ()
  (cl-mongo:db.update "counters" (cl-mongo:kv "id" "users") (cl-mongo:$inc "seq" 1))
  (cl-mongo:get-element "seq" (caadr (cl-mongo:db.find "counters" (cl-mongo:kv "id" "users")))))

(defmethod open-store ((store mongo-store))
  (cl-mongo:db.use (database-name store))
  (let ((doc (cl-mongo:make-document)))
    (cl-mongo:add-element "id" "users" doc) ;; cant be _id, bug in CL-mongo
    (cl-mongo:add-element "seq" (next-id store) doc)
    (cl-mongo:db.insert "counters" doc)))

(defmethod initialize-store ((store mongo-store))
  (open-store store))

(defmethod create-new-object-id ((store mongo-store) class-id)
  (setf (next-id store) (get-next-mongo-id)))

;; save

(defmethod convert-slot-value-to-mexpr (value)
  value)

(defmethod convert-slot-value-to-mexpr ((value list))
  (mapcar #'convert-slot-value-to-mexpr value))

(defmethod convert-slot-value-to-mexpr ((object root-object))
  (list "OBJ-ID" (id object) (guid (class-of object))))

(defmethod convert-parent-slot-value-to-mexpr ((object null))
  nil)

(defgeneric find-mongo-document (store object-or-id)
  )

(defmethod find-mongo-document ((store mongo-store) (object t))
  (caadr (cl-mongo:db.find (collection-name store) (cl-mongo:kv "object-id" (id object)))))

(defmethod find-mongo-document ((store mongo-store) (id integer))
  (caadr (cl-mongo:db.find (collection-name store) (cl-mongo:kv "object-id" id))))

(defun create-or-modify-mongo-document (store object)
  (let ((class (class-of object))
        (data-object (load-object-data object))
        (document (or (find-mongo-document store object)
                      (cl-mongo:make-document))))
    (cl-mongo:add-element "object-id" (id object) document)
    (cl-mongo:add-element "guid" (guid class) document)
    (cl-mongo:add-element "version" (version class) document)
    (let (#+nil(parent (when (parent object) (id (parent object)))))
      (cl-mongo:add-element "parent" (convert-slot-value-to-mexpr (parent object)) document))
    (loop
       for slot in (class-slots class)
       when (stored slot)
       do
         (slot-definition-name slot)
         (cl-mongo:add-element (princ-to-string (slot-definition-name slot))
                               (convert-slot-value-to-mexpr
                                (slot-value data-object (slot-definition-name slot))) document))
    document))

(defmethod save-object-to-store ((store mongo-store) object) 
  (let ((document (create-or-modify-mongo-document store object))
        (*package* (find-package "COMMON-LISP-USER")))
    (cl-mongo:db.save (collection-name store) document)
    (setf (modified object) nil)))


;; load

(defmethod convert-mexpr-to-slot-value (value)
  value)

#+nil(defmethod convert-document-to-object ((document cl-mongo:document))
  (let ((object (create-proxy-object (cl-mongo:get-element "object-id" document) (cl-mongo:get-element "guid" document) t))) ;; t anonymous
    (init-object-from-document object document)))
#+nil(defmethod convert-mexpr-to-slot-value ((documents list))
  (mapcar #'convert-document-to-object documents))

(defmethod convert-mexpr-to-slot-value ((value list))
  (if (and (stringp (first value))
           (string= (first value) "OBJ-ID")) ;; it means we have parent
      (let* ((id (second value))
	     (found (gethash id (loaded-objects *default-store*))))
	(if found
	    found
	    (create-proxy-object id (third value))))
      (mapcar 'convert-mexpr-to-slot-value value)))

(defun init-object-from-document (object document)
  (let* ((data-object (create-data-object object))
	 (class (class-of data-object)))
    (setf (modified object) nil)
    (let ((parent (cl-mongo:get-element "parent" document)))
      (when parent
        (setf (parent object) (convert-mexpr-to-slot-value parent))))
    (loop
       with guid = (cl-mongo:get-element "guid" document)
       for slot-name in (cl-mongo:get-keys document)
       for value = (cl-mongo:get-element slot-name document)
       for slot-value = (convert-mexpr-to-slot-value value)
       when (string= slot-name "object-id")
       do (or (gethash slot-value (loaded-objects *default-store*))
              (create-proxy-object value guid))            
       do (let ((slot (find slot-name (class-slots class) :key (lambda (x) (princ-to-string (slot-definition-name x)))  :test #'equal)))
            (when slot
              (setf (slot-value data-object (slot-definition-name slot)) slot-value))
            (format t "~S => ~S~%" slot-name value)))
    (initialize-unbound-slots object)
    (initialize-disable-predicates object)
    object))

(defmethod read-object-proxy-from-store ((store mongo-store) id)
    (read-object-from-store store id))

(defmethod read-object-data-from-store ((store mongo-store) object)
  (let* ((*default-store* store)
         #+nil(filename (merge-pathnames (file-directory store) (format nil "~D.fco" (id object))))
         (document (find-mongo-document store object))
         (*read-eval* nil)
         (*package* (find-package "COMMON-LISP-USER")))
    (init-object-from-document object document)))

(defmethod read-object-from-store ((store mongo-store) id)
  (let ((found (gethash id (loaded-objects store))))
    (if found
        found
        (let* ((document (find-mongo-document store id))
               (*read-eval* nil)
               (*default-store* store)
               (*package* (find-package "COMMON-LISP-USER")))
          (assert document)
          (let ((object (create-proxy-object id (cl-mongo:get-element "guid" document))))
          (init-object-from-document object document))))))

(defmethod load-named-object ((store mongo-store) name)
    (print name)
    (let* ((document (caadr (cl-mongo:db.find "named-objects" (cl-mongo:kv "name" name))))
           (id (cl-mongo:get-element "object-id" document)))
      (when (numberp id)
        (load-object id store))))

(defmethod register-named-object ((store mongo-store) object name) 
  (let ((document (cl-mongo:make-document)))
    (cl-mongo:add-element "name" (sxhash name) document)
    (cl-mongo:add-element "object-id" (id object) document)
    (cl-mongo:db.insert "named-objects" document)))

(defmethod load-or-create-named-object ((store mongo-store) name class-id) ;; who calls this function?
  (let ((object (load-named-object store name)))
    (unless object
      (setf object (make-instance (find-meta-class class-id) :store store))
      (register-named-object store object name))
    object))

(defmethod delete-object-from-store ((store mongo-store) object)
  (remhash (id object) (loaded-objects store)))

(defmethod close-store ((store mongo-store))
  )

(defmethod update-object-parent-in-store (object store)
  (mark-object-as-modified object))

