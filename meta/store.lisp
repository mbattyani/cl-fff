(in-package meta)

(defmacro with-store-lock ((store) &body body)
  `(bt:with-lock-held ((store-lock ,store))
     ,@body))

(defun silent-mark-object-as-modified (object)
  (unless (modified object)
    (with-store-lock ((object-store object))
      (setf (modified object) t)
      (when (object-store object)
        (push object (modified-objects (object-store object)))))))

(defmethod mark-object-as-modified (object)
  (silent-mark-object-as-modified object))

(defun create-data-object (object)
  (when (data-object object) (error "data object already created"))
  (setf (data-object object) (make-instance (data-class (class-of object))))
  (data-object object))

(defun create-proxy-object (id class-id &optional anonymous)
  (let ((class (find-meta-class (guid-to-id class-id))))
    (make-instance class :anonymous anonymous :force-id id :store *default-store*)))

#|
;object record format
;<object> ::= <type=object-tag(byte)><class-id(long)><id(long)><size(long)>{<slot>}*
;<object> ::= <type=anonymous-object-tag(byte)><class-id(long)><size(long)>{<slot>}*
;<slot> ::= <slot-name><type(byte)><value>
;<type> ::= void || boolean || long || double || string || symbol
;           || array || list || object || linked-object  || end-of-list

(defconstant +void-tag+ 0)
(defconstant +boolean-tag+ 1)
(defconstant +long-tag+ 2)
(defconstant +double-tag+ 3)
(defconstant +string-tag+ 4)
(defconstant +symbol-tag+ 5)
(defconstant +array-tag+ 6)
(defconstant +list-tag+ 7)
(defconstant +object-tag+ 8)
(defconstant +linked-object-tag+ 9)
(defconstant +anonymous-object-tag+ 10)
(defconstant +end-of-list-tag+ 11)
(defconstant +pathname-tag+ 12)
(defconstant +end-of-object-tag+ 13)

(defvar *ascii-stream* nil)

(defmacro write-tag-value (tag value)
  `(progn
    (write-string ,(format nil "~d " tag) *ascii-stream*)
    (prin1 ,value *ascii-stream*)
    (terpri *ascii-stream*)))

(defmacro write-tag (tag)
  `(progn
    (write-string ,(format nil "~d " tag) *ascii-stream*)))

(defun write-value-as-ascii (value &optional integer-as-id)
  (if value
      (etypecase value
	 (boolean (write-tag-value #.+boolean-tag+ value))
	 (integer (if integer-as-id
		    (write-tag-value #.+linked-object-tag+ value)
		    (write-tag-value #.+long-tag+ value)))
	 (float   (write-tag-value #.+double-tag+ value))
	 (string  (write-tag-value #.+string-tag+ value))
	 (symbol  (write-tag-value #.+symbol-tag+ value))
	 (pathname (write-tag-value #.+pathname-tag+ value))
	 (array   (let ((rank (array-rank value))
			(total-size (array-total-size value)))
		    (write-tag-value #.+array-tag+ rank)
		    (dotimes (d rank)
		      (prin1 (array-dimension value d) *ascii-stream*)
		      (write-char #\Space *ascii-stream*))
		    (dotimes (i total-size)
		      (write-value-as-ascii (row-major-aref value i))))) ;integer-as-id ?
	 (list    (write-tag #.+list-tag+)
		  (dolist (v value) (write-value-as-ascii v integer-as-id))
		  (write-tag #.+end-of-list-tag+))
	 (root-object (when (eq (id value) :anonymous)
                        (break))
                      (if (eq (id value) :anonymous)
                          (write-anonymous-fc-object-as-ascii value)
                          (write-tag-value #.+linked-object-tag+ (id value)))))
      (write-tag #.+void-tag+)))

(defun write-fc-object-slots-as-ascii (object)
  (let ((class (class-of object))
	(data-object (data-object object)))
    (dolist (slot (c2mop:class-slots class))
      (when (stored slot)
	(prin1 (c2mop:slot-definition-name slot) *ascii-stream*)
	(write-char #\Space *ascii-stream*)
	(write-value-as-ascii (slot-value data-object (c2mop:slot-definition-name slot)))))
    (write-tag #.+end-of-object-tag+)))

(defun write-fc-object-disabled-slots-as-ascii (object)
  (when (disabled-slots object)
    (maphash #'(lambda (key value)
		 (prin1 key *ascii-stream*)
		 (write-char #\Space *ascii-stream*)
		 (prin1 value *ascii-stream*)
		 (write-char #\Space *ascii-stream*))
	     (disabled-slots object)))
  (write-tag #.+end-of-object-tag+))

(defun write-fc-object-as-ascii (object)
  (let* ((*package* (find-package "COMMON-LISP-USER")))
    (prin1 (version class) *ascii-stream*)
    (write-char #\Space *ascii-stream*)
    (write-fc-object-slots-as-ascii object)
    (write-fc-object-disabled-slots-as-ascii object)
    (setf (modified object) nil)))

(defun write-anonymous-fc-object-as-ascii (object)
  (let ((class (class-of object))
	(data-object (data-object object)))
    (write-tag #.+anonymous-object-tag+)
    (format *ascii-stream* "#x~x~%" (guid class))
    (write-fc-object-as-ascii object)))

(defun write-fc-object-to-ascii-file (object filename)
  (with-open-file (s filename :direction :output :if-exists :supersede)
		  (let ((*ascii-stream* s))
		    (write-fc-object-as-ascii object))))

(defun read-fc-object-from-ascii-file (filename &optional no-data)
  (with-open-file (s filename :direction :input)
		  (let ((*ascii-stream* s))
		    (read-value-as-ascii no-data))))

(defun read-value-as-ascii (&optional no-data)
  (let ((tag (read *ascii-stream*)))
    (case tag
	  (#.+void-tag+ nil)
	  (#.+boolean-tag+ (read *ascii-stream* ))
	  (#.+long-tag+    (read *ascii-stream* ))
	  (#.+double-tag+  (read *ascii-stream* ))
	  (#.+string-tag+  (read *ascii-stream* ))
	  (#.+symbol-tag+  (read *ascii-stream* ))
	  (#.+pathname-tag+  (read *ascii-stream* ))
	  (#.+array-tag+   (let ((rank (read *ascii-stream* ))
				 (dims ())
				 array)
			     (dotimes (d rank)
				      (push (read *ascii-stream* ) dims))
			     (setf array (make-array (nreverse dims)))
			     (dotimes (i (array-total-size array))
				      (setf (row-major-aref array i) (read-value-as-ascii)))))
	  (#.+list-tag+    (let ((values ())
				 (value (read-value-as-ascii)))
			     (loop while (not (eq value +end-of-list-tag+))
			       do  (push value values)
			       (setf value (read-value-as-ascii)))
			     (nreverse values)))
;	  (#.+object-tag+ (read-fc-object-as-ascii nil no-data))
	  (#.+anonymous-object-tag+ (read-anonymous-fc-object-as-ascii))
	  (#.+linked-object-tag+ (read-object-proxy-from-store *default-store* (read *ascii-stream*)))
	  (#.+end-of-list-tag+ +end-of-list-tag+))))

(defun read-fc-object-slots-as-ascii (object)
  (let* ((data-object (data-object object))
	 (class (class-of data-object)))
    (unless data-object (setf data-object (create-data-object object)))
    (setf (modified object) nil)
    (loop for slot-name = (read *ascii-stream*)
	  until (eql slot-name +end-of-object-tag+)
	  for slot-value = (read-value-as-ascii)
	  when (find slot-name (c2mop:class-slots class) :key 'c2mop:slot-definition-name)
	  do (setf (slot-value data-object slot-name) slot-value)))
  object)

(defun read-fc-object-disabled-slots-as-ascii (object)
  (loop with disabled-slots = nil
	for slot-name = (read *ascii-stream*)
	until (eql slot-name +end-of-object-tag+)
	for disable-value = (read *ascii-stream*)
	do (unless disabled-slots
	     (setf disabled-slots (make-hash-table :test #'eql))
	     (setf (disabled-slots object) disabled-slots))
	(setf (gethash slot-name disabled-slots) disable-value)))

(defun initialize-unbound-slots (object)
  (let ((data-object (data-object object)))
    (dolist (slot (c2mop:class-slots (class-of object)))
      (when (and (not (in-proxy slot))(not (slot-boundp data-object (c2mop:slot-definition-name slot)))(slot-definition-initfunction slot))
	(setf (slot-value data-object (c2mop:slot-definition-name slot)) (funcall (slot-definition-initfunction slot)))))))
(defun read-fc-object-as-ascii (object)
  (setf (modified object) nil)
  (let* ((*parent-object* object) ;parent for anonymous object, see semantic notes
	 (*package* (find-package "COMMON-LISP-USER"))
	 (version (read *ascii-stream*)))
    (read-fc-object-slots-as-ascii object)
    (read-fc-object-disabled-slots-as-ascii object)
    (initialize-unbound-slots object)
    (when (/= version (version (class-of object)))
      (update-object-version object version))))

(defun read-anonymous-fc-object-as-ascii ()
  (let* ((class-id (read *ascii-stream*))
	 (version (read *ascii-stream*))
	 (object (create-proxy-object 0 class-id t)))
    (setf (slot-value object 'parent) *parent-object*)
    (read-fc-object-as-ascii object)))

(defmethod update-object-version ((object root-object) version)
  object)

;;;********************************************************************************

(export 'set-stored-slot-value)
(defun set-stored-slot-value (obj slot value)
  (if (used-by-rules slot)
      (irules::change-slot-value obj slot value)
    (set-stored-location-value obj (slot-definition-location slot) value)))

(defun set-stored-location-value (obj location value)
  (let ((data (data-object obj)))
;    (when (numberp state)
;	  (setf value (/ (round (* state value)) state)))
    (unless (eql value (%location-value data location))
	    (setf (%location-value data location) value)
	    (unless (= location +recipients-slot-location+)
		    (mark-object-as-modified obj)
					;	    (setf (%location-value data +modified-slot-location+) t) ;modified = t
		    (announce obj `(:slot-changed ,location))
		    (announce obj :object-changed)))))

|#

;;;********************************************************************************

(defvar *parent-object* nil)

(defvar *ascii-store-slot-readers* (make-hash-table :test #'equal))

(defun add-ascii-store-slot-reader (name reader-fn)
  (setf (gethash name *ascii-store-slot-readers*) reader-fn))

(export 'load-object)
(defun load-object (id &optional (store *default-store*))
  (read-object-from-store store id))

(defun delete-object (object)
  (delete-object-from-store (object-store object) object))

(defun load-object-data (object)
  (let ((data-object (data-object object)))
    (if data-object
        data-object
        (progn (read-object-data-from-store (object-store object) object)
               (initialize-unbound-slots object)
               (initialize-disable-predicates object)
               (data-object object)))))

(export 'reload-object)
(defun reload-object (id &optional (store *default-store*))
  (let ((found-object (gethash id (loaded-objects store))))
    (when found-object
      (setf (data-object found-object) nil
            (disabled-slots found-object) nil)
      (read-object-data-from-store store found-object)))
  (load-object id store))

(export 'save-object)
(defun save-object (object)
  (unless (anonymous-object-p object)
    (when (and (modified object)
               (data-object object))
      (save-object-to-store (object-store object) object)
      (setf (modified object) nil))))

;;;********************************************************************************
;;; Store
;;;********************************************************************************

(defvar *store-id* 0)
(defvar *stores* (make-hash-table :test #'eql))

(defun find-store (store-id)
  (gethash store-id *stores*))

(defun make-weak-hashtable ()
  #+sbcl
  (make-hash-table :test #'eql :weakness :value)
  #+lispworks
  (make-hash-table :test #'eql :weak-kind :value)
  #-(or sbcl lispworks)
  (error "implement weak-hashtable"))

(defclass store ()
  ((loaded-objects :initform (make-weak-hashtable) :accessor loaded-objects)
   (modified-objects :initform () :accessor modified-objects)
   (store-id :initform 0 :accessor store-id)
   (store-lock :accessor store-lock)))

(defmethod save-modified-objects (store)
  (with-store-lock (store)
    (dolist (object (modified-objects store))
      (save-object-to-store store object))
    (setf (modified-objects store) ())))


(defmethod initialize-instance :after ((store store) &rest init-options &key &allow-other-keys)
  (declare (ignore init-options))
  (setf (store-id store) (incf *store-id*))
  (setf (gethash (store-id store) *stores*) store)
  (setf (store-lock store) (bt:make-lock (format nil "store-~d-lock" (store-id store))))
  ;(hcl:set-hash-table-weak (loaded-objects store) :value) ;;CONFIRM - moved to defclass.
  )

;;;********************************************************************************
;;; Ascii Store
;;;********************************************************************************

(defclass ascii-store (store)
  ((file-directory :initform #P""  :accessor file-directory :initarg :file-directory)
   (next-id-file-path :accessor next-id-file-path)
   (next-id :initform 10000 :accessor next-id :initarg :next-id)))

(defmethod initialize-instance :after ((store ascii-store) &rest init-options &key &allow-other-keys)
  (declare (ignore init-options))
  ;(hcl:set-hash-table-weak (loaded-objects store) :value)
  (setf (next-id-file-path store) (merge-pathnames (file-directory store) "next-id.store"))
  (open-store store))

(defmethod open-store ((store ascii-store))
  (ensure-directories-exist (file-directory store))
  (setf (next-id store) (ignore-errors (with-open-file (s (next-id-file-path store) :direction :input)
                                         (read s nil nil))))
  (unless (next-id store)
    (setf (next-id store) 10000)
    (with-open-file (s (next-id-file-path store) :direction :output :if-exists :supersede)
      (format s "~D~%" (next-id store)))))

(defmethod initialize-store ((store ascii-store))
  (open-store store)
   )

(defmethod create-new-object-id ((store ascii-store) class-id)
  (incf (next-id store))
  (with-open-file (s (next-id-file-path store) :direction :output :if-exists :supersede)
    (format s "~D~%" (next-id store)))
  (next-id store))

(defmethod convert-slot-value-to-sexpr (value)
  value)

(defmethod convert-slot-value-to-sexpr ((value list))
  (mapcar 'convert-slot-value-to-sexpr value))

(defmethod convert-slot-value-to-sexpr ((obj root-object))
  (if (zerop (id obj))
      (cons :obj (convert-object-to-sexpr obj))
      (list :obj-id (id obj) (guid (class-of obj)))))

(defun convert-object-to-sexpr (object)
  (let* ((class (class-of object))
	 (data-object (load-object-data object)))
    (list :guid (guid class) :version (version class)
          :parent (convert-slot-value-to-sexpr (parent object)) :slots
          (loop for slot in (c2mop:class-slots class)
             when (stored slot)
             collect (list (c2mop:slot-definition-name slot)
                           (convert-slot-value-to-sexpr
                            (slot-value data-object (c2mop:slot-definition-name slot))))))))

(defmethod save-object-to-store ((store ascii-store) object)
  (let ((filename (merge-pathnames (file-directory store) (format nil "~D.fco" (id object))))
        (*package* (find-package "COMMON-LISP-USER"))
        (sexpr-string (format nil "~s~%" (convert-object-to-sexpr object))))
    (loop for i from 0
       for close-paren = nil then (char= c #\))
       for c across sexpr-string
       do (when (and close-paren (char= c #\space))
            (setf (aref sexpr-string i) #\linefeed)))
    (with-open-file (s filename :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string sexpr-string s))
    (setf (modified object) nil)))

(defmethod convert-sexpr-to-slot-value (value)
  value)

(defmethod convert-sexpr-to-slot-value ((value list))
  (case (first value)
    (:obj-id (let* ((id (second value))
                    (found (gethash id (loaded-objects *default-store*))))
               (if found
                   found
                   (create-proxy-object id (third value)))))
    (:obj (let ((object (create-proxy-object 0 (getf (second sexpr) :guid))))
            (init-object-from-sexpr object sexpr)
            object))
    (:reader (let ((reader-fn (gethash (second value) *ascii-store-slot-readers*)))
               (if reader-fn (funcall reader-fn value)
                   (error "ascii-store-slot-reader not found for ~s" value))))
    (t (mapcar 'convert-sexpr-to-slot-value value))))

(defun init-object-from-sexpr (object sexpr)
  (let* ((data-object (create-data-object object))
	 (class (class-of data-object))
         (parent-id (getf sexpr :parent)))
    (setf (modified object) nil)
    (when parent-id
      (setf (parent object) (convert-sexpr-to-slot-value parent-id)))
    (loop with *parent-object* = object
          for (slot-name value) in (getf sexpr :slots)
	  for slot-value = (convert-sexpr-to-slot-value value)
	  when (find slot-name (c2mop:class-slots class) :key 'c2mop:slot-definition-name)
       do (setf (slot-value data-object slot-name) slot-value)))
  (initialize-unbound-slots object)
  (initialize-disable-predicates object)
  object)

(defmethod read-object-proxy-from-store ((store ascii-store) id)
  (read-object-from-store store id))

(defmethod read-object-data-from-store ((store ascii-store) object)
  (let ((filename (merge-pathnames (file-directory store) (format nil "~D.fco" (id object))))
	(*read-eval* nil)
        (*default-store* store)
        (*package* (find-package "COMMON-LISP-USER")))
    (with-open-file (s filename :direction :input :external-format :utf-8)
      (init-object-from-sexpr object (read s nil nil)))))

(defmethod read-object-from-store ((store ascii-store) id)
  (let ((found (gethash id (loaded-objects store))))
    (if found
      found
      (let ((filename (merge-pathnames (file-directory store) (format nil "~D.fco" id)))
	    (*read-eval* nil)
	    (*default-store* store)
            (*package* (find-package "COMMON-LISP-USER"))
            (sexpr nil)
	    (object nil))
	(with-open-file (s filename :direction :input :external-format :utf-8)
	  (setf sexpr (read s nil nil))
	  (setf object (create-proxy-object id (getf sexpr :guid)))
	  (init-object-from-sexpr object sexpr))))))

(defmethod load-named-object ((store ascii-store) name)
  (let ((filename (merge-pathnames (file-directory store) (format nil "~d.named" (sxhash name))))
	id)
    (ignore-errors
      (with-open-file (s filename :direction :input)
	(setf id (read s nil nil))
	(when (numberp id)
	  (load-object id store))))))

(defmethod register-named-object ((store ascii-store) object name)
  (let ((filename (merge-pathnames (file-directory store) (format nil "~d.named" (sxhash name)))))
    (with-open-file (s filename :direction :output :if-exists :supersede)
      (format s "~D~%" (id object)))))

(defmethod load-or-create-named-object ((store ascii-store) name class-id)
  (let ((object (load-named-object store name)))
    (unless object
      (setf object (make-instance (find-meta-class class-id) :store store))
      (register-named-object store object name))
    object))

(defmethod delete-object-from-store ((store ascii-store) object)
  (remhash (id object) (loaded-objects store))
  )

(defmethod close-store ((store ascii-store))
  )

(defmethod update-object-parent-in-store (object store)
  (mark-object-as-modified object))

;;;********************************************************************************
;;; Void  Store
;;;********************************************************************************

(defclass void-store (store)
  ((named-objects :initform (make-hash-table :test #'eql) :accessor named-objects)
   (next-id :initform 10000 :accessor next-id :initarg :next-id)))

(defmethod initialize-instance :after ((store void-store) &rest init-options &key &allow-other-keys)
  (declare (ignore init-options))
  ;(hcl:set-hash-table-weak (loaded-objects store) :value)
  (open-store store))

(defmethod open-store ((store void-store))
    )

(defmethod initialize-store ((store void-store))
  (open-store store)
   )

(defmethod create-new-object-id ((store void-store) class-id)
  (incf (next-id store))
  (dpb class-id (byte 32 32) (next-id store)))

(defmethod save-object-to-store ((store void-store) object)
  )

(defmethod read-object-proxy-from-store ((store void-store) id)
  )

(defmethod read-object-data-from-store ((store void-store) object)
  )

(defmethod read-object-from-store ((store void-store) id)
  (gethash id (loaded-objects store)))

(defmethod load-named-object ((store void-store) name)
  (gethash name (named-objects store)))

(defmethod register-named-object ((store void-store) object name)
  (setf (gethash name (named-objects store)) object))

(defmethod load-or-create-named-object ((store void-store) name class-id)
  (let ((object (load-named-object store name)))
    (unless object
      (setf object (make-instance (find-meta-class class-id) :store store))
      (register-named-object store object name))
    object))

(defmethod delete-object-from-store ((store void-store) object)
  (remhash (id object) (loaded-objects store)))

(defmethod clear-store ((store void-store))
  (clrhash (loaded-objects store)))

(defmethod close-store ((store void-store))
  )

;;;*****************************************************
;;; moving one store content to another storere
;;; this is a destructive operation

(defun move-objects-to-store (src-store dst-store)
  (with-store-lock (src-store)
    (with-store-lock (dst-store)
       (maphash
        #'(lambda (key obj)
            (declare (ignore key))
            (let ((id (create-new-object-id dst-store (guid (class-of obj)))))
              (load-object-data obj)
              (setf (id obj) id
                    (modified obj) t
                    (object-store obj) dst-store
                    (gethash id (loaded-objects dst-store)) obj)
              (push obj (modified-objects dst-store))))
        (loaded-objects src-store))
      (setf (loaded-objects src-store) nil
            (modified-objects src-store) nil))))
