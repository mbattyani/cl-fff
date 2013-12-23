(in-package #:meta)

(defmethod save-modified-objects ((store psql-store))
  (with-store-lock (store)
   (when (modified-objects store)
     (with-psql-store-db (store)
       (let ((*forced-psql-connection* postmodern:*database*)
             (modified-objects (modified-objects store))
             (current-object nil)
             (saved-so-far ())
             (created-so-far ()))
         (progn ;mp:without-interrupts
          (setf modified-objects (modified-objects store))
          (setf (modified-objects store) ()))
         (psql-with-transaction ()
           (sql-add-transaction-rollback-hook
            #'(lambda()
                (progn ;mp:without-interrupts
                 (when current-object
                   (pushnew current-object (broken-objects store))
                   (setf modified-objects (remove current-object modified-objects)))
                 (setf (modified-objects store) (append (modified-objects store) modified-objects)))
                (dolist (object saved-so-far)
                  (setf (modified object) t))
                (dolist (object created-so-far)
                  (setf (modified object) t
                        (new-object object) t))))
           (dolist (object modified-objects)
             (when (modified object)
               (setf current-object object)
               (save-object-to-store store object)
               (if (new-object object)
                   (push object created-so-far)
                 (push object saved-so-far))))))))))

(defmethod read-object-data-from-store ((store psql-store) object)
  (with-psql-store-db (store)
    (let* ((*default-store* store)
	   (sql-data (sql-query (gen-sql-read-object object)))
	   (result (cdar sql-data))
	   (parent-id (postmodern:coalesce (pop result)))
	   (class (class-of object))
	   (data-object (create-data-object object))
	   (id (id object)))
      (unless sql-data
	(utility:with-logged-errors (:ignore-errors t)
	  (error (with-output-to-string (s)
		   (format s "~%object not found ~a (~a)" id (class-name (class-of object)))
		   (loop for obj = (meta::parent object) then (meta::parent obj)
			 while obj do (format s " =>  ~a (~a, ~s)"
					      (meta::id obj)
                                              (ignore-errors (class-name (class-of obj)))
					      (ignore-errors (short-description obj))))
                   (when *preloaded-objects-stack*
                     (format s " load-path: ")
                     (loop for obj in (reverse *preloaded-objects-stack*)
                           do (format s " =>  ~a (~a, ~s)"
                                      (meta::id obj)
                                      (ignore-errors (class-name (class-of obj)))
                                      (ignore-errors (short-description obj)))))
                   (format s "~%")))))
      (when sql-data
	(when parent-id ;(and parent-id (not (eq parent-id :null)))
	  (setf (parent object) (read-object-from-store store parent-id)))
	(loop for slot in (class-slots class)
	      for sql-to-value-fn = (sql-to-value-fn slot)
	      for linked-value = (linked-value slot)
	      do
	      (when (stored slot)
		(if (list-of-values slot)
		    (let ((list nil))
		      (if (subtypep (value-type slot) 'root-object)
			  (sql-do-query
                              (child-id)
                               (:order-by
                                (:select 'id :from (ut:build-symbol  (sql-name class) '_ (sql-name slot)) :where (:= 'parentid (id object)))
                                (:desc 'ordernb))
                            (let ((proxy (read-object-proxy-from-store store child-id)))
                              (when proxy
                                (unless linked-value
                                  (setf (parent proxy) object))
                                (push proxy list))))
                            (sql-do-query
                                 (value)
                                 (:order-by
                                   (:select 'value :from (ut:build-symbol (sql-name class) '_ (sql-name slot)) :where (:= 'parentid (id object)))
                                   (:desc 'ordernb))
                                (push (funcall sql-to-value-fn (postmodern:coalesce value)) list)))
		      (push (cons slot (copy-list list))(original-lists object))
		      (setf (slot-value data-object (slot-definition-name slot)) list))
		    (setf (slot-value data-object (slot-definition-name slot))
			  (funcall sql-to-value-fn (postmodern:coalesce (pop result)))))))
        (initialize-unbound-slots object)
	(initialize-disable-predicates object)))))
