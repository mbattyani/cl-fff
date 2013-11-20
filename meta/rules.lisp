(in-package #:meta)

(defun find-used-slots (expr class)
  (let ((slots (c2mop:class-slots class))
	(used-slots ()))
    (labels
	((map-tree (expr)
	   (cond
	     ((symbolp expr)(let ((slot (find expr slots :key 'c2mop:slot-definition-name)))
			      (when slot (pushnew slot used-slots))))
	     ((listp expr) (dolist (item expr) (map-tree item))))))
      (map-tree expr))
    used-slots))

(defmacro with-slot-values (slots instance &body body)
  `(let ,(mapcar #'(lambda (slot)
		     `(,(c2mop:slot-definition-name slot) (,(accessor slot) ,instance)))
                 slots)
     ,@body))

(defun compile-predicate (slot class)
  (when (disable-predicate slot)
    (let* ((*package* (symbol-package (class-name class)))
	   (used-slots (find-used-slots (disable-predicate slot) class))
	   (object (intern "OBJECT"))
	   (disabled (gensym "disabled"))
	   (predicate
	    (compile nil `(lambda (,object)
                            (with-slot-values ,used-slots ,object
                              (let ((,disabled (and ,(disable-predicate slot) t)))
                                (unless (eq ,disabled (and (find ,slot (disabled-slots ,object)) t))
                                  (if ,disabled
                                      (push ,slot (disabled-slots ,object))
                                      (setf (disabled-slots ,object) (delete ,slot (disabled-slots ,object))))
                                  (fire-slot-state-changed ,object ,slot ,disabled)
                                  (values ,slot ,disabled))))))))
      (setf (disable-predicate-fn slot) predicate)
      (loop for used-slot in used-slots do
           (push predicate (used-by-predicates used-slot))))))

#+ignore
(defun compile-function-predicate (function class)
  (when (disable-predicate function)
    (let* ((*package* (symbol-package (class-name class)))
	   (used-slots (find-used-slots (disable-predicate function) class))
	   (object (intern "OBJECT"))
	   (disabled (gensym "disabled"))
	   (predicate
	    (compile nil `(lambda (,object)
			   (with-slot-values ,used-slots ,object
			     (let ((,disabled (and ,(disable-predicate function) t)))
			       (unless (eq ,disabled (and (find ,function (disabled-slots ,object)) t))
				 (if ,disabled
				   (push ,function (disabled-slots ,object))
				   (setf (disabled-slots ,object) (delete ,function (disabled-slots ,object))))
				 (fire-slot-state-changed ,object ,function ,disabled)
				 (values ,slot ,disabled))))))))
      (setf (disable-predicate-fn slot) predicate)
      (loop for used-slot in used-slots do
	    (push predicate (used-by-predicates used-slot))))))

(defvar *slot-values-context* nil)
(defvar %no-slot-value% (cons nil nil))

(defmacro with-slot-values-in-context (slots instance &body body)
  `(let ,(mapcar #'(lambda (slot)
		     `(,(c2mop:slot-definition-name slot)
                        (let ((value (getf *slot-values-context* ,slot %no-slot-value%)))
                          (if (eq value %no-slot-value%)
                              (,(accessor slot) ,instance)
                              value))))
                 slots)
     ,@body))

(defun compile-value-constraint (slot class)
  (when (value-constraint slot)
    (let* ((*package* (symbol-package (class-name class)))
	   (used-slots (find-used-slots (value-constraint slot) class))
	   (object (intern "OBJECT"))
	   (value (intern "VALUE"))
           ;; (disabled (gensym "disabled"))
	   (value-constraint `(lambda (,object ,value)
                                (with-slot-values-in-context ,used-slots ,object
                                  ,(value-constraint slot)))))
      (setf value-constraint (compile nil value-constraint))
      (setf (value-constraint-fn slot) value-constraint)
      #+nil(loop for used-slot in used-slots do
                (push predicate (used-by-predicates used-slot))))))

