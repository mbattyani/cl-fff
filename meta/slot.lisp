(in-package meta)

(eval-when (:load-toplevel :compile-toplevel :execute)
(defparameter +slot-initargs+ '(:user-name :description :object-help :stored :in-proxy 
			       :indexed :unique :null-allowed :vaccessor :initarg
			       :initform :choices :list-of-values :value-type :linked-value
			       :modifiable :modifiable-groups :visible :visible-groups :sql-name
			       :unit :disable-predicate :new-objects-first
			       :value-constraint :sql-length :nb-decimals
			       :void-link-text :pathname-filter
			       :make-copy-string :duplicate-value-fn
			       :get-value-html-fn :get-value-title :get-value-text :get-value-sql
			       :value-to-string-fn :string-to-value-fn :value-to-sql-fn :sql-to-value-fn
			       :can-create-new-object :create-new-object :get-object-func :process-new-object-fn
			       :dont-display-null-value :view-type :duplicate-value
			       :html-tag-attributes)))

;; ------------------------------------------------------------------------
;; Mixin metaclass for stored slots
(clos:defclass fc-slot-definition-mixin ()
  ((user-name     :initform ""  :accessor user-name :initarg :user-name)
   (description   :type string :initarg :description :initform "" :accessor description)
   (object-help   :initarg :object-help :initform "" :accessor object-help)
   (stored        :type boolean :initarg :stored :initform t :accessor stored)
   (in-proxy      :initform nil :accessor in-proxy :initarg :in-proxy)
   (indexed       :type boolean :initarg :indexed :initform nil :accessor indexed)
   (unique        :type boolean :initarg :unique :initform nil :accessor unique)
   (null-allowed  :type boolean :initarg :null-allowed :initform t :accessor null-allowed)
   (accessor      :initarg :vaccessor :initform nil :accessor vaccessor :accessor accessor)
   (initarg       :initarg :initarg :initform nil :accessor initarg)
   (initform      :initarg :initform :initform nil :accessor initform)
   (choices       :type list :initarg :choices :initform nil :accessor choices)
   (list-of-values :type symbol :initarg :list-of-values :initform nil :accessor list-of-values)
   (value-type    :type symbol :initarg :value-type :initform t :accessor value-type)
   (linked-value  :type t :initarg :linked-value :initform nil :accessor linked-value) ;t nil :anonymous
   (modifiable     :type boolean :initarg :modifiable :initform nil :accessor modifiable)
   (modifiable-groups :initarg :modifiable-groups :initform nil :accessor modifiable-groups)
   (visible        :type boolean :initarg :visible :initform nil :accessor visible)
   (visible-groups :initarg :visible-groups :initform nil :accessor visible-groups)
   (sql-name      :type string :initarg :sql-name :initform nil :accessor sql-name)
   (unit          :initform nil :accessor unit :initarg :unit)
   (duplicate-value  :type boolean :initarg :duplicate-value :initform t :accessor duplicate-value)
   (make-copy-string :type boolean :initform nil :accessor make-copy-string :initarg :make-copy-string)
   (duplicate-value-fn :initform nil :accessor duplicate-value-fn :initarg :duplicate-value-fn)
   (html-tag-attributes :initform nil :accessor html-tag-attributes :initarg :html-tag-attributes)
   (get-value-html-fn :initform nil :accessor get-value-html-fn :initarg :get-value-html-fn)
   (get-value-title :initform nil :accessor get-value-title :initarg :get-value-title)
   (get-value-text :initform nil :accessor get-value-text :initarg :get-value-text)
   (get-value-sql :initform nil :accessor get-value-sql :initarg :get-value-sql)
   (disable-predicate :type t :initarg :disable-predicate :initform nil :accessor disable-predicate)
   (disable-predicate-fn :initform nil :accessor disable-predicate-fn)
   (value-constraint :initform nil :accessor value-constraint :initarg :value-constraint)
   (value-constraint-fn :initform nil :accessor value-constraint-fn)
   (value-to-string-fn :initform nil :type symbol :accessor value-to-string-fn :initarg :value-to-string-fn)
   (string-to-value-fn :initform nil :type symbol :accessor string-to-value-fn :initarg :string-to-value-fn)
   (value-to-sql-fn :initform nil :type t :accessor value-to-sql-fn :initarg :value-to-sql-fn)
   (sql-to-value-fn :initform nil :type t :accessor sql-to-value-fn :initarg :sql-to-value-fn)
   (sql-length    :initform nil :accessor sql-length :initarg :sql-length)
   (nb-decimals   :initform nil :accessor nb-decimals :initarg :nb-decimals)
   (used-by-rules :initform nil :type boolean :accessor used-by-rules)
   (used-by-predicates      :initform nil :accessor used-by-predicates)
   (void-link-text          :initform nil :accessor void-link-text
			    :initarg :void-link-text)
   (pathname-filter         :initform nil :accessor pathname-filter :initarg :pathname-filter)
   (create-new-object       :initform nil :accessor create-new-object :initarg :create-new-object)
   (can-create-new-object   :initform nil :accessor can-create-new-object :initarg :can-create-new-object)
   (get-object-func         :initform nil :accessor get-object-func :initarg :get-object-func)
   (process-new-object-fn   :initform nil :accessor process-new-object-fn :initarg :process-new-object-fn)
   (new-objects-first       :initform t   :accessor new-objects-first :initarg :new-objects-first)
   (dont-display-null-value :initform nil :accessor dont-display-null-value :initarg :dont-display-null-value)
   (view-type               :initform nil :accessor view-type :initarg :view-type)
   ))

(defmethod initialize-instance :after ((slot fc-slot-definition-mixin) &rest init-options &key &allow-other-keys)
  (unless (sql-name slot)
    (setf (sql-name slot) (convert-name-to-sql-name (slot-definition-name slot))))
  (when (stored slot)
    (unless (sql-to-value-fn slot)
      (setf (sql-to-value-fn slot) (default-sql-to-value-fn (value-type slot) slot)))
    (unless (value-to-sql-fn slot)
      (setf (value-to-sql-fn slot) (default-value-to-sql-fn (value-type slot) slot))))
  (unless (string-to-value-fn slot)
    (setf (string-to-value-fn slot) (default-string-to-value-fn (value-type slot) slot)))
  (unless (value-to-string-fn slot)
    (setf (value-to-string-fn slot) (default-value-to-string-fn (value-type slot) slot)))
  )

(export 'fc-slot-p)
(defmacro fc-slot-p (obj)
  `(typep ,obj 'meta::fc-slot-definition-mixin))

(defun get-choices (slot object)
  (let ((choices (choices slot)))
    (if (and choices (or (symbolp choices)(functionp choices)))
      (funcall choices object)
      choices)))

(clos:defclass stored-slot-definition (standard-slot-definition fc-slot-definition-mixin)
  ())

;; ------------------------------------------------------------------------
;; Class of direct stored slots and methods to construct them when
;; appropriate.

(clos:defclass stored-direct-slot-definition (standard-direct-slot-definition stored-slot-definition)
  ())

;; Called when the class is being made, to choose the metaclass of a
;; given direct slot.  It should return the class of slot definition
;; required.
#-(or lispworks4.3 lispworks4.4)
(defmethod direct-slot-definition-class ((slotd fc-class) stored-specification)
  (find-class 'stored-direct-slot-definition)) ;; Use stored-direct-slot-definition

#+(or lispworks4.3 lispworks4.4)
(defmethod direct-slot-definition-class ((slotd fc-class) &rest initargs)
  (find-class 'stored-direct-slot-definition))

(defmethod slot-definition-allocation ((slot stored-slot-definition))
  (if (in-proxy slot)
    (call-next-method)
    :data-object))

(defmethod (setf slot-definition-allocation) (allocation (slot stored-slot-definition))
  (call-next-method))

;; Called when the defclass is expanded, to process a slot option.  It
;; should return the new list of slot options, based on
;; already-processed-options.
(defmethod process-a-slot-option ((class fc-class) option value already-processed-options slot)
  ;; Handle the :function option by adding it to the list of processed options.
					;  (if (eq option :function)
					;      (list* :function value already-processed-options)
  (if (member option +slot-initargs+)
      (list* option value already-processed-options)
    (call-next-method)))

;; ------------------------------------------------------------------------
;; Class of effective stored slots and methods to construct them when appropriate.
(clos:defclass stored-effective-slot-definition (standard-effective-slot-definition stored-slot-definition)
  ())

;; Called then the class is being finalized, to choose the metaclass
;; of a given effective slot.  It should return the class of slot definition required.
#-(or lispworks4.3 lispworks4.4)
(defmethod clos:effective-slot-definition-class ((class fc-class) direct-slot-definitions)
  (find-class 'stored-effective-slot-definition)) ;; Use stored-effective-slot-definition

#+(or lispworks4.3 lispworks4.4)
(defmethod clos:effective-slot-definition-class ((class fc-class) &rest initargs)
  (find-class 'stored-effective-slot-definition))

;; Called then the class is being finalized, to compute the initargs
;; used to construct the effective slot.  It should return the list of initargs.
;; WARNING CLOS PORTABILITY ?
;(defmethod clos::compute-effective-slot-definition-initargs ((class fc-class) name direct-slot-definitions)
;  (macrolet ((copy-slots (initargs)
;	       (cons 'list (loop for initarg in initargs
;				 nconc `(,initarg (,(intern (symbol-name initarg)) direct-slot))))))
;    (let ((direct-slot (first direct-slot-definitions)))
;      (append (copy-slots #.+slot-initargs+)(call-next-method)))))

(defmethod clos::compute-effective-slot-definition-initargs ((class fc-class) name direct-slot-definitions)
  (let ((direct-slot (first direct-slot-definitions)))
    (append #.(cons 'list (loop for initarg in +slot-initargs+
				nconc `(,initarg (,(intern (symbol-name initarg)) direct-slot))))
	    (call-next-method))))

;; ------------------------------------------------------------------------

(defmethod slot-value-using-class ((class fc-class) object slot-name)
  (let* ((slot (find slot-name (class-slots class) :key 'slot-definition-name))
	 (value (if (in-proxy slot)
		    (call-next-method)
		      (let ((data (data-object object)))
			(unless data (setf data (load-object-data object)))
			(slot-value data slot-name)))))
    (if (eq (value-type slot) :decimal)
	(float (/ value (expt 10 (nb-decimals slot))))
	value)))

(defmethod (setf slot-value-using-class) (value (class fc-class) object slot-name)
  (let ((slot (find slot-name (class-slots class) :key 'slot-definition-name)))
    (when (eq (value-type slot) :decimal)(setf value (fround (* value (expt 10 (nb-decimals slot))))))
    (if (in-proxy slot)
	(call-next-method)
	(let ((data (data-object object)))
	  (unless data (setf data (load-object-data object)))
	  (setf (slot-value data slot-name) value)))
      (when (eq (value-type slot) :decimal)(setf value (float (/ value (expt 10 (nb-decimals slot))))))
      value))

(defmethod slot-boundp-using-class ((class fc-class) object slot-name)
  (let ((slot (find slot-name (class-slots class) :key 'slot-definition-name)))
    (if (in-proxy slot)
      (call-next-method)
      (or (not (data-object object))(slot-boundp (data-object object) slot-name))))) ;data not loaded <=> slot is bound because in store

(defmethod slot-makunbound-using-class ((class fc-class) object slot-name)
  (let ((slot (find slot-name (class-slots class) :key 'slot-definition-name)))
    (when (in-proxy slot) (call-next-method))))


;;;********************************************************************************

(defun slot-disabled-p (object slot) ;; slot is a symbol
  (unless (data-object object)(load-object-data object))
  (find slot (disabled-slots object)))

(defun recompute-disable-predicate (object slot)
  (unless (data-object object)(load-object-data object))
  (when (symbolp slot)
    (setf slot (find slot (class-slots (class-of object)) :key 'slot-definition-name)))
  (when (disable-predicate-fn slot)
    (funcall (disable-predicate-fn slot) object)))

;--------------------------------------------------------------------------
; listeners format: (slot-name lambda(new-value) lambda(new-state))

(defun fire-slot-value-changed (obj slot new-value)
  (dolist (predicate (used-by-predicates slot))
    (funcall predicate obj))
  (dolist (listener (listeners obj))
    (when (eq slot (first listener))
      (funcall (second listener) :value-changed new-value))))

#+ignore ;;old version
(defun fire-slot-value-changed (obj slot new-value)
  (dolist (predicate (used-by-predicates slot))
    (multiple-value-bind (slot disabled)(funcall predicate obj)
      (when slot (fire-slot-state-changed obj slot disabled))))
  (dolist (listener (listeners obj))
    (when (eq slot (first listener)) (funcall (second listener) :value-changed new-value))))

(defun fire-slot-state-changed (obj slot disabled)
  (dolist (listener (listeners obj))
    (when (eq slot (first listener))
      (funcall (second listener) :status-changed disabled))))

;--------------------------------------------------------------------------
;;;------------------------------------------------
(defun std-value-to-string (value slot)
  (format nil "~a" value))

(defun %string-to-float% (string)
  (let* ((*read-eval* nil)
	 (first-digit (position-if #'(lambda (c)
				       (find c "0123456789+-.,"))
				   string))
	 (last-digit (position-if-not #'(lambda (c)
					  (find c "0123456789+-.,"))
				      string
				      :start (if first-digit first-digit 0))))
    (float (read-from-string (nsubstitute #\. #\, (subseq string first-digit last-digit))))))

(defun find-value-from-choice (choice-nb choices)
  (let* ((choice-nb (parse-integer choice-nb :junk-allowed t))
	 (choice (and choice-nb (elt choices choice-nb))))
    (if choice
      (first choice)
      :choice-not-found)))

(defvar *names-for-true* '("true" "1" "yes" "oui" "vrai" "t"))

(defun parse-date (string)
  (let* ((pos1 (position #\/ string))
	 (pos2 (when pos1 (position #\/ string :start (1+ pos1))))
	 (pos3 (position #\Space string))
	 (pos4 (when pos3 (position #\: string :start pos3)))
	 (pos5 (when pos4 (position #\: string :start (1+ pos4)))))
    (when (and pos1 pos2)
      (let ((n1 (parse-integer string :start 0 :end pos1 :junk-allowed t))
	    (n2 (parse-integer string :start (1+ pos1) :end pos2 :junk-allowed t))
	    (n3 (parse-integer string :start (1+ pos2) :junk-allowed t))
	    (h (or (and pos3 (parse-integer string :start (1+ pos3) :junk-allowed t)) 0))
	    (m (or (and pos4 (parse-integer string :start (1+ pos4) :junk-allowed t)) 0))
	    (s (or (and pos5 (parse-integer string :start (1+ pos5) :junk-allowed t)) 0)))
	(when (< n3 100) (incf n3 2000))
	(unless (and n1 n2 n3 (<= 1 n2 12) (<= 0 h 23) (<= 0 m 59) (<= 0 s 59))
	  (error "bad date : ~s" string))
	(encode-universal-time s m h n1 n2 n3)))))

;**** MUST be provided by the interface!
;(defun decode-object-id (string)
;  )
    
(defun std-string-to-value (string slot)
  (let* ((*read-eval* nil)
	 (type (value-type slot))
	 (unique-value (cons nil nil))
	 (value unique-value)
	 temp-value)
 ;   (ignore-errors
      (if (choices slot)
	(let ((slot-value (find-value-from-choice string (choices slot))))
	  (unless (eq slot-value :choice-not-found)
	    (setf value slot-value)))
	(cond
	  ((eq type :date) (setf value (parse-date string)))
	  ((fc-class-p type) (setf value (decode-object-id string)))
	  ((subtypep type 'string) (setf value string))
	  ((subtypep type 'float)  (setf temp-value (string-to-float string t))
	   (when (typep temp-value 'float)(setf value temp-value)))
	  ((subtypep type 'integer)(setf temp-value (parse-integer string :junk-allowed t))
	   (when (typep temp-value 'integer)(setf value temp-value)))
	  ((subtypep type 'boolean) (setf value (not (null (member string *names-for-true* :test #'string-equal)))))
	  (t (setf value (read-from-string string)))))
      (if (eq value unique-value)
	(values nil t)
	(values value nil))))

(defun setf-string-to-slot-value (object slot string)
  (when (symbolp slot)
    (setf slot (find slot (class-slots (class-of object)) :key 'slot-definition-name)))
  (when slot
    (multiple-value-bind (value ok)(safe-string-to-value string slot)
      (when ok
	(funcall (fdefinition (list 'setf (meta::accessor slot))) value object)))))

;;sql conversion functions. those functions take a value and a stream
(defun safe-subtypep (type1 type2)
  (let ((result nil))
    (ignore-errors
      (setf result (subtypep type1 type2)))
    result))

(defun boolean-to-sql (value stream)
  (write-string (if value "true" "false") stream))

(defun date-to-sql (value stream)
  (if value
    (multiple-value-bind (s mn h d m y) (decode-universal-time value)
      (format stream "'~4,'0d-~2,'0d-~2,'0d'" y m d))
    (write-string "null" stream)))

(defun utime-to-sql (value stream)
  (if value
    (multiple-value-bind (s mn h d m y) (decode-universal-time value)
      (format stream "'~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d'" y m d h mn s))
    (write-string "null" stream)))

(defun unknown-to-sql (value s)
  (if value
    (progn
      (setf value (write-to-string value))
      (write-char #\' s)
      (loop with length = (length value)
	    for start = 0 then (when stop (1+ stop))
	    while (and start (< start length))
	    for stop = (position #\' value :start start)
	    do (write-string value s :start start :end stop)
	    (when stop (write-string "''" s)))
      (write-char #\' s))
    (write-string "null" s)))

(defun string-to-sql (value s)
  (if value
    (progn
      (write-char #\' s)
      (loop with length = (length value)
	    for start = 0 then (when stop (1+ stop))
	    while (and start (< start length))
	    for stop = (position #\' value :start start)
	    do (write-string value s :start start :end stop)
	    (when stop (write-string "''" s)))
      (write-char #\' s))
    (write-string "null" s)))

(defun ip-to-sql (value s)
  (if (and value (not (zerop (length value))))
      (progn
	(write-char #\' s)
	(write-string value s)
	(write-char #\' s))
    (write-string "null" s)))

(defun symbol-to-sql (value s)
  (if value
    (progn
      (write-char #\' s)
      (write-string (symbol-name value) s)
      (write-char #\' s))
    (write-string "null" s)))

(defun number-to-sql (value s)
  (if value
    (prin1 value s)
    (write-string "null" s)))

(defun fc-object-to-sql (value s)
  (if value
    (prin1 (id value) s)
    (write-string "null" s)))

(defun decimal-to-sql (value s &optional (format "'~,2f'"))
  (if value
    (format s format value)
    (write-string "null" s)))

(defun default-value-to-sql-fn (type slot)
  (cond
    ((safe-subtypep type 'boolean) 'boolean-to-sql)
    ((eq type :date) 'date-to-sql)
    ((eq type :universal-time) 'utime-to-sql)
    ((eq type :decimal) 'decimal-to-sql
        (let ((format (format nil "'~~,~df'" (nb-decimals slot))))
	  (lambda (value stream)
	    (decimal-to-sql value stream format))))
    ((eq type :ip-address) 'ip-to-sql)
    ((safe-subtypep type 'string) 'string-to-sql)
    ((eq type 'symbol) 'symbol-to-sql)
    ((safe-subtypep type 'number) 'number-to-sql)
    ((fc-class-p type) 'fc-object-to-sql)
    ((eq type t) 'unknown-to-sql)
    (t (format t "Warning unknown type : ~a assuming fc-object~%" type) 'fc-object-to-sql)
    ))

(defun parse-iso-date (string)
  (when string
    (let ((length (length string)))
      (encode-universal-time (if (>= length 19) (parse-integer string :start 17 :end 19) 0)
			     (if (>= length 16) (parse-integer string :start 14 :end 16) 0)
			     (if (>= length 13) (parse-integer string :start 11 :end 13) 0)
			     (parse-integer string :start 8 :end 10)
			     (parse-integer string :start 5 :end 7)
			     (parse-integer string :start 0 :end 4)))))

(defun parse-sql-boolean (value)
  (when value
    (char-equal (aref value 0) #\t)))

(defun parse-sql-symbol (value)
  (when value
    (intern value :keyword)))

(defun parse-sql-obj (value)
  (when value
    (read-object-proxy-from-store *default-store* value)))

(defun parse-sql-unknown (value)
  (when value
    (if (stringp value)
	(read-from-string value)
	value)))

(defun parse-sql-ip (value)
  (if value value ""))

(defun default-sql-to-value-fn (type slot)
  (cond
    ((safe-subtypep type 'boolean) 'parse-sql-boolean)
    ((eq type :date) 'parse-iso-date)
    ((eq type :universal-time) 'parse-iso-date)
    ((eq type :ip-address) 'parse-sql-ip)
    ((safe-subtypep type 'string) 'identity)
    ((eq type 'symbol) 'parse-sql-symbol)
    ((safe-subtypep type 'number) 'identity)
    ((fc-class-p type) 'parse-sql-obj)
    ((eq type t) 'parse-sql-unknown)
    (t ;(format t "Error unkown type : ~a assuming fc-object~%" type)
       'parse-sql-obj)))

;;string conversion functions. those functions take a value and a stream

(defun boolean-to-string (value stream)
  (write-string (if value "true" "false") stream))

(defun date-to-string (value stream)
  (if value
    (utime-to-string-date value stream)
    (write-string "null" stream)))

(defun utime-to-string (value stream)
  (if value
    (utime-to-string-time value stream)
    (write-string "null" stream)))

(defun string-to-string (value s)
  (if value
    (progn
      (write-char #\' s)
      (loop with length = (length value)
	    for start = 0 then (when stop (1+ stop))
	    while (and start (< start length))
	    for stop = (position #\' value :start start)
	    do (write-string value s :start start :end stop)
	    (when stop (write-string "''" s)))
      (write-char #\' s))
    (write-string "null" s)))

(defun symbol-to-string (value s)
  (if value
    (progn
      (write-char #\' s)
      (write-string (symbol-name value) s)
      (write-char #\' s))
    (write-string "null" s)))

(defun number-to-string (value s)
  (if value
    (prin1 value s)
    (write-string "null" s)))

(defun fc-object-to-string (value s)
  (if value
    (prin1 (id value) s)
    (write-string "null" s)))

(defun decimal-to-string (value s &optional (format "'~,2f'"))
  (if value
    (format s format value)
    (write-string "null" s)))

(defun translated-choice-value (slot obj &optional (country *country-language*))
  (let* ((slot (if (symbolp slot)
		   (find slot (class-slots (class-of obj)) :key 'slot-definition-name)
		   slot))
	 (value (when slot (funcall (fdefinition (accessor slot)) obj)))
	 (choice (find value (meta::choices slot) :key #'first :test #'equal)))
    (when choice
      (translate (second choice)))))

(defun default-value-to-string-fn (type slot)
  (cond
    ((safe-subtypep type 'boolean) #'(lambda(value stream)
				  (write-string (if value "true" "false") stream)))
    ((eq type :date) #'(lambda (value stream)
			 (if value
			   (utime-to-string-date value stream)
			   (write-string "null" stream))))
    ((eq type :universal-time) #'(lambda (value stream)
			 (if value
			   (utime-to-string-time value stream)
			   (write-string "null" stream))))
    ((safe-subtypep type 'string) #'(lambda (value s)
				 (if value
				   (progn
				     (write-char #\' s)
				     (loop with length = (length value)
					   for start = 0 then (when stop (1+ stop))
					   while (and start (< start length))
					   for stop = (position #\' value :start start)
					   do (write-string value s :start start :end stop)
					   (when stop (write-string "''" s)))
				     (write-char #\' s))
				   (write-string "null" s))))
    ((eq type 'symbol) #'(lambda (value s)
			   (if value
			     (progn
			       (write-char #\' s)
			       (write-string (symbol-name value) s)
			       (write-char #\' s))
			     (write-string "null" s))))
    ((safe-subtypep type 'number) #'(lambda (value s)
				      (if value
					(prin1 value s)
					(write-string "null" s))))
    ((fc-class-p type) #'(lambda (value s)
			   (if value
			     (prin1 (id value) s)
			     (write-string "null" s))))
    ((eq type t) #'(lambda (value s)
		     (if value
		       (progn
			 (write-char #\' s)
			 (prin1 value s)
			 (write-char #\' s))
		       (write-string "null" s))))
    (t ;(format t "Error unkown type : ~a assuming fc-object~%" type)
     #'(lambda (value s)
	 (if value
	   (prin1 (id value) s)
	   (write-string "null" s))))))

;;; String to Value

(defun string-choice-to-value (string choices null-allowed)
  (if (and null-allowed (string-equal string "nil"))
    nil
    (find-value-from-choice string choices)))

(defun string-to-boolean (string null-allowed)
  (not (null (member string *names-for-true* :test #'string-equal))))

(defun string-to-date (string null-allowed)
  (if (and null-allowed (or (string-equal string "nil")(string-equal string "")))
    nil
    (parse-date string)))

(defun string-to-utime (string null-allowed)
  (if (and null-allowed (or (string-equal string "nil")(string-equal string "")))
    nil
    (parse-date string)))

(defun string-to-ip (string null-allowed)
  (if (and null-allowed (or (string-equal string "nil")(zerop (length string))))
    ""
    string))

(defun string-to-string (string null-allowed)
  (if (and null-allowed (string-equal string "nil"))
    nil
    string))

(defun string-to-symbol (string null-allowed)
  (if (and null-allowed (string-equal string "nil"))
    nil
    (intern string :keyword)))

(defun string-to-integer (string null-allowed)
  (if (and null-allowed (or (string-equal string "nil")(string-equal string "")))
    nil
    (let ((integer (parse-integer string :junk-allowed t)))
      (if integer
	integer
	(error "~s is not an integer" string)))))

(defun string-to-float (string &optional null-allowed)
  (if (and null-allowed (string-equal string "nil"))
    nil
    (let* ((*read-eval* nil)
	   (first-digit (position-if #'(lambda (c)
					 (find c "0123456789+-.,"))
				     string))
	   (last-digit (position-if-not #'(lambda (c)
					    (find c "0123456789+-.,"))
					string
					:start (if first-digit first-digit 0))))
      (float (read-from-string (nsubstitute #\. #\, (subseq string first-digit last-digit)))))))

(defun string-to-number (string null-allowed)
  (if (and null-allowed (string-equal string "nil"))
    nil
    (let* ((*read-eval* nil)
	   (value (read-from-string (substitute #\. #\, string))))
      (if (numberp value)
	value
	(error "~s is not a number" value)))))

(defun string-to-fc-object (string null-allowed)
  (if (and null-allowed (string-equal string "nil"))
    nil
    (decode-object-id string)))

(defun default-string-to-value-fn (type slot)
  (let ((choices (choices slot))
	(null-allowed (null-allowed slot)))
    (cond
      ((choices slot) #'(lambda (string)
			  (string-choice-to-value string choices null-allowed)))
      ((safe-subtypep type 'boolean) #'(lambda (string)
					 (string-to-boolean string null-allowed)))
      ((eq type :date) #'(lambda (string)
			   (string-to-date string null-allowed)))
      ((eq type :universal-time) #'(lambda (string)
				     (string-to-utime string null-allowed)))
      ((eq type :decimal) #'(lambda (string)
			      (string-to-decimal string null-allowed)))
      ((eq type :ip-address) #'(lambda (string)
					(string-to-ip string null-allowed)))
      ((safe-subtypep type 'string) #'(lambda (string)
					(string-to-string string null-allowed)))
      ((safe-subtypep type 'float) #'(lambda (string)
				       (string-to-float string null-allowed)))
      ((eq type 'symbol) #'(lambda (string)
			     (string-to-symbol string null-allowed)))
      ((eq type 'integer) #'(lambda (string)
			     (string-to-integer string null-allowed)))
      ((eq type 'number) #'(lambda (string)
			     (string-to-number string null-allowed)))
      ((fc-class-p type) #'(lambda (string)
			     (string-to-fc-object string null-allowed)))
      (t ;(format t "Error unkown type : ~a assuming fc-object~%" type)
       #'(lambda (string)
	   (string-to-fc-object string null-allowed)))
      )))

(defun safe-string-to-value (string slot)
  (let ((ok nil)
	(value nil))
    (ignore-errors
      (setf value (funcall (string-to-value-fn slot) string)
	    ok t))
    (values value ok)))

