(in-package #:meta)

(export '*country-language*)
(defvar *country-language* :en)

(export '*country*)
(defvar *country* :en)

(defclass translated-string ()
  ((english  :initarg :en  :accessor english :initform "")
   (french   :initarg :fr  :accessor french :initform "")
   (german   :initarg :de  :accessor german :initform "")
   (spanish  :initarg :sp  :accessor spanish :initform "")
   (italian  :initarg :it  :accessor italian :initform "")))

(defun translated-string-reader (stream subchar arg)
  (declare (ignore arg subchar))
  (let ((first-char (peek-char nil stream t nil t)))
    (cond ((char= first-char #\space)
	   (read-char stream)	  ; skip over whitespaceitalian
	   (translated-string-reader stream nil nil))
	  ((char= first-char #\() ;read a list
	   (list* 'make-instance ''meta:translated-string (read stream t nil t)))
	  (t
	   (error "Bad translated-string expression")))))

(set-dispatch-macro-character #\# #\L 'translated-string-reader)

(defun check-string (string)
  (and string (> (length string) 0) string))

(defmethod translate ((tr-string t) &key default (country-lang *country-language*))
  (declare (ignore country-lang))
  (or (check-string tr-string) (translate default) ""))

(defmethod translate ((tr-string string) &key default (country-lang *country-language*))
  (declare (ignore country-lang))
  (or (check-string tr-string) (translate default) ""))

(defmethod translate ((tr-string list) &key default (country-lang *country-language*))
  (if (null tr-string)
      (if (not default)
	  ""
	  (translate default))
      (or (getf tr-string country-lang)
	  (translate default)
	  (getf tr-string :en)
	  (second tr-string)
	  "")))

(defmethod translate ((tr-string translated-string) &key default (country-lang *country-language*))
  (let ((string (case country-lang
		  (:fr (french tr-string))
		  (:en (english tr-string))
		  (:de (german tr-string))
		  (:sp (spanish tr-string))
		  (:it (italian tr-string)))))
    (or (check-string string) (check-string (translate default))
	(check-string (english tr-string))(check-string (french tr-string))
	(check-string (german tr-string)))))

(defun translated-name (obj)
  (translate (user-name obj)))

(defun translated-class-name (obj)
  (translate (user-name (class-of obj))))

(defvar *default-void-link-text*
  (make-instance 'translated-string
		 :en "not defined" :fr "non défini" :sp "No definido"))

(defun translated-void-link-text (slot)
  (let ((translation (translate (void-link-text slot))))
    (if translation
      translation
      (translate *default-void-link-text*))))

(defun translate-reader (stream subchar arg)
  (declare (ignore arg subchar))
  (let ((first-char (peek-char nil stream t nil t)))
    (cond ((char= first-char #\space)
	   (read-char stream)	  ; skip over whitespace
	   (translate-reader stream nil nil))
	  ((char= first-char #\") ;read one string
	   `(meta:translate ,(read stream t nil t)))
	  ((char= first-char #\() ;read a list
	   `(meta:translate ',(read stream t nil t)))
	  (t
	   (error "Translate expression starts with ~A only \" or ( are accepted" first-char)))))

(set-dispatch-macro-character #\# #\T #'translate-reader)

(defclass object-help ()
  ((english-tooltip :initarg :en   :accessor english-tooltip)
   (english-help    :initarg :en-h :accessor english-help)
   (french-tooltip  :initarg :fr   :accessor french-tooltip)
   (french-help     :initarg :fr-h :accessor french-help)
   (german-tooltip  :initarg :de   :accessor german-tooltip)
   (german-help     :initarg :de-h :accessor german-help)
   (spanish-tooltip :initarg :sp   :accessor spanish-tooltip)
   (spanish-help    :initarg :sp-h :accessor spanish-help)
   (italian-tooltip :initarg :it   :accessor italian-tooltip)
   (italian-help    :initarg :it-h :accessor italian-help)
   ))

(defmethod tooltip ((help t) &optional (country-lang *country-language*))
  (declare (ignore country-lang))
   "")

(defmethod tooltip ((help string) &optional (country-lang *country-language*))
  (declare (ignore country-lang))
   help)

(defmethod tooltip ((slot fc-slot-definition-mixin) &optional (country-lang *country-language*))
  (declare (ignore country-lang))
  (tooltip (object-help slot)))

(defmethod tooltip ((help object-help) &optional (country-lang *country-language*))
  (let ((string
	 (case country-lang
	   (:fr (french-tooltip help))
	   (:en (english-tooltip help))
	   (:de (german-tooltip help))
	   (:sp (spanish-tooltip help))
	   (:it (italian-tooltip help)))))
    (or (check-string string)(check-string (english-tooltip help))(check-string (french-tooltip help)))))

(defmethod help ((help t) &optional (country-lang *country-language*))
  (declare (ignore country-lang))
   "")

(defmethod help ((help string) &optional (country-lang *country-language*))
  (declare (ignore country-lang))
   help)

(defmethod help ((slot fc-slot-definition-mixin) &optional (country-lang *country-language*))
  (declare (ignore country-lang))
  (help (object-help slot)))

(defmethod help ((help object-help) &optional (country-lang *country-language*))
    (case country-lang
      (:fr (french-help help))
      (:en (english-help help))
      (:de (german-help help))
      (:sp (spanish-help help))
      (:it (italian-help help))))

(defclass fc-function ()
  ((name        :initarg :name :accessor name)
   (user-name   :initarg :user-name :accessor user-name)
   (visible        :type boolean :initarg :visible :initform nil :accessor visible)
   (visible-groups :initarg :visible-groups :initform nil :accessor visible-groups)
   (html-tag-attributes :initform nil :accessor html-tag-attributes :initarg :html-tag-attributes)
   (get-value-html-fn :initform nil :accessor get-value-html-fn :initarg :get-value-html-fn)
   (get-value-title :initform nil :accessor get-value-title :initarg :get-value-title)
   (get-value-text :initform nil :accessor get-value-text :initarg :get-value-text)
   (get-value-sql :initform nil :accessor get-value-sql :initarg :get-value-sql)
   (get-object-func :initform nil :accessor get-object-func :initarg :get-object-func)
   (disable-predicate :initarg :disable-predicate :initform nil :accessor disable-predicate)
   (disable-predicate-fn :initform nil :accessor disable-predicate-fn)
   (object-help :initarg :object-help :accessor object-help)))

(defclass triggered-function ()
  ((name :initarg :name :accessor name)
   (trigger-on-value-change :initform nil :accessor trigger-on-value-change
			    :initarg :trigger-on-value-change)
   (trigger-on-status-change :initform nil :accessor trigger-on-status-change
			     :initarg :trigger-on-status-change)))

(export 'short-description)
(defmethod short-description (obj)
  (format nil "~A" obj))

(defmethod short-description ((obj root-object))
  (format nil "~A ~A" (translated-class-name obj) (id obj)))

(defvar *undefined-short-desc*
  (make-instance 'translated-string
		 :en "(no description)" :fr "(pas de description)" :sp "(no descripción)"))

(defmethod short-description :around ((obj root-object))
  (let ((desc nil)
        (got-error t))
    (ignore-errors
      (setf desc (call-next-method))
      (setf got-error nil))
    (if (and (stringp desc) (> (length desc) 0))
      desc
      (if got-error
          "(error)"
          (translate *undefined-short-desc*)))))

(export 'long-description)
(defmethod long-description (obj)
  (short-description obj))

(export 'list-description)
(defmethod list-description (obj container-obj)
  (short-description obj))

(defmethod cl::print-object ((object root-object) stream)
  (format stream "<FC-Object ~A ~A (~A)>"
	  (class-name (class-of object))
	  (id object)
	  (ignore-errors (short-description object))))
#+nil (if *print-redably*
      (write-string (short-description object) stream)
      (format stream "<FC-Object ~A ~A>"
	      (class-name (class-of object))
	      (id object)))
