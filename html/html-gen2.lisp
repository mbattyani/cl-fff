(in-package html)

(defun join-strings (forms string result)
  (let* ((form (first forms))
	 (new-string (or (and (eq (first form) 'write-string)(stringp (second form))
			      (eq (third form) '*html-stream*)(second form))
			 (and (eq (first form) 'write-char)(typep (second form) 'base-char)
			      (eq (third form) '*html-stream*)(make-string 1 :initial-element (second form))))))
    (setf forms (rest forms))
    (if string
      (if (not new-string)
	(progn (push (list 'write-string string '*html-stream*) result)
	       (push form result))
	(setf string (concatenate 'string string new-string)))
      (when new-string
	(setf string new-string)))
    (when (and (null forms) new-string)
      (push (list 'write-string string '*html-stream*) result))
    (when (and form (not new-string)(not string))
      (push form result))
    (cond
      ((not forms)(nreverse result))
      ((not new-string)(join-strings forms nil result))
      (t (join-strings forms string result)))))

(defun %optimize-progn (forms)
  (let ((flat-forms (mapcan #'(lambda (form)
				(cond ((eq (first form) 'progn) (rest form))
				      ((eq (first form) 'optimize-progn) (%optimize-progn (rest form)))
				      (t (list form))))
			    forms)))
    (join-strings flat-forms nil nil)))

(defmacro optimize-progn (&body forms)
  `(progn ,@(%optimize-progn forms)))

(defmacro html (&body forms)
  `(optimize-progn ,@(mapcar 'html-gen forms)))

(defmacro ffmt (format-string &rest args)
  `(fast-format *html-stream* ,format-string ,@args))

(defmacro fmt (format-string &rest args)
  `(format *html-stream* ,format-string ,@args))

(defmacro esc (string)
  (if (stringp string)
    `(write-string ,(quote-string string) *html-stream*)
    `(write-string-quoting-specials ,string)))

(defvar *func-tags-table* (make-hash-table)) ; lambda(attributes forms) => compiled forms

(defun add-func-tag (tag macro)
  (setf (gethash tag *func-tags-table*) macro))

(defun func-tag (tag)
  (gethash tag *func-tags-table*))

(defvar *empty-table* (make-hash-table))

(defun empty-tag-p (tag)
  (values (gethash tag *empty-table*)))

(defmacro define-empty-tags (&rest tags)
  `(loop for tag in ',tags
    do (setf (gethash tag *empty-table*) tag)
    finally (return ',tags)))

(define-empty-tags :br :hr :crlf :input :link :img)

(defmethod html-gen ((form cons))
   (let ((elt (first form)))
     (if (or (keywordp elt)(stringp elt)
	     (and (consp elt)
		  (or (keywordp (first elt))(stringp (first elt)))))
       (let ((tag (if (consp elt) (first elt) elt))
	     (attributes (if (consp elt) (rest elt) nil)))
	 (cond
	   ((empty-tag-p tag)
	    (when (rest form) (warn "Ignoring body of empty tag ~S" tag))
	    (open-tag tag attributes))
	   ((func-tag tag)
	    (funcall (func-tag tag) attributes (rest form)))
	   (t `(optimize-progn
		,(open-tag tag attributes)
		,@(mapcar #'(lambda (e) (html-gen e))(rest form))
		,(close-tag tag)))))
       form)))

(defmethod html-gen ((form symbol))
 (when form
   (if (keywordp form)
     (cond
       ((empty-tag-p form) (open-tag form nil))
       ((func-tag form)(funcall (func-tag form) nil nil))
       (t `(optimize-progn
	    ,(open-tag form nil)
	    ,(close-tag form))))
     `(princ ,form *html-stream*))))

(defmethod html-gen ((form string))
 `(write-string ,form *html-stream*))

(defmethod html-gen ((form function))
 `(funcall ,form))

(defmethod html-gen (form) 
  `(princ ,form *html-stream*))

(defmethod open-tag ((tag symbol) (attributes (eql nil)))
 `(write-string ,(format nil "<~A>" (substitute #\: #\. (string tag))) *html-stream*))

(defmethod open-tag ((tag string) (attributes (eql nil)))
 `(write-string ,(format nil "<~A>" (substitute #\: #\. (string tag))) *html-stream*))

(defvar *func-attr-table* (make-hash-table))

(defun add-func-attr (attr macro)
  (setf (gethash attr *func-attr-table*) macro))

(defun func-attr (attr)
  (gethash attr *func-attr-table*))

(defmethod open-tag (tag (attributes cons))
 `(optimize-progn
   (write-string ,(format nil "<~a" (substitute #\: #\. (string tag))) *html-stream*)
   ,@(loop with forms
	   for attr = attributes then (cddr attr)
	   while attr
	   for attribute = (first attr)
	   for value = (second attr)
	   do
	   (cond
	     ((func-attr attribute)
	      (setf forms (nconc (reverse (funcall (func-attr attribute) value)) forms)))
	     (t (cond
		  ((stringp value)
		   (push `(write-string ,(format nil " ~a=\"~a\"" (string attribute) value) *html-stream*) forms))
		  (value
		   (push `(write-string ,(format nil " ~a=\"" (string attribute)) *html-stream*) forms)
		   (push `(write-string-quoting-specials ,value) forms)
		   (push `(write-char #\" *html-stream*) forms))
		  (t (push `(write-char #\Space *html-stream*) forms)
		     (push `(write-string ,(string attribute) *html-stream*) forms)))))
	   finally return (nreverse forms))
   (write-char #\> *html-stream*)))

(defmethod open-tag ((tag (eql :crlf)) attributes)
  `(write-string +crlf+ *html-stream*))

(defun close-tag (tag)
  `(write-string ,(format nil "</~A>" (substitute #\: #\. (string tag))) *html-stream*))

(defmacro html-to-stream (stream &rest forms)
  `(let ((*html-stream* ,stream)) (html ,@forms)))

(defmacro html-to-string (&rest forms)
  `(with-output-to-string (*html-stream*) (html ,@forms)))

(defmacro html-to-file (file-name &rest forms)
  `(with-open-file (*html-stream* ,file-name :direction :output :if-exists :supersede)
    (html ,@forms)))

