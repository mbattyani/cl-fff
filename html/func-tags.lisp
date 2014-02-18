(in-package #:html)

(defun comment-tag (attributes form)
  (declare (ignore attributes))
  `(optimize-progn
    (write-string "<!--" *html-stream*)
    ,@(mapcar #'html-gen form)
    (write-string "-->" *html-stream*)))

(add-func-tag :comment 'comment-tag)

(defun progn-tag (attributes form)
  (declare (ignore attributes))
  `(optimize-progn ,@(mapcar #'html-gen form)))

(add-func-tag :progn 'progn-tag)

(defun merge-attributes (attributes defaults)
  (loop
     for (attribute value . nil) on defaults
     do
	(unless (find attribute attributes :test #'eq)
	  (push value attributes)
	  (push attribute attributes)))
  attributes)

(defun if-tag (attributes form)
  (declare (ignore attributes))
  `(if ,(first form)
    ,(html-gen (second form))
    ,(html-gen (third form))))

(add-func-tag :if 'if-tag)

(defun when-tag (attributes form)
  (declare (ignore attributes))
  `(when ,(first form)
    (optimize-progn ,@(mapcar #'html-gen (cdr form)))))

(add-func-tag :when 'when-tag)

(defun exec-attr (value)
  `((write-char #\Space *html-stream*), value))

(add-func-attr :exec 'exec-attr)

(defun insert-string-attr (value)
  `((write-char #\Space *html-stream*)
    (write-string ,value *html-stream*)))

(add-func-attr :insert-string 'insert-string-attr)

(defun format-attr (value)
  `((write-string ,(format nil " ~a=\"" (string (first value))) *html-stream*)
    (format *html-stream* ,@(rest value))
    (write-char #\" *html-stream*)))

(add-func-attr :format 'format-attr)

(defun format-tag (attributes form)
  (declare (ignore attributes))
  `(format *html-stream* ,@form))

(add-func-tag :format 'format-tag)

(defun fformat-attr (value)
  `((write-string ,(format nil " ~a=\"" (string (first value))) *html-stream*)
    (fast-format *html-stream* ,@(rest value))
    (write-char #\" *html-stream*)))

(add-func-attr :fformat 'fformat-attr)

(defun fformat-tag (attributes form)
  (declare (ignore attributes))
  `(fast-format *html-stream* ,@form))

(add-func-tag :fformat 'fformat-tag)

(defun imgz-tag (attributes form)
  (declare (ignore form))
  (let* ((src (getf attributes :src ""))
         (pos (search "-s.jpg" src))
	 (srcz (getf attributes :srcz
		     (or (when pos (remove-if #'identity src :start pos :end (+ pos 2)))
			 src))))
   (setf attributes (copy-list attributes))
   (remf attributes :srcz)
   (html-gen
    `((:a :target "_blank" :href ,srcz)
      ((:img :border ,(getf attributes :border 0) ,@attributes))))))

(add-func-tag :imgz 'imgz-tag)

(defun optional-attr (value)
  (let ((val (gensym)))
    `((let ((,val ,(second value)))
	(when ,val
	  (write-string ,(format nil " ~a=\"" (string (first value))) *html-stream*)
	  (princ ,val *html-stream*)
	  (write-char #\" *html-stream*))))))

(add-func-attr :optional 'optional-attr)

(defun esc-tag (attributes form)
  (declare (ignore attributes))
  `(esc ,@form))

(add-func-tag :esc 'esc-tag)

(defun insert-string-tag (attributes form)
  (declare (ignore attributes))
  `(write-string ,(first form) *html-stream*))

(add-func-tag :insert-string 'insert-string-tag)

(export '*html-insert-file-defaults*)
(defvar *html-insert-file-defaults* ".htm")

(defun insert-file-tag (attributes forms)
  (declare (ignore attributes))
  (destructuring-bind (file-name) forms
    (let ((data nil))
      (ignore-errors
	(with-open-file (s (if *html-insert-file-defaults*
                               (merge-pathnames file-name *html-insert-file-defaults*)
                               file-name)
			   :direction :input :external-format '(:default :eol-style :lf))
	  (let* ((file-length (file-length s)))
	    (setf data (make-string file-length))
	    (read-sequence data s))))
      (unless data (setf data (format nil "The file ~a does not exit"
				      (merge-pathnames file-name *html-insert-file-defaults*))))
      `(write-string ,data html:*html-stream*))))

(add-func-tag :insert-file 'insert-file-tag)

(defun jscript-tag (attributes forms)
  (html-gen
   `((:script :language "JavaScript" :type "text/javascript" ,@attributes) ,@forms)))

(add-func-tag :jscript 'jscript-tag)

(defun %ps (attributes forms)
  (declare (ignore attributes))
  `(optimize-progn
     ,@(mapcar (lambda (x)
                 (html-gen (ps:ps* x))) forms)))

(add-func-tag :ps '%ps)
