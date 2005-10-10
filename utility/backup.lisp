


(defun map-all-files (directory file-function &optional (directory-function #'print) exclude-types exclude-directories)
  (dolist (path (directory (make-pathname :name :wild :type :wild :defaults directory) :directories t))
	  (if (lw:file-directory-p path)
	      (unless (member (first (last (pathname-directory path))) exclude-directories :test #'equal)
		      (funcall directory-function path)
		      (map-all-files path file-function directory-function exclude-types exclude-directories))
	      (unless (and (pathname-type path)(member (pathname-type path) exclude-types :test #'string-equal))
		      (funcall file-function path)))))


;(defun collect-all-files (directory &optional exclude-types exclude-directories)
;  (cons directory
;	(mapcan #'(lambda (path)
;		    (if (lw:file-directory-p path)
;			(unless (member (first (last (pathname-directory path))) exclude-directories :test #'equal)
;				(list (collect-all-files path exclude-types exclude-directories)))
;		      (unless (and (pathname-type path)(member (pathname-type path)exclude-types :test #'string-equal)) (list path))))
;		    (directory (make-pathnames :name :wild :type :wild :defaults directory) :directories t))))


(defun collect-files-times (directory &optional exclude-types exclude-directories)
  (let ((files '())
	(dirs  '()))
  (map-all-files directory
		 #'(lambda (file)
		     (push (list file (file-write-date file)) files))
		 #'(lambda (file)
		     (push file dirs))
		 exclude-types exclude-directories)
  (values files dirs)))


(defconstant +disk-block-size+ 8192)

(defun copy-file (from to)
  (with-open-file (in from :direction :input)
     (with-open-file (out to :direction :output :if-exists :supersede)
        (let ((buffer (make-array +disk-block-size+ :element-type (stream-element-type in))))
	  (loop for read = (read-sequence buffer in) until (zerop read) do
		(write-sequence buffer out :end read))))))

(defun backup-directories (source-dir dest-dir exclude-types exclude-directories)
  (multiple-value-bind (source-files source-dirs) (collect-files-times source-dir exclude-types exclude-directories)
     (loop for dir in source-dirs
	   for name = (concatenate 'string dest-dir (directory-namestring dir))
	   do
	   (format t "Creating directory ~A~%" name)
	   (ensure-directories-exist name))
     (loop for (src-file time) in source-files
	   for dst-file = (make-pathname concatenate 'string dest-dir (directory-namestring src-file)(file-namestring src-file))
	   for old-time = (with-open-file (s dst-file :if-does-not-exist nil) (if s (file-write-date s) 0))
	   do
	   (if (< time old-time)
	       (format t "Skipping file ~A~%" dst-file)
	       (progn (format t "Copying file ~A~%" dst-file)
		      (copy-file src-file dst-file))))))

















