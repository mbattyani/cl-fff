(in-package interface)

(defun write-header-line (socket key value)
  (write-string key socket)
  (write-char #\NewLine socket)
  (write-string value socket)
  (write-char #\NewLine socket))

(defun write-http-header (socket &key (status "200 OK")
				 authenticate location content-location content-length
				 (content-type "text/html"))
  (macrolet ((header-write (name string)
	       `(when ,name
		 (write-string ,string socket)
		 (write-char #\NewLine socket)
		 (write-string ,name socket)
		 (write-char #\NewLine socket))))
    (header-write status "Status")
    (header-write content-type "Content-Type")
    (header-write authenticate "WWW-Authenticate")
    (header-write location "Location")
    (header-write content-location "Content-Location")
    (header-write (when content-length (html::fast-format nil "~s" content-length))
		  "Content-Length")
    (write-string "end" socket)
    (write-char #\NewLine socket))
  (unless content-length (setf *close-apache-socket* t)))


(defmacro extract-param (param-name url &optional default)
  `(let ((pos (search ,param-name ,url))
	 (value ,default))
    (when pos
      (incf pos) ;skip #\=
      (let ((end (position #\/ ,url :start pos)))
	(when end (setf value ,`(read-from-string ,url nil nil :start (+ pos ,(length param-name)) :end end)))))
    value))

(defun extract-param-string (param-name url &optional (start 0))
  (let ((pos (search param-name url :start2 start))
	(end nil)
	(value nil))
    (when pos
;      (incf pos) ;skip #\=
      (setf end (position #\/ url :start pos))
      (unless end (setf end (position #\# url :start pos)))
      (setf value (subseq url (+ pos (length param-name)) end)))
    (values value end)))

(defun decode-search-params (url)
  (let* ((params (html::parse-url-search-part url))
	 (fdata (cdr (assoc "fdata" params :test #'equal))))
    (when fdata (setf fdata (read-from-string (html:decode-url-string fdata))))
    (acons :fdata fdata params)))

(defparameter *country-items* '(:en ("en" "/sen.gif" "Switch to English")
				:fr ("fr" "/sfr.gif" "Passer en français")))

(defun language-choice (country &key flag-file)
  (let ((items (getf *country-items* country)))
    (html:html ;(:td((:form "method" "GET")
		((:input "type" "image" "src" (if flag-file flag-file (second items))
			 "style" "width:30;height:20;border:none" "title" (third items)
			 "name" (first items))))))
					;:click-id (first items) :tooltip (third items)
					;:action-func 'interface::switch-country-lang)))))

(defun language-choices ()
  (html:html ((:form "method" "GET")
;  (html:html (:table
	      ((:input "type" "hidden" "name" "fdata" "value" #.(html:encode-url-string "(:post \"switch-lang\")")))
	      (loop for country in *country-items* by 'cddr
		    do (language-choice country)))))

(defmacro with-html-page (args &body forms)
  (destructuring-bind (stream &key no-header title style-sheet description script-lib icon 
			      (encoding "text/html; charset=ISO-8859-1")) args
    `(progn
      (html::html-to-stream ,stream "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" >"
       (:html (:head 
	       ,@(when icon `(((:link :rel "Shortcut icon" :href ,icon))))
	       ,@(when title `((:title ,title)))
	       ,@(when style-sheet `(((:link :rel "stylesheet" :type "text/css" :href ,style-sheet))))
	       ,@(when description `(((:meta :name "description" :content ,description))))
	       ,@(when keywords `(((:meta :name "keywords" :content ,keywords))))
	       ,@(when encoding `(((:meta :http-equiv "Content-type" :content ,encoding))))
	       ,@(when script-lib `(((:script :src ,script-lib)))))
	,@forms)))))

(defvar *ip-hosts* (make-hash-table :test #'equal))

(defun look-up-ip (ip &key print-new)
  (let ((host (gethash ip *ip-hosts*)))
    (unless host
      (setf host (list (comm:get-host-entry ip :fields '(:name :aliases :address)) 0))
      (setf (gethash ip *ip-hosts*) host)
      (when print-new (format t "~a ~a~%" (first host) ip)))
    (incf (second host))
    (values (first host)(second host))))

(defun parse-log-file (file)
  (with-open-file (s file :direction :input)
    (loop for line = (read-line s)
	  while line
	  do
	  (look-up-ip (subseq line 0 (position #\Space line)) :print-new t))))

(defvar *domains* (make-hash-table :test #'equal))

(defun find-domains ()
  (clrhash *domains*)
  (loop for h being the hash-value in *ip-hosts*
	for name = (nstring-downcase (first h))
	for domain = (when (and name (position #\. name)) (subseq name (position #\. name :from-end t)))
	do (when domain
	     (let ((host (gethash domain *domains*)))
	       (unless host
		 (setf host (list domain 0))
		 (setf (gethash domain *domains*) host))
	       (incf (second host)))))
  (sort (loop for d being the hash-value in *domains* collect d) #'> :key #'second))

(defun write-24-hour-time (hours mins secs &optional (stream *standard-output*))
  (when (< hours 10) (write-char #\0 stream))
  (html:fast-format stream "~d" hours)
  (write-char #\: stream)
  (when (< mins 10) (write-char #\0 mins))
  (html:fast-format stream "~d" hours)
  (write-char #\: stream)
  (when (< secs 10) (write-char #\0 stream))
  (html:fast-format stream "~d" secs))

(defun write-standard-time (&key (universal-time (get-universal-time))
				 (stream *standard-output*) gmt-offset-p time-zone)
  "Writes the ISO date followed by the time on STREAM."
  (flet ((gmt-offset (timezone &optional daylight-savings (stream *standard-output*))
           (write-char #\space stream)
           (when daylight-savings (setq timezone (the fixnum (1- timezone))))
           (multiple-value-bind (hours sec)
               (truncate (* timezone 3600) 3600)
             (if (minusp hours)
                 (write-char #\- stream)
                 (write-char #\+ stream))
             (when (< (abs hours) 10) (write-char #\0 stream))
             (write-positive-fixnum (abs hours) 10 stream)
             (let ((mins (abs (truncate sec 60))))
               (when (< mins 10) (write-char #\0 stream))
               (write-positive-fixnum mins 10 stream)))))
    (declare (inline gmt-offset))
    (multiple-value-bind (secs mins hours day month year weekday daylight-savings timezone)
        (if time-zone
            (decode-universal-time universal-time time-zone)
            (decode-universal-time universal-time))
      weekday                                   ;ignore
        (html:fast-format stream "~d" year)
        (write-char #\- stream)
        (when (< month 10) (write-char #\0 stream))
        (html:fast-format stream "~d" month)
        (write-char #\- stream)
        (when (< day 10) (write-char #\0 stream))
        (html:fast-format stream "~d" day)
        (write-char #\space stream)
        (write-24-hour-time hours mins secs stream)
        ;; not sure if this is really the right GMT offset computational  6/28/94 -- JCMa.
        (and gmt-offset-p (gmt-offset timezone daylight-savings stream)))))

