(in-package interface)

(defvar *session-encoding-vector* "0123456789azertyuiopqsdfghjklmwxcvbnAZERTYUIOPQSDFGHJKLMWXCVBN-$")

(defun reverse-encoding (string)
  (let ((decode-vector (make-array 128 :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop for c across *session-encoding-vector*
	  for i from 0
	  do (setf (aref decode-vector (char-code c)) i))
    decode-vector))

(defvar *session-decoding-vector* (reverse-encoding *session-encoding-vector*))

(defvar *sessions* (make-hash-table :test #'equal))
(defvar *session-cookies* (make-hash-table :test #'equal))
(defvar *enable-cookies* t)
(defvar *session-log* nil)
(defvar *session-timeout* 1200) ;in seconds
(defvar *session-file* #P"~/session-log.txt")
(defvar *robot-session-id* "Robot")
(defvar *robot-log* nil)
(defvar *robot-file* #P"~/robot-log.txt")
(defvar *session-timer-time* (get-universal-time))

(defvar *create-sessions-data-funcs* (make-hash-table))

(defvar *session-char-to-token* (make-hash-table))
(defvar *session-token-to-char* (make-hash-table))

(defun make-session-id ()
  (let ((id (make-string 8)))
    (loop for i from 0 below 8
	  do (setf (aref id i) (aref *session-encoding-vector* (random 63))))
    id))

(defclass session ()
  ((id :initform (make-session-id) :accessor id :initarg :id)
   (creation-time :initform *session-timer-time* :accessor creation-time)
   (last-access-time :initform *session-timer-time* :accessor last-access-time)
   (history :initform nil :accessor history)
   (original-cookie :initform nil :accessor original-cookie)
   (cookie :initform nil :accessor cookie)
   (browser :initform nil :accessor browser)
   (browser-ip :initform nil :accessor browser-ip)
   (user :initform nil :accessor user)
   (authentified :initform nil :accessor authentified)
   (application-data :initform nil :accessor application-data)
   (country-language-id :initform nil :accessor country-language-id)
   (country-language :initform nil :accessor country-language)
   (url-history :initform nil :accessor url-history)
   ))

(defmethod initialize-instance :after ((session session) &rest init-options &key &allow-other-keys)
  (declare (ignore init-options))
  (setf (gethash (id session) *sessions*) session)
  )

(defun end-session (session)
  (when (history session)
    (let ((data (list* (cookie session)(browser session)(browser-ip session)(nreverse (history session)))))
      (if (string= *robot-session-id* (id session))
	(push data *robot-log*)
	(push data *session-log*))))
  (remhash (id session) *sessions*)
  (remhash (cookie session) *session-cookies*))

(defun clear-all-sessions ()
  (clrhash *sessions*)
  (clrhash *session-cookies*))

(defun register-tokens-chars (token-chars)
  (loop for (token char . rest) on token-chars by 'cddr
	do (setf (gethash char *session-char-to-token*) token
		 (gethash token *session-token-to-char*) char)))

(defmethod (setf cookie) :around (value (obj session))
  (when (cookie obj)
    (remhash (cookie obj) *session-cookies*))
  (call-next-method)
  (setf (gethash value *session-cookies*) obj))

(register-tokens-chars
 '(:session #\S
   :lang #\L
   :page #\P ;never touch this one!
   :state #\N
   :appli #\A
   :func #\F
   :user #\U
   :object #\O
   :view #\V
   :interface #\I
   :link #\K
   :store #\R
   :param0 #\0
   :param1 #\1
   :param2 #\2
   :param3 #\3
   :param4 #\4
   :param5 #\5
   :param6 #\6
   :param7 #\7
   :param8 #\8
   :param9 #\9
   ))

(defun encode-session-url (stream values &key (url-prefix *url-prefix*) absolute)
  (declare (optimize (speed 3)(debug 0)(safety 0)(space 0)))
  (let ((object (getf values :object)))
    (when (and object (not (stringp object)))
      (setf (getf values :object) (encode-object-id  object))))
  (setf values (loop for (key val . rest) on values by 'cddr
		     when val nconc (list key val)))
  (flet ((write-url (stream values url-prefix absolute)
	   (let ((page "/b"))
	     (when absolute
	       (write-string "http://" stream)
	       (write-string *server-name* stream))
	     (write-string url-prefix stream)
	     (write-char #\/ stream)
	     (write-string *request-id* stream)
	     (write-string "/sdata" stream)
;internal format ::= <TokenChar><Value><Tab>
	     (write-string (html:encode-url-string
			    (let ((string (make-string (+ (length values)
							  (loop for (key val . rest) on values by 'cddr
								if (eq key :page)
								sum -2
								else
								sum (length val))))))
			      (loop with i of-type fixnum = 0
				    for (key val . rest) on values by 'cddr
				    for l of-type fixnum = (length val)
				    if (eq key :page) do
				    (setf page (concatenate 'string "/sdata"
							    (html:encode-url-string
							     (concatenate 'string "P" val))))
				    else do
				    (setf (aref string i) (gethash key *session-token-to-char*))
				    (incf i)
				    (replace string val :start1 i)
				    (incf i l)
				    (setf (aref string i) #\Tab)
				    (incf i))
			      string))
			   stream)
	     (write-string page stream))))
    (if stream
      (write-url stream values url-prefix absolute)
      (with-output-to-string (s) (write-url s values url-prefix absolute)))))

(defun encode-page (page)
  (concatenate 'string *request-id* "sdata"
	       (html:encode-url-string (concatenate 'string "P" page))))

(defun encode-session-values (stream values)
  (declare (optimize (speed 3)(debug 0)(safety 0)(space 0)))
  (flet ((write-url (stream values)
	   (write-string "sdata" stream)
;internal format ::= <TokenChar><Value><Tab>
	   (write-string (html:encode-url-string
			  (let ((string (make-string (+ (length values)
							(loop for (key val . rest) on values by 'cddr
							      sum (length val))))))
			    (loop with i of-type fixnum = 0
				  for (key val . rest) on values by 'cddr
				  for l of-type fixnum = (length val)
				  do
				  (setf (aref string i) (gethash key *session-token-to-char*))
				  (incf i)
				  (replace string val :start1 i)
				  (incf i l)
				  (setf (aref string i) #\Tab)
				  (incf i))
			    string))
			 stream)))
    (if stream
      (write-url stream values)
      (with-output-to-string (s) (write-url s values)))))

(defun decode-session-url (url)
  (let (string
	(params nil)
	(next-char 0))
    (loop while next-char
	  do (setf (values string next-char) (extract-param-string "sdata" url next-char))
	  while string
	  do 
	  (setf string (html:decode-url-string (nsubstitute #\= #\* (nsubstitute #\= #\_ string))))
	  (setf params (nconc (loop with i = 0 and l = (length string)
				    while (< i l)
				    for key = (gethash (aref string i) *session-char-to-token*)
				    for pos = (position #\Tab string :start (1+ i))
				    for value = (subseq string (1+ i) pos)
				    nconc (list key value)
				    while pos
				    do (setf i (1+ pos)))
			      params)))
    params))

(defun encode-integer (n)
  (declare (optimize (speed 3)(debug 0)(safety 0)(space 0)))
  (loop with size = (ceiling (integer-length n) 6)
	with string = (make-string size)
	for i below size
	for b from 0 by 6
	do (setf (schar string i)(schar *session-encoding-vector* (ldb (byte 6 b) n)))
	finally (return string)))

(defun decode-integer (string)
  (declare (optimize (speed 3)(debug 0)(safety 0)(space 0)))
  (let ((decode-vector *session-decoding-vector*)
	(l (length string)))
    (declare (type (simple-array (unsigned-byte 8) 128) decode-vector))
    (loop with n = 0
	  for i below l
	  for b from 0 by 6
	  do (setf (ldb (byte 6 b) n)(aref decode-vector (char-code (aref string i))))
	  finally (return n))))

(defun encode-object-id (object)
  (encode-integer (dpb (meta::store-id (meta::object-store object))
		       (byte 8 64) (meta::id object))))

(defun decode-object-id (obj-string)
  (when (and obj-string (not (string= obj-string "nil")))
    (let ((full-id (decode-integer obj-string)))
      (meta::load-object (ldb (byte 64 0) full-id) (meta::find-store (ldb (byte 8 64) full-id))))))

(defun meta::decode-object-id (string)
  (decode-object-id string))

(defun encode-object-url (object &key stream absolute args)
  (setf args (append (list :page "object" :object object) args))
  (when *session*
    (setf args (list* :session (id *session*) :lang *country-language-id* args)))
  (encode-session-url stream args :absolute absolute))

(defun get-session (request &optional (session-params (decode-session-url (url request))))
  (let* ((session-id (getf session-params :session))
	 (session (or (and session-id (gethash session-id *sessions*))
                      (and *enable-cookies* (gethash (cookie request) *session-cookies*)))))
    (when (and session (cookie session)
               (not (new-cookie request)) (not (equal (cookie session)(cookie request))))
      (setf session nil)) ;;bad cookie!
    (if session
      (when (string= session-id *robot-session-id*)
	(unless (web-robot-request? request)
	  (setf session nil)))
      (when (web-robot-request? request)
	(setf session (make-instance 'session :id *robot-session-id*))))
    (when session
      (unless (history session)
	(setf (cookie session) (cookie request))
        (setf (browser-ip session) (cdr (assoc "remote-ip-addr" (command request) :test #'equal)))
	(setf (browser session) (cdr (assoc "User-Agent" (command request) :test #'equal))))
      (setf (last-access-time session) *session-timer-time*)
      (unless (getf session-params :func)
	(process-url-history request session)
	(push (cons *session-timer-time* session-params) (history session)))
      (setf (session request) session)
      (setf (session-params request) session-params))
    session))

(defun create-session (request)
  (if (web-robot-request? request)
    (make-instance 'session :id *robot-session-id*)
    (let ((session (make-instance 'session)))
      (setf (cookie session) (cookie request)))))

(defun encode-page-reader (stream subchar arg)
  (declare (ignore arg subchar))
  (let ((first-char (peek-char nil stream t nil t)))
    (cond ((char= first-char #\space)
	   (read-char stream)	  ; skip over whitespaceitalian
	   (encode-page-reader stream nil nil))
	  ((char= first-char #\") ;read a string
	   `(encode-page ,(read stream t nil t)))
	  ((char= first-char #\() ;read a list
	   `(encode-session-url nil ,(cons 'list (read stream t nil t))))
	  (t
	   (error "need a list or a string for #E")))))

(set-dispatch-macro-character #\# #\E 'encode-page-reader)

(defvar %next-timeout-check% 0)
(defvar %timeout-check-interval% 60)

(defun sessions-timer ()
  (setf *session-timer-time* (get-universal-time))
  (when (> *session-timer-time* %next-timeout-check%)
    (setf %next-timeout-check% (+ *session-timer-time* %timeout-check-interval%))
    (maphash #'(lambda (id session)
		 (declare (ignore id))
		 (when (and session
                            (or 
                             (and (not (browser-ip session))
                                  (> (- *session-timer-time* (last-access-time session)) 20))
                             (> (- *session-timer-time* (last-access-time session)) *session-timeout*)))
		   (end-session session)))
	     *sessions*)
    (dump-session-log)))

(defun dump-session-log ()
  (when *session-log*
    (with-open-file (s *session-file* :direction :output :if-exists :append :if-does-not-exist :create)
      (loop for session in *session-log*
	    do (format s "~s~%" session)))
    (setf *session-log* nil))  
  (when *robot-log*
    (with-open-file (s *robot-file* :direction :output :if-exists :append :if-does-not-exist :create)
      (loop for session in *robot-log*
	    do (format s "~s~%" session)))
    (setf *robot-log* nil)))

(defun start-session-timer ()
  (let ((timer (mp:make-timer 'sessions-timer)))
    (mp:schedule-timer-relative timer 5 5)))

(defvar *session-timer* (start-session-timer))

(defvar *cookie-expiration* (concatenate 'string "; path=/; expires="
					 (html:universal-time-to-string
					  (+ (* 3600 24 365 10) *session-timer-time*))))

(defun make-cookie ()
  (concatenate 'string (make-session-id)(make-session-id)))

(defun process-cookie (request)
  (let ((cookie (cdr (assoc "Cookie" (command request) :test #'string=))))
    (unless cookie
      (setf cookie (make-cookie)
            (new-cookie request) t)
      (push-header "Set-Cookie" (concatenate 'string cookie *cookie-expiration*) request))
    (setf (cookie request) cookie)))

;  (let ((cookies (cdr (assoc "Cookie" (command request) :test #'string=))))
;    (if cookies
;      (let ((pos (search "LispID=" cookies)))
;	(when pos
;	  (let ((end (position #\Space cookies :start pos)))
;	    (setf (cookie request)(subseq cookies (+ pos 11) end)))))
;      (progn (setf cookie (make-session-id))
;	     (push-header "Set-Cookie" (concatenate 'string "LispID=" cookie *cookie-expiration*) request)))
;  (let ((cookie (cdr (assoc "Cookie" (command request) :test #'string=))))
;    (unless cookie
;      (setf cookie (make-session-id))
;      (push-header "Set-Cookie" (concatenate 'string "LispID=" cookie *cookie-expiration*) request))
;    (setf (cookie request) cookie)))

(defun string-to-float (string)
  (let* ((*read-eval* nil)
	 (first-digit (position-if #'(lambda (c)
				       (find c "0123456789+-.,"))
				   string))
	 (last-digit (position-if-not #'(lambda (c)
					  (find c "0123456789+-.,"))
				      string
				      :start (if first-digit first-digit 0)))
	 (trimmed-value (ignore-errors (read-from-string (nsubstitute #\. #\, (subseq string first-digit last-digit))))))
    (when trimmed-value (ignore-errors (float trimmed-value)))))

(defun update-params (object posted-content param-list)
  (loop for (slot-name param-name) in param-list
	for string-value = (cdr (assoc param-name posted-content :test 'string=))
	do (when string-value (meta::setf-string-to-slot-value object slot-name string-value)))
  object)

(defun with-session-param-tag (attributes form)
  (destructuring-bind ((param-block-name data-tag &rest params) . forms) form
    `(let ((,param-block-name (update-params (get-application-data interface::*session* ,data-tag)
					     (interface::posted-content interface::*request*)
					     ',params)))
      (html::optimize-progn ,@(mapcar #'(lambda (e) (html:html-gen e)) forms)))))

(html:add-func-tag :with-session-params 'with-session-param-tag)

(defun get-application-data (session data-tag)
  (let ((data (getf (application-data session) data-tag)))
    (when (not data)
      (let ((create-application-data-func (gethash data-tag *create-sessions-data-funcs*)))
	(when create-application-data-func
	  (setf data (funcall create-application-data-func session data-tag))
	  (set-application-data session data-tag data))))
    data))

(defun set-application-data (session data-tag data)
  (setf (getf (application-data session) data-tag) data))

(defun add-create-session-data (data-tag func)
  (setf (gethash data-tag *create-sessions-data-funcs*) func))

(defun remove-create-session-data (data-tag)
  (remhash data-tag *create-sessions-data-funcs*))

(defun with-posted-param-tag (attributes form)
  (destructuring-bind (params . forms) form
    (let ((posted-content (gensym)))
      `(let* ((,posted-content (posted-content interface::*request*))
	      ,@(loop for (var-name posted-param type default) in params
		      collect `(,var-name (extract-posted-param ,posted-content ,posted-param ,type ,default))))
      (html::optimize-progn ,@(mapcar #'(lambda (e) (html:html-gen e)) forms))))))

(html:add-func-tag :with-posted-params 'with-posted-param-tag)

(defun extract-posted-param (posted-content posted-param type default)
  (let* ((string-value (cdr (assoc posted-param posted-content :test 'string=)))
	 (value (if string-value
		  (case type
		    (:int (parse-integer string-value :junk-allowed t))
		    (:float (meta::string-to-float string-value))
		    (:string string-value)
		    ((t) string-value))
		  default)))
    (if value value default)))
    
;;*************************
;; log stuff

(defparameter *cookies-to-remove* '("LispID=MDF2A3OE"))

(defun process-logs (file result)
  (with-open-file (r result :direction :output :if-exists :supersede)
    (with-open-file (s file :direction :input)
      (loop for session = (read s nil nil)
	    while session
	    do
	    (when (and (string/= (third session) "192.168.2.10")
		       (string/= (third session) "192.168.2.200")
		       (not (member (first session) *cookies-to-remove* :test #'string=)))
	      (format r "~%~%Cookie: ~a~%Browser: ~a~%Adresse IP: ~a~%"
		      (first session)(second session)(third session))
	      (loop for (time . rest) in (cdddr session)
		    do (format r "          ~a ~s~%" (html:universal-time-to-string time) rest)))))))

(defun map-logs (file func)
  (with-open-file (s file :direction :input)
    (loop for session = (read s nil nil)
	  while session
	  do
	  (when (and (string/= (third session) "213.11.22.164")
		     (not (member (first session) *cookies-to-remove* :test #'string=)))
	    (funcall func session)))))

(defun extract-stats (file page-table)
  (let ((pages (make-hash-table :test #'equal))
	(sorted-pages nil)
	(page-pages (make-hash-table :test #'equal))
	(sorted-page-pages nil)
	(nb-sessions 0)
	(nb-pages 0))
    (map-logs file
	      #'(lambda (session)
		  (when (search "MSIE" (second session))
		    (incf nb-sessions)
		    (loop for (time . rest) in (cdddr session)
			  for prev-page = nil then page
			  for page = (getf rest :page)
			  do (when page
			       (incf nb-pages)
			       (incf (gethash page pages 0))
			       (when page
				 (incf (gethash (list prev-page page) page-pages 0))))))))
    (format t "Nombre de sessions: ~d~%Nombre de pages: ~d~%" nb-sessions nb-pages)
    (maphash #'(lambda (page count)(push (list page count) sorted-pages))
	     pages)
    (maphash #'(lambda (page count)(push (list page count) sorted-page-pages))
	     page-pages)
    (format t "~%~%Fréquence des pages (% par session): ~%" )
    (loop for (page count) in (sort sorted-pages #'> :key 'second)
	  do (format t "~a	~,2f~%" page (* 100.0 (/ count nb-sessions))))
    (format t "~%~%Fréquence des pages transitions page->page (% par session):~% " )
    (loop for ((page1 page2)  count) in (sort sorted-page-pages #'> :key 'second)
	  repeat 50
	  do (format t "~a -> ~a	~,2f~%" page1 page2 (* 100.0 (/ count nb-sessions))))
    (format t "~%~%Page non visitées:~%" )
    (maphash #'(lambda (page func)
		 (unless (gethash page pages)
		   (format t "~a ~%" page)))
	     page-table)))

(defun clear-url-history (session)
    (setf (url-history session) ()))

(defun normalize-referer (url)
  (when url
    (let ((start (search "/asp" url)))
      (when start (subseq url start)))))
  
(defun process-url-history (request session)
  (let ((url (url request))
	(referer (referer request)))
    (unless (find url (url-history session) :test #'string= :key 'first)
      (setf referer (normalize-referer referer))
      (when referer
	(push (cons referer url) (url-history session))
	(when (> (length (url-history session)) 50)
	  (setf (url-history session) (subseq (url-history session) 0 50)))))))

(defun get-previous-page (request)
  (first (find (url request) (url-history (session request)) :test #'string= :key 'cdr)))

