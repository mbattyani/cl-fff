(in-package #:html)

(defvar *html-stream*)


(defmacro with-fast-array-references (bindings &body body)
  "Declares the arrays in bindings (var value &optional type)
as type and sets speed to 3 with safety 0 within its scope."
  (loop for (var val type) in bindings
        collect `(,var ,val) into n-bindings
        when type
          collect `(type ,type ,var) into type-dcls
        finally (return `(let ,n-bindings
                           (locally 
                             (declare (optimize (speed 3) (safety 0)) . ,type-dcls)
                             ,@body)))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defconstant *special-character-translation-alist*
    '((#\> . "&gt;")
      (#\< . "&lt;")
      (#\& . "&amp;")
      (#\ÿ . "<br>")
      (#\" . "&quot;")
      #+nil(#\space . "&nbsp;"))
    "&; delimited tokens are used to print special tokens.")
  )

(defun write-string-quoting-specials (string &optional (stream *html-stream*) (start 0) end)
  "Writes STRING to STREAM being careful to translated any special characters for HTML."
  (when string
    (flet ((%token-spec-for-special-char (char)
	     #.`(case char
                  ,.(loop for (char . string) in *special-character-translation-alist*
                       collect `(,char '(,string . ,(length string))))
                  (t nil)))
	   (write-part (string stream start end)
	     (unless (= start end)
	       (write-string string stream :start start :end  end))))
      (declare (inline %token-spec-for-special-char write-part))
      (with-fast-array-references ((vector string string))
	(loop
           with scan-idx
           for idx upfrom start below (or end (length vector))
           for char = (aref vector idx)
           for token-spec = (%token-spec-for-special-char char)
           do (when token-spec
                (write-part vector stream (or scan-idx start) idx)
                (write-string (car token-spec) stream :start 0 :end (cdr token-spec))
                (setq scan-idx (1+ (the fixnum idx))))
           finally (if scan-idx
                       (write-part vector stream scan-idx idx)
                       (write-string vector stream :start start :end idx)))))))

(defun quote-string (string)
  (with-output-to-string (str)
			 (write-string-quoting-specials string str)
			 str))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defconstant *javascript-character-translation-alist*
    '((#\' . "\\'")
      (#\" . "\\\"")
      (#\Newline . "\\n")
      (#\Return . "\\r")))
  )

(defun quote-javascript-string (string)
  (with-output-to-string (str)
    (write-javascript-string string str)
    str))

(defun write-javascript-string (string &optional (stream *html-stream*) (start 0) end)
  (flet ((%token-spec-for-special-char (char)
	   (case char (#\\ (quote ("\\\\" . 2)))(#\' (quote ("\\'" . 2))) (#\" (quote ("\\\"" . 2)))
		 (#\Newline (quote ("\\n" . 2)))(#\Return (quote ("\\r" . 2))) (t nil)))
;           #.`(case char
;                ,.(loop for (char . string) in html::*javascript-character-translation-alist*
;                        collect `(,char '(,string . ,(length string))))
         (write-part (string stream start end)
           (unless (= start end)
             (write-string string stream :start start :end  end))))
    (declare (inline %token-spec-for-special-char write-part))
    (with-fast-array-references ((vector string string))
      (loop with scan-idx
            for idx upfrom start below (or end (length vector))
            for char = (aref vector idx)
            for token-spec = (%token-spec-for-special-char char)
            do (when token-spec
                 (write-part vector stream (or scan-idx start) idx)
                 (write-string (car token-spec) stream :start 0 :end (cdr token-spec))
                 (setq scan-idx (1+ (the fixnum idx))))
            finally (if scan-idx
                        (write-part vector stream scan-idx idx)
                        (write-string vector stream :start start :end idx))))))

;;; from TBNL
(defun url-encode (string)
  "URL-encodes a string using the external format EXTERNAL-FORMAT."
  (with-output-to-string (s)
    (loop for c across string
          do (cond ((or (char<= #\0 c #\9)
                        (char<= #\a c #\z)
                        (char<= #\A c #\Z)
                        (find c "$-_.!*'()," :test #'char=))
                     (write-char c s))
                   #+nil((char= c #\Space)
                     (write-char #\+ s))
                   (t (format s "%~2,'0x" (char-code c)))))))

;;;------------------------------------------------------------------- 
;;;
;;; FAST COMPILE-TIME FORMAT
;;;

(defgeneric %princ-item (item stream)
  (:documentation "Prints item without slashification on stream.
Specialize for higher performance according platform."))

(defmethod %princ-item (item stream)
    (princ item stream))

(defmethod %princ-item ((item string) stream)
    (write-string item stream)) 

(defun %fast-format-execute-command (idx format-string stream arg-n args length &aux base-p)
  (macrolet ((pop-arg (args)
	       `(prog1 (pop ,args)
		 (incf arg-n)))
	     (arg-n (n) `(nth ,n format-args)))
    (values
     (ecase (aref format-string (incf idx))
       ((#\A #\a)
	`(%princ-item,(pop-arg args) ,stream))
       ((#\D #\d)
	(setq base-p t)
	`(%princ-item ,(pop-arg args) ,stream))
       ((#\C #\c)
	`(write-char ,(pop-arg args) ,stream))
       ((#\S #\s)
	`(prin1 ,(pop-arg args) ,stream))
       ((#\Return #\Linefeed)
	(loop with s = (1+ idx)
	      for i upfrom s below length
	      while (member (aref format-string i) '(#\space #\tab #\Return #\Linefeed))
	      finally (when (< s i)
			(setq idx (1- i))))
	nil)
       (#\&
	`(fresh-line ,stream))
       (#\%
	`(terpri ,stream))
       (#\~ `(write-char #\~ ,stream))
       ((#\I #\i)
	(pop-arg args)))
     args
     arg-n
     (1+ idx)
     base-p)))

(defun %build-fast-format-code (stream format-string format-args)
  (check-type format-string string)
  (check-type format-args list)
  (let ((len (length format-string))
	(args (copy-list format-args))
	(arg-n 0))
    (declare (dynamic-extent args))
    (flet ((write-string-form (stream string start end)
	     (declare (fixnum start end))
	     (let ((length (- end start)))
	       (if (= 1 length)
		 `(write-char ,(aref string start) ,stream)
		 `(write-string ,(subseq string start end) ,stream :start 0 :end ,length)))))
      (loop with start = 0 and form and arg-form and base-p
	    for idx = 0 then (1+ idx)
	    while (< idx len)
	    for char = (aref format-string idx)
	    for inside-command-p = nil then start-command-p
	    for start-command-p = (and (not inside-command-p) (char-equal char #\~))
	    when start-command-p
	    do (setq form (when (< start idx)
			    (write-string-form stream format-string start idx)))
	    (multiple-value-bind (form n-args n-arg-n n-start bind-base-p)
		(%fast-format-execute-command idx format-string stream arg-n args len)
	      (when bind-base-p
		(setq base-p t))
	      (setq arg-form form
		    args n-args
		    arg-n n-arg-n
		    start n-start))
	    when (and start-command-p form)
	    collect form into result
	    when (and start-command-p arg-form)
	    collect arg-form into result
	    finally (return (let ((code (if (< start idx)
					  `(,.result ,(write-string-form stream format-string start idx) nil)
					  `(,.result nil))))
			      (if base-p
				`((let ((*print-base* 10.)) ,.code))
				code)))))))

(defmacro fast-format (stream format-string &rest format-args)
"A simple version of FORMAT that expands into fast code at compile time.
FORMAT-STRING must be a string.
The follow format directives are supported.

     ~%         -- hard carriage return
     ~&         -- fresh line
     ~~         -- writes a ~
     ~<Return>  -- ignores a carriage return or line feed
     ~A         -- writes a lisp object with escape nil
     ~C         -- writes a character with escape nil
     ~D         -- writes a number in base 10
     ~S         -- writes a lisp object with escape t
     ~I         -- inserts a lisp form

Arguments to directives are supported only as indicated." 
(case stream
  ((nil)
   `(with-output-to-string (stream)
     ,. (%build-fast-format-code 'stream format-string format-args)))
  ((t)
   `(let ((stream *standard-output*))
     ,.(%build-fast-format-code 'stream format-string format-args)))
  (t `(let ((stream ,stream))
       ,.(%build-fast-format-code 'stream format-string format-args)))))

(defun force-style (new-styles current-styles)
  (let ((styles-to-add ())
	(styles-to-remove ()))
    (loop for new-style in new-styles
	  as found = (find (first new-style) current-styles :key #'first)
	  do
	  (cond
	    ((not found)(push new-style styles-to-add))
	    ((equal (second found) (second new-style)) nil)
	    (t (push (first new-style) styles-to-remove)(push new-style styles-to-add))))
    (if styles-to-remove
      (loop for style in current-styles
	    unless (member (first style) styles-to-remove :test #'eq)
	       collect style into l
	    finally (return (nconc styles-to-add l)))
      (nconc styles-to-add new-styles))))

(defconstant +crlf+ (string #\newline) #+nil(make-array 2 :element-type 'base-char :initial-contents '(#\Return #\Newline)))

(defun write-http-newline (stream)
  (write-string +crlf+ stream))

(export 'universal-time-to-string)

(defun universal-time-to-string (time &optional (time-zone 0))
  (if time
      (let ((*print-pretty* nil))
        (multiple-value-bind
              (sec min hour date month year day-of-week dsp tz)
            (decode-universal-time time time-zone)
          (declare (ignore tz dsp))
          (format nil
                  "~a, ~2,'0d ~a ~d ~2,'0d:~2,'0d:~2,'0d~@[ GMT~]"
                  (svref '#("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") day-of-week)
                  date
                  (svref '#(nil "Jan" "Feb" "Mar" "Apr" "May" "Jun"
                            "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") month)
                  year hour min sec (= 0 time-zone))))
      ""))
  
(export 'universal-time-to-date)
(defun universal-time-to-date (time lang &optional (time-zone 0))
  (if time
      (let ((*print-pretty* nil))
        (multiple-value-bind (sec min hour date month year day-of-week dsp tz)
            (decode-universal-time time time-zone)
          (declare (ignore hour min sec tz dsp))
          (case lang
            (:fr 
             (format nil "~a ~2,'0d ~a ~d"
                     (svref '#("Lundi" "Mardi" "Mercredi" "Jeudi" "Vendredi" "Samedi" "Dimanche") day-of-week)
                     date
                     (svref '#(nil "Janvier" "Février" "Mars" "Avril" "Mai" "Juin"
                               "Juillet" "Août" "Septembre" "Octobre" "Novembre" "Décembre") month) year))
            (t 
             (format nil "~a, ~2,'0d ~a ~d"
                     (svref '#("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday") day-of-week)
                     date
                     (svref '#(nil "January" "February" "March" "April" "May" "June"
                               "July" "August" "September" "October" "November" "December") month) year)))))
      ""))
  
  
