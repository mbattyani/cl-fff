(in-package #:interface)

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

(defconstant *special-character-translation-alist*
    '((#\> . "&gt;")
      (#\< . "&lt;")
      (#\& . "&amp;")
      (#\" . "&quot;")
      (#\space . "&nbsp;"))
    "&; delimited tokens are used to print special tokens.")

(defconstant *max-length-quotation-token* 6)

(defun write-string-quoting-specials (string &optional (stream *layout-stream*) (start 0) end)
  "Writes STRING to STREAM being careful to translated any special characters for HTML."
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

;; moved from http:server;html2.lisp due to compile order problems   7/3/96 -- JCMa.
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
	    finally return (nconc styles-to-add l))
      (nconc styles-to-add new-styles))))

(defun write-http-newline (stream)
  (write-char #\Return stream)
  (write-char #\Newline stream))


	  
