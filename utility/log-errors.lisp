(in-package utility)

(export '(with-logged-errors *ignore-logged-errors* *disable-error-logging* *disable-error-backtrace*
	*error-log-file* *error-hooks-fn*))

(defvar *ignore-logged-errors* nil)
(defvar *disable-error-logging* nil)
(defvar *disable-error-backtrace* nil)
(defvar *error-log-file* #P"/tmp/lisp-errors.txt")
(defvar *error-hooks-fn* nil) ;list of (lambda (error backtrace)...)

(defun universal-time-to-stream (time stream)
  (let ((*print-pretty* nil))
    (multiple-value-bind
	  (sec min hour date month year day-of-week dsp tz)
	(decode-universal-time time)
      (declare (ignore tz dsp))
      (format stream
	      "~a, ~2,'0d ~a ~d ~2,'0d:~2,'0d:~2,'0d"
	      (svref '#("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") day-of-week)
	      date
	      (svref '#(nil "Jan" "Feb" "Mar" "Apr" "May" "Jun"
			"Jul" "Aug" "Sep" "Oct" "Nov" "Dec") month)
	      year hour min sec))))

(defun dump-error-backtrace (e)
  (when (and (not *disable-error-logging*) (or *error-log-file* *error-hooks-fn*))
    (with-standard-io-syntax
	(let* ((*print-readably* nil)
	       (*print-level* nil)
	       (backtrace
		(progn #+nil ignore-errors
		  (with-output-to-string (*debug-io*)
		    (format *debug-io* "~%~%****************************************************~%")
  		    (universal-time-to-stream (get-universal-time) *debug-io*)
		    (format *debug-io* "  Error : ~a~%" e)
		    (unless *disable-error-backtrace*
		      (format *debug-io* "****************************************************~%")
		      (dbg:with-debugger-stack ()
			(dbg:bug-backtrace nil :printer-bindings
					   '((*print-level*)(*print-length* . 100)))))))))
	  (unless backtrace (setf backtrace "Ultimate-error"))
	  (when *error-log-file*
	    (ignore-errors
	      (with-open-file (s *error-log-file* :direction :output
				 :if-exists :append :if-does-not-exist :create)
		(write-string backtrace s))))
	  (loop for hook-fn in *error-hooks-fn* do
		(ignore-errors (funcall hook-fn e backtrace)))))))

(defun call/backtrace (fn)
  (declare (dynamic-extent fn)
	   (type function fn))
  (if *disable-error-logging*
      (funcall fn)
      (handler-bind
	  ((error #'(lambda (e)
		      (dump-error-backtrace e))))
	(funcall fn))))

(defmacro with-logged-errors ((&key (ignore-errors '*ignore-logged-errors*))
			      &body forms)
  (with-gensyms (logged-error-fn)
    `(let ((,logged-error-fn #'(lambda () ,@forms)))
      (declare (dynamic-extent ,logged-error-fn))
      (if ,ignore-errors
	  (ignore-errors (call/backtrace ,logged-error-fn))
	  (call/backtrace ,logged-error-fn)))))

