(in-package utility)

(export 'push-at-end)
(defmacro push-at-end (item place)
  `(setf ,place (nconc ,place (list ,item))))

(export 'with-gensyms)
(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym ,(symbol-name s))))
                 syms)
    ,@body))

(export 'file-exist-p)
(defun file-exist-p (pathname)
  (let ((file (probe-file pathname)))
    (and file (stringp (pathname-name file)))))

(defparameter *drng-seed* 31415)
(defconstant +drng-a+ 1597)
(defconstant +drng-c+ 51749)
(defconstant +drng-m+ 244944)

(defun simple-random ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (setf *drng-seed* (mod (+ (* +drng-a+ *drng-seed*) +drng-c+) +drng-m+)))

(defun crypt-str (string key)
  (let ((*drng-seed* key))
    (map 'string #'(lambda (x)(code-char (logxor (char-code x) (logand (simple-random) 255)))) string)))

(export 'full-symbol-name)
(defun full-symbol-name (symbol)
  (let ((*package* (find-package :keyword)))
    (write-to-string symbol :readably t)))

(defmacro ppmacro (&rest body)
  `(pprint (macroexpand ,@body)))

(defmacro ppmacro-1 (&rest body)
  `(pprint (macroexpand-1 ,@body)))

(defun atoi (string &optional (error-value 0))
  (let ((value 0))
    (ignore-errors (setf value (read-from-string string)))
    (if (numberp value) value error-value)))

(export 'extremum)
(defun extremum (sequence predicate &key (key #'identity))
  (reduce #'(lambda (x y)
	      (if (funcall predicate (funcall key x) (funcall key y))
		  x
                  y))
          sequence))


(defun maximizing-item (func list)
  (let* ((max-item (first list))
	 (max-value (funcall func max-item)))
    (dolist (item (rest list))
      (let ((value (funcall func item)))
	(when (> value max-value)
	  (setf max-value value max-item item))))
    (values max-item max-value)))

(defun minimizing-item (func list)
  (let* ((min-item (first list))
	 (min-value (funcall func min-item)))
    (dolist (item (rest list))
      (let ((value (funcall func item)))
	(when (< value min-value)
	  (setf min-value value min-item item))))
    (values min-item min-value)))


(defun copy-array (array)
  (let ((dims (array-dimensions array)))
    (adjust-array (make-array dims :displaced-to array) dims)))

(eval-when (compile load eval)
  (when (eval '(flet ((ltv () (load-time-value (cons nil nil)))) (eq (ltv) (ltv))))
    (pushnew :eval-uses-compiler *features*)))

(defmacro memoized (form) 
  `(LET ((MEMORY
           (IF #-eval-uses-compiler (EVAL-WHEN (EVAL) T) #+eval-uses-compiler NIL
             ',(cons nil nil)
             ;; Careful: Different expansions of MEMOIZED forms must yield
             ;; LOAD-TIME-VALUE forms that are not EQ, otherwise compile-file
             ;; will coalesce these LOAD-TIME-VALUE forms. Therefore here we
             ;; explicitly cons up the list and don't use backquote.
             ,(list 'LOAD-TIME-VALUE '(CONS NIL NIL)))))
     (UNLESS (CAR MEMORY)
       (SETF (CDR MEMORY) ,form)
       (SETF (CAR MEMORY) T))
     (CDR MEMORY)))


;; a python-style multi-line strings """..."""
#+nil
(eval-when (:execute :load-toplevel :compile-toplevel)
(let ((normal-string-reader (get-macro-character #\")))
  (declare (type function normal-string-reader))
  (defun read-multiline-string (stream c)
    (let ((buffer ()))
      (when (not (char= #\" (peek-char nil stream)))
        (return-from read-multiline-string
          (funcall normal-string-reader stream c)))
      (read-char stream)
      (when (not (char= #\" (peek-char nil stream)))
        (return-from read-multiline-string
          ""))
      (read-char stream)
      (do ((chars (list (read-char stream)
                        (read-char stream)
                        (read-char stream))
                  (cdr (nconc chars (list (read-char stream))))))
          ((every #'(lambda (c) (eq c #\")) chars)
           (coerce (nreverse buffer) 'string))
        (push (car chars) buffer)))))

(set-macro-character #\" #'read-multiline-string))

(defun ap (&rest args)
  (let ((package nil)
  (search-list nil))
    (loop for arg in args do
   (if (and (symbolp arg) (find-package arg))
       (setf package (find-package arg))
       (push arg search-list)))
    (%ap (nreverse search-list) package)))

(defgeneric %ap (thing &optional package))

(defmethod %ap ((thing string) &optional package)
  (let ((*package* (or (and package (find-package package))
           *package*)))
    (apropos-list thing package)))

(defmethod %ap ((thing symbol) &optional package)
  (%ap (symbol-name thing) package))

(defmethod %ap ((thing list) &optional package)
  (cond ((null thing) nil)
  ((null (rest thing)) (%ap (first thing) package))
  (t
   (let ((current (%ap (first thing) package)))
     (dolist (next (rest thing))
       (setf current (intersection current (%ap next package))))
     current))))

(export 'ap)

(defun build-symbol (&rest things)
  "Return a symbol in current package with a name assembled from things"
  (intern
   (format nil "~@:(~{~A~}~)" things)))

(export 'build-symbol)
