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

(export 'split-sequence)
(defun split-sequence (delimiter seq
                       &key (maximum nil)
                            (keep-empty-subseqs nil)
                            (from-end nil)
                            (start 0)
                            (end nil)
                            (test nil test-supplied)
                            (test-not nil test-not-supplied)
                            (key nil key-supplied))
 "Return a list of subsequences in <seq> delimited by <delimiter>.
If :keep-empty-subseqs is true, empty subsequences will be included 
in the result; otherwise they will be discarded.
If :maximum is supplied, the result will contain no more than :maximum 
(possibly empty) subsequences. The second result value contains the 
unsplit rest of the sequence. 
All other keywords work analogously to those for CL:POSITION."
(let ((len (length seq))
      (other-keys (nconc (when test-supplied 
			   (list :test test))
			 (when test-not-supplied 
			   (list :test-not test-not))
			 (when key-supplied 
			   (list :key key)))))
  (unless end (setq end len))
  (if from-end
      (loop for right = end then left
	    for left = (max (or (apply #'position delimiter seq 
				       :end right
				       :from-end t
				       other-keys)
				-1)
			    (1- start))
	    unless (and (= right (1+ left) )
			(not keep-empty-subseqs)) ; empty subseq we don't want
	    if (and maximum (>= nr-elts maximum))
	    ;; We can't take any more. Return now.
	    return (values subseqs (subseq seq start right))
	    else 
	    collect (subseq seq (1+ left) right) into subseqs
	    and sum 1 into nr-elts
	    until (<= left start)
	    finally return (values subseqs (subseq seq start (1+ left))))
      (loop for left = start then (+ right 1)
            for right = (min (or (apply #'position delimiter seq 
                                        :start left 
                                        other-keys)
                                 len)
                             end)
            unless (and (= right left) 
                        (not keep-empty-subseqs)) ; empty subseq we don't want
            if (and maximum (>= nr-elts maximum))
            ;; We can't take any more. Return now.
            return (values subseqs (subseq seq left end))
            else 
            collect (subseq seq left right) into subseqs
            and sum 1 into nr-elts
            until (= right end)
            finally return (values subseqs (subseq seq right end))))))


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

#|
(defun call-splitting-vector (vector splitter fn
                                     &key (start 0)
                                          (end (length vector))
                                          (test #'eq))
  (when (< start 0)
    (error ":START should be <= 0, not ~S" start))
  (when (> end (length vector))
    (error ":END is out of bounds"))
  (loop with begin = start
        for i from start below end
        when (funcall test splitter (aref vector i))
        do (funcall fn begin i)
        (setf begin (1+ i))
        finally (funcall fn begin i)))

(defmacro do-vector-split ((start end (vector splitter)
                                  &rest keys &key &allow-other-keys)
                           &body forms)
  (when (null start) (setf start (gensym)))
  (when (null end) (setf end (gensym)))
  `(call-splitting-vector ,vector ,splitter
                          #'(lambda (,start ,end) ,@forms)
                          ,@keys))

You can use DO-VECTOR-SPLIT to collect a list of substrings:

  [58]> (let ((result ())
              (string " la dee dah "))
          (do-vector-split (s e (string #\space)
                              :start 1)
            (push (subseq string s e) result))
          (nreverse result))
  ("la" "dee" "dah" "")

Of course, you can always define SPLIT-VECTOR in terms of
CALL-SPLITTING-VECTOR:

  (defun split-vector (vector splitter &rest keys &key &allow-other-keys)
    (let ((result ()))
      (apply #'call-splitting-vector
             vector splitter #'(lambda (s e)
                                 (push (subseq vector s e) result))
             keys)
      (nreverse result)))

But you'll probably only very rarely need it, because you're probably
splitting the string as an intermediate step to some other end
(stuffing numbers into a vector that represents an IP address, for
example), so you may as well avoid consing up new strings and a new
list just to throw them away.
|#

#|
(defun extract-case-keys (case-form)
  (loop for clause in (cddr case-form)
	until (and (eq (car case-form) 'case) (member (car clause) '(t otherwise)))
	if (listp (car clause))
        append (car clause)
	else
        collect (car clause)))

;; export if packaged
(defparameter *with-hashed-identity-body-forms*
  '((case . extract-case-keys)
    (ccase . extract-case-keys)
    (ecase . extract-case-keys))
  "Alist of the valid operators in body forms of a with-hashed-identity form
with their key-extraction function.")

(defun with-hashed-identity-error (body)
  (error "Body form of with-hashed-identity is ~A, but must be one of:~{ ~A~}."
	 (caar body) (mapcar #'car *with-hashed-identity-body-forms*)))

(defun with-hashed-identity-hashtable (hash-table body)
  (dolist (key (funcall (or (cdr (assoc (car body) *with-hashed-identity-body-forms*))
			    'with-hashed-identity-error)
			body))
    (setf (gethash key hash-table) key))
  hash-table)

;; export if packaged
(defmacro with-hashed-identity (hash-options &body body)
  "A wrapper around case forms to enable case tests via a hashtable."
  (unless (and (listp (car body))
	       (null (cdr body)))    ;TODO: Allow multiple body forms.
    (error "Body of with-hashed-identity must be a single form.")
    (let ((hash-table (make-symbol "hashtable")))
      `(let ((,hash-table (load-time-value
			   (with-hashed-identity-hashtable (make-hash-table ,@hash-options)
			     ',(car body)))))
	(,(caar body) (gethash ,(cadar body) ,hash-table) ,@(cddar body))))))
  
  (with-hashed-identity (:test #'equal)
    (case "foo"
      ("foo" 'yeah)
      (t 'bummer)))
  
  (with-hashed-identity (:test #'equalp)
    (case "foo"
    ("FOO" 'yeah)
    (t 'bummer)))
  
  (with-hashed-identity (:test #'equalp)
    (case (vector #\f #\o #\o)
      (#(#\F #\O #\O) 'yeah)
      (t 'bummer)))
|#

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