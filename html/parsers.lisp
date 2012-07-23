(in-package html)

(defun parse-url-search-part (url &optional (start 0) (end (length url)))
  (let ((start (position #\? url :start start :end end)))
    (when start
      (incf start)
      (loop for s1 = start then (1+ (the fixnum e2))
	    while (< s1 end)
	    for e1 = (position #\= url :start s1 :end end)
	    while e1
		; (handle-bad-form-encoding url start end))
	    for s2 = (1+ (the fixnum e1))
	    for e2 = (or (position #\& url :start s2 :end end) end)
	    for keyword = (subseq url s1 e1)
	    for value = (unless (= s2 e2)
			  (string-unescape-url-chars url s2 e2))
	    collect `(,keyword . ,value))
      )))

(defun parse-url-posted-content (url &optional (start 0) (end (length url)))
  (when start
    (loop for s1 = start then (1+ (the fixnum e2))
	  while (< s1 end)
	  for e1 = (position #\= url :start s1 :end end)
	  while e1
		; (handle-bad-form-encoding url start end))
	  for s2 = (1+ (the fixnum e1))
	  for e2 = (or (position #\& url :start s2 :end end) end)
	  for keyword = (subseq url s1 e1)
	  for value = (unless (= s2 e2)
			(nsubstitute #\Space #\+ url  :start s2 :end e2)
			(string-unescape-url-chars url s2 e2))
	  collect `(,keyword . ,value))
    ))

(defun parse-posted-content (content content-type)
  (cond
    ((string= (caar content-type) "application/x-www-form-urlencoded")
     (parse-url-posted-content content))
    ((string= (caar content-type) "multipart/form-data")
     (decode-mime-multipart content content-type))))

(defun string-unescape-url-chars (string &optional (start 0) (end (length string) end-supplied-p)
                                              (only-safe-characters-p nil) &aux new-string)
  "When any escaped characters are present, this returns a string with these characters unescaped.
A new string is consed only when escaped characters are present.
ONLY-SAFE-CHARACTERS-P set to non-nill skips reserved or unsafe URL characters."
  (declare (values unescaped-string chars-unescaped-p new-string-returned-p)
           (fixnum start end))
  (loop with idx fixnum = start
	and last-idx fixnum = start
	and new-idx fixnum = start
	and new-char
	and new-char-code
	and offset
	while (< idx end)
	for char = (aref string idx)
	when (char= char #\%)
	do (setf new-char-code (parse-integer string :radix 16 :start (1+ idx) :end (+ idx 3)))
	(setf new-char 
	      (code-char
	       (logand 255
		       (cond
			 ((zerop (logand #x80 new-char-code)) (setf offset 3) new-char-code)
			 ((= #xc0 (logand #xe0 new-char-code))(setf offset 6)
			  (logior (ash (logand new-char-code #x1f) 6)
				  (logand (parse-integer string :radix 16 :start (+ idx 4) :end (+ idx 6)) #x3f)))
			 ((= #xe0 (logand #xf8 new-char-code))(setf offset 9)
			  (logior (logior (ash (logand new-char-code #x0f) 12)
					  (ash (logand (parse-integer string :radix 16 :start (+ idx 4) :end (+ idx 6)) #x3f) 6))
				  (logand (parse-integer string :radix 16 :start (+ idx 7) :end (+ idx 9)) #x3f)))
			 ((= #xf0 (logand #xf8 new-char-code)(setf offset 12))
			  )
			 (t new-char-code)))))
	(cond  ;; Skip unescaping a char, just incf idx to skip the hex pair.
	  ;; Escape a char, we have already started a new string.
	  (new-string 
	   (let ((new-idx2 (+ new-idx (- idx last-idx))))
	     (setf new-string (replace new-string string :start1 new-idx :end1 new-idx2 :start2 last-idx :end2 idx)
		   (aref new-string new-idx2) new-char
		   new-idx (1+ (the fixnum new-idx2))
		   last-idx (incf idx offset))))
	  ;; Escape a char, need to start a new string.
	  (t (setf new-idx (- idx start)
		   new-string (replace (make-array (- end start 2) :fill-pointer t :element-type 'standard-char)
				       string :start1 0 :end1 new-idx :start2 start :end2 idx)
		   (aref new-string new-idx) new-char
		   last-idx (incf idx offset))
                    (incf new-idx)))
	else
	do (incf idx)
	finally (return (cond ;; We've started a new string, now finish up
			  (new-string 
			   (let ((new-end (+ (the fixnum new-idx) (- end last-idx))))
			     (setf new-string (replace new-string string :start1 new-idx :end1 new-end :start2 last-idx :end2 end)
				   (fill-pointer new-string) new-end))
			   (values new-string t t))
			  ;; No escaping was performed
			  ((and (zerop start) (or (not end-supplied-p) (= end (length string))))
			   (values string nil nil))
			  ;; Trim original as necessary
			  (t (values (subseq string start end) nil t))))))

(defun get-header-value (value-name header)
  (cdr (assoc value-name header :test #'string=)))

(defvar *white-space* (make-array 4 :element-type 'base-char
				  :initial-contents '(#\Space #\Tab #\Return #\Newline)))

(defun white-space-p (c)
  (find c *white-space*))

(defun parse-header-values (string)
  (let ((list (mapcar #'(lambda (s) (string-trim *white-space* s))
		      (split-sequence:split-sequence #\; string))))
    (loop for s in list
	  for pos = (position #\= s)
	  if pos collect (cons (string-trim *white-space* (subseq s 0 pos))
			       (string-trim *white-space* (subseq s (1+ pos))))
	  else collect (cons s nil))))

(defun collect-headers (string &optional (start 0))
  (let ((end-of-header nil)
	(header-pos ())
	(headers ()))
    (loop for stop = (search +crlf+ string :start2 start)
	  for start2 = (when stop (+ stop 2))
	  while (and stop (> stop start))
	  collect (list start stop) into list
	  do (setf start start2)
	  finally (setf header-pos list end-of-header start2))
    (loop with key = nil and values = ()
	  for (start stop) in header-pos
	  do
	  (if (white-space-p (aref string start))
	      (push (subseq string start stop) values)
	      (progn
		(when key
		  (push (cons key (parse-header-values
				   (apply 'concatenate 'string (nreverse values)))) headers)
		  (setf values nil))
		(let ((pos (min (position #\: string :start start) stop)))
		  (setf key (subseq string start pos)
			pos (min (+ pos 2) stop)
			values (list (subseq string pos stop))))))
	  finally (when key
		  (push (cons key (parse-header-values
				   (apply 'concatenate 'string (nreverse values)))) headers)
		  (setf values nil)))
    (values (nreverse headers) end-of-header)))

(defun decode-mime-multipart (content content-type)
  (let* ((boundary (get-header-value "boundary" content-type))
	 (start-boundary (concatenate 'string "--" boundary))
	 (start-boundary-length (+ (length start-boundary) 2))
	 (decoded-content nil))
    (loop with pos1 = (search start-boundary content)
	  for pos2 = (when pos1 (search start-boundary content :start2 (+ pos1 start-boundary-length)))
	  while (and pos1 pos2)
	  do (multiple-value-bind (headers end-of-header)
		(collect-headers content (+ pos1 start-boundary-length))(print headers)
	       (let* ((dispostion (get-header-value "Content-Disposition" headers))
		      (name (get-header-value "name" (cdr dispostion)))
		      (filename (get-header-value "filename" (cdr dispostion))))
		 (when name
		   (setf name (string-trim "\"" name))
		   (push (cons name
			       ;(make-array 
			       (subseq content end-of-header (- pos2 2))
			       ) decoded-content))
		 (when filename
		   (push (cons (concatenate 'string name "-filename")(string-trim "\"" filename))
			 decoded-content))))
	  (setf pos1 pos2))
    (nreverse decoded-content)))

