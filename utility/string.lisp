(in-package utility)

#|
;;;;;;;;;;; string manipulations


(defun string-empty-p (str)
  (string-equal str ""))


;;;;;;;; finding tokens


(defun tok-after-str (text prefix-str)
  "returns the next non-whitespace token that occurs in TEXT after
the first occurrence of STR"
  (let ((str-pos (search prefix-str text)))
    (when (numberp str-pos)
      (let* ((start (position-if-not #'whitespace-p text
				     :start (+ str-pos (length prefix-str))))
	     (end (position-if #'whitespace-p
			       text :start start)))
	(subseq text start end)))))


(defun last-token-in-line (line)
  "returns the substring of LINE after the last whitespace"
  (let* ((end (position-if-not #'whitespace-p line :from-end t))
	 (start (position-if #'whitespace-p line :from-end t :end end)))
    (subseq line (if start (1+ start) 0)
	    (when end (1+ end)))))

|#
;;;;;;;;;; substrings


(defun subseq-before (suffix-str text)
  "returns the substring of TEXT before the last occurrence of SUFFIX-STR"
  (let ((str-pos (search suffix-str text :from-end t)))
    (when str-pos
      (subseq text 0 str-pos))))


(defun subseq-after (prefix-str text)
  "returns the substring of TEXT after the last occurrence of PREFIX-STR"
  (let ((str-pos (search prefix-str text :from-end t)))
    (when str-pos
      (subseq text (+ str-pos (length prefix-str))))))


(defun subseq-before-any (suffix-str text)
  (let ((str-pos (search suffix-str text :from-end t)))
    (if str-pos
	(subseq text 0 str-pos)
      text)))


(defun subseq-after-any (prefix-str text)
  (let ((str-pos (search prefix-str text :from-end t)))
    (if str-pos
	(subseq text (+ str-pos (length prefix-str)))
      text)))


(defun seq-starts-with (initial-element seq
			&key (test #'eql))
  (funcall test initial-element (elt seq 0)))


(defun seq-starts-as (prefix-str text)
  "returns non-nil iff TEXT starts as STR"
  (let ((diff-pos (mismatch prefix-str text)))
    (or (null diff-pos)
	(>= diff-pos (length prefix-str)))))


(defun seq-ends-as (suffix-str text)
  "returns non-nil iff TEXT ends as SUFFIX-STR.  Works for any sequence, not
just strings, but optimized for vectors."
  (let ((diff-pos (mismatch text suffix-str :from-end t)))
    (or (null diff-pos)
	(<= diff-pos (- (length text)
			(length suffix-str))))))


(defun strip-suffix (suffix string)
  "returns string without suffix, nil if suffix not present"
  (when (seq-ends-as suffix string)
    (let ((l (length string))
	  (s (length suffix)))
      (subseq string 0 (- l s)))))

#|
(defun common-prefix (str1 str2)
  (subseq str1 0 (mismatch str1 str2)))


(defun remove-common-prefix (list-of-strs)
  "returns the common prefix and a list of reduced strings"
  (let* ((pref (reduce #'common-prefix list-of-strs))
	 (pref-len (length pref)))
    (values pref
	    (mapcar #'(lambda (s) (subseq s pref-len))
		    list-of-strs))))


;; no keyword args makes this about twice as fast (for shortish strings,
;; at least) as it would otherwise be
;;
(defun fast-string-replace (target source start1 end1 start2 end2)
  "copies source into target, stopping when shorter length is covered.
 No error checking!"
  (declare (optimize (speed 3) (safety 1) (debug 0))
	   (type simple-string target source)
	   (type fixnum start1 end1 start2 end2))
  ;; (assert (= (- end1 start1) (- end2 start2)))
  ;;  (format t "Copying ~D-~D=~D (~S) into ~D-~D=~D (~S).~%"
  ;;      start2 end2 (- end2 start2) (subseq source start2 (max end2 start2))
  ;;      start1 end1 (- end1 start1) (subseq target start1 (max end1 start1)))
  (until (or (= start1 end1)
	     (= start2 end2))
	 (setf (schar target start1)
	   (schar source start2))
	 (incf start1)
	 (incf start2))
  target)


;;;;;;;;;;; splitting on characters


;; this and next from cl-lib, contributed by mkant@cs.cmu.edu
(defun string-search-car (character-bag string)
  "Returns the part of the string before the first of the delimiters in 
   CHARACTER-BAG and the delimiter."
  (let* ((delimiter nil)
	 (delimiter-position (position-if #'(lambda (character)
					      (when (find character 
							  character-bag)
						(setq delimiter character)))
					  string)))
    (values (subseq string 0 delimiter-position)
	    delimiter)))


(defun string-search-cdr (character-bag string)
  "Returns the part of the string after the first of the delimiters in 
   CHARACTER-BAG, if any, and the delimiter. If none of the delimiters 
   are found, returns NIL and NIL."
  (let* ((delimiter nil)
	 (delimiter-position (position-if #'(lambda (character)
					      (when (find character 
							  character-bag)
						(setq delimiter character)))
					  string)))
    (if delimiter-position
	(values (subseq string (1+ delimiter-position))
		delimiter)
      ;; Maybe this should be "" instead of NIL?
      (values nil delimiter))))


(defun split-seq-on (str &optional (ch #\Space))
  "returns a list of strings formed by breaking STR at every occurance
of CH (which is not included).  Works for any sequence, not just strings,
but optimized for vectors."
  (when str
    (do* ((prev-pos 0 (1+ next-pos))
	  (next-pos (position ch str)
		    (position ch str :start prev-pos))
	  (stuff (list (subseq str 0 next-pos))
		 (cons (subseq str prev-pos next-pos)
		       stuff)))
	((null next-pos) (nreverse stuff)))))


;; definition of +whitespace+ moved to data-utils.lisp (next to definition
;; of member-of-constant-set) so it will be loaded in time for definition
;; of WHITESPACE-P


(defun split-seq-on-bag (str &optional (bag +whitespace+))
  "returns a list of strings formed by breaking STR at every occurance
of any element of BAG.  Works for any sequence, not just strings,
but optimized for vectors."
  (when str
    (flet ((in-bag (char)
	     (member char bag)))
      (do* ((prev-pos 0 (1+ next-pos))
	    (next-pos (position-if #'in-bag str)
		      (position-if #'in-bag str :start prev-pos))
	    (stuff (list (subseq str 0 next-pos))
		   (cons (subseq str prev-pos next-pos)
			 stuff)))
	  ((null next-pos) (nreverse stuff))))))


(defun split-seq-using (str &optional (ch #\Space))
  "returns a list of strings.  Ignores multiple delimiters."
  (when str
    (do* ((prev-pos (position ch str :test-not #'eql)
		    (position ch str :test-not #'eql :start next-pos))
	  (next-pos (when prev-pos (position ch str :start prev-pos))
		    (when prev-pos (position ch str :start prev-pos)))
	  (stuff (when prev-pos
		   (list (subseq str prev-pos next-pos)))
		 (if prev-pos
		     (cons (subseq str prev-pos next-pos)
			   stuff)
		   stuff)))
	((null next-pos) (nreverse stuff)))))


(defun split-seq-using-bag (str &optional (bag +whitespace+))
  "returns a list of strings formed by breaking STR. Ignores multiple
delimiters."
  (when str
    (flet ((in-bag (char)
	     (member char bag)))
      (do* ((prev-pos (position-if-not #'in-bag str)
		      (position-if-not #'in-bag str :start next-pos))
	    (next-pos (when prev-pos (position-if #'in-bag str
						  :start prev-pos))
		      (when prev-pos (position-if #'in-bag str
						  :start prev-pos)))
	    (stuff (when prev-pos
		     (list (subseq str prev-pos next-pos)))
		   (if prev-pos
		       (cons (subseq str prev-pos next-pos)
			     stuff)
		     stuff)))
	  ((null next-pos) (nreverse stuff))))))


;;;;;;;;;;;;; misc


(defun date-string ()
  "prints a representation of the curren time and date to the given stream"
  (multiple-value-bind
      (sec min hour day mon year) (get-decoded-time)
    (format nil "~D:~2,'0D:~2,'0D on ~D/~D/~D (M/D/Y)"
	    hour min sec mon day year)))


(defun short-date-string ()
  "prints a representation of the curren time and date to the given stream"
  (multiple-value-bind
      (sec min hour day mon) (get-decoded-time)
    (declare (ignore sec))
    (format nil "~D:~2,'0D, ~D/~D"
	    hour min mon day)))


(defun whitespace-p (ch)
  (declare (optimize (speed 3) (safety 1) (debug 0))
	   (type character ch))
  (member-of-constant-set ch +whitespace+ :test #'char=))


(defun string-trim-white (string)
  "returns a copy of STRING w/o whitespace at front or rear"
  (string-trim +whitespace+ string))
|#

;;; EOF - strings.lisp
