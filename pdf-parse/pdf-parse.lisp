(in-package pdf)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "parsergen"))

(defvar *pdf-read-stream* nil)
(defvar *lex-val* nil)
(defvar *force-eof* nil)

(defclass raw-object (indirect-object)
  ())

(defmethod write-object ((obj raw-object) &optional root-level)
  (if root-level
    (progn
      (vector-push-extend (format nil "~10,'0d ~5,'0d n "
				  (file-position *pdf-stream*)(gen-number obj))
			  *xrefs*)
      (write-sequence (content obj) *pdf-stream*))
    (format *pdf-stream* "~d ~d R" (obj-number obj)(gen-number obj))))

(defconstant +white-char+ (concatenate 'string '(#\Space #\Newline #\Return #\Tab #\Null #\Page)))
(defmacro white-char-p (c)
  `(find c, +white-char+))

(defun eat-comment ()
  (loop for c = (read-char *pdf-read-stream* nil *pdf-read-stream*)
	until (or (eq c *pdf-read-stream*)(eq c #\Newline))))

(defun eat-string ()
  (let ((string (make-array 10 :adjustable t :fill-pointer 0
			    :element-type 'standard-char)))
    (vector-push-extend #\( string)
    (loop with level = 1
	  for c = (read-char *pdf-read-stream* nil *pdf-read-stream*)
	  when  (eq c *pdf-read-stream*) return string
	  when (char= c #\)) do (decf level)
	  when (char= c #\() do (incf level)
	  do (vector-push-extend c string)
	  when (zerop level) return string)))

(defun eat-stream (length)
  (let ((stream (make-string length)))
    (read-sequence stream *pdf-read-stream*)
    stream))

#+nil(defun pdf-lex0 ()
  (let ((string :eof))
    (labels ((trig-char (c tc token)
	       (when (char= c tc)
		 (if (eq string :eof)
		   (return-from pdf-lex0 token)
		   (progn (unread-char c *pdf-read-stream*)
			  (return-from pdf-lex0 string)))))
	     (trig2-char (c1 c2 tc1 tc2 token)
	       (when (and (char= c1 tc1)(char= c2 tc2))
		 (if (or (eq string :eof) (= (length string) 1))
		   (return-from pdf-lex0 token)
		   (progn (unread-char c1 *pdf-read-stream*)(unread-char c2 *pdf-read-stream*)
			  (return-from pdf-lex0 string))))))
      (loop for prev-c = #\Space then c
	    for c = (read-char *pdf-read-stream* nil *pdf-read-stream*)
	    for eof = (eq c *pdf-read-stream*)
	    for prev-white-char = t then white-char
	    for white-char = (or eof (white-char-p c))
	    when (or eof (and white-char (not prev-white-char))) do (return string)
	    when (char= c #\%) do (eat-comment)(setf white-char t)
	    when (char= c #\() do (setf *lex-val* (eat-string)) (return :string)
	    do
	    (trig-char c #\[ :start-array)
	    (trig-char c #\] :end-array)
	    (trig2-char c prev-c #\< #\< :start-dict)
	    (trig2-char c prev-c #\> #\> :end-dict)
	    when (and prev-white-char (not white-char))
	    do (setf string (make-array 5 :adjustable t :fill-pointer 0
					:element-type 'standard-char))
	    unless white-char do (vector-push-extend c string)))))

(defun pdf-lex ()
  (if *force-eof*
    nil
    (let ((token (pdf-lex0)))
      (cond
	((eq token :eof) nil)
	((symbolp token) token)
	((every #'digit-char-p token)(setf *lex-val*(parse-integer token)) :integer)
	((string= token "R") :R)
	((string= token "n") :n)
	((string= token "f") :f)
	((string= token "obj") :obj)
	((string= token "endobj") :endobj)
	((string= token "stream") :stream)
	((string= token "endstream") :endstream)
	((string= token "true")  :true)
	((string= token "false") :false)
	((string= token "xref") :xref)
	((string= token "trailer") :trailer)
	((string= token "null") :null)
	(t (setf *lex-val* token) (print token) :token)))))

(defun parse-pdf (file)
  (with-open-file (s file :direction :input :external-format '(:default :eol-style :lf))
    (parse-pdf-stream s)))

(defun parse-pdf-stream (*pdf-read-stream*)
  (let ((file-length (file-length *pdf-read-stream*))
	(trailer (make-string 100))
	startxref startxref-pos)
    (file-position *pdf-read-stream* (- file-length 100))
    (read-sequence trailer *pdf-read-stream*)
    (setf startxref-pos (search "startxref" trailer))
    (when startxref-pos
      (setf startxref (parse-integer trailer :start (+ startxref-pos 10) :junk-allowed t))
      (file-position *pdf-read-stream* startxref)
      (let* ((trailer (parse-trailer))
	     (trailer-dict (fourth trailer))
	     (xref-first (first trailer))
	     (nb-xrefs (second trailer))
	     (xrefs (third trailer))
	     (sorted-pos ())
	     (last-object (get-dict-value trailer-dict "/Size"))
	     (*document* (make-instance 'document :empty t))
	     (objects (make-array (1+ last-object) :fill-pointer (1+ last-object) :adjustable t
				  :initial-element nil)))
      (setf (objects *document*) objects)
      (setf sorted-pos (sort (cons startxref (loop for (pos gen type) in xrefs
						   when (eq type :n) collect pos)) #'<))
      (loop repeat nb-xrefs
	for object-number from xref-first
	for (pos gen type) in xrefs
	when (eq type :n) do (setf (aref objects object-number)
				   (read-raw-object pos object-number gen sorted-pos)))
      (let* ((cat-number (obj-number (get-dict-value trailer-dict "/Root")))
	     (catalog (parse-raw-object (aref objects cat-number)))
	     (pages-number (obj-number (get-dict-value (content catalog) "/Pages")))
	     (pages (parse-raw-object (aref objects pages-number))))
	(setf pages (change-class pages 'page-node))
	(setf (pages pages) (get-dict-value (content pages) "/Kids"))
	(change-dict-value (content pages) "/Count" #'(lambda () (length (pages pages))))
	(setf (root-page *document*) pages)
	(setf (last-object-number *document*) last-object)
	(setf (catalog *document*) catalog))
      *document*))))

(defun parse-trailer ()
  (let ((*force-eof* nil))
    (pdf-object-parser 'pdf-lex)))

(parsergen:defparser pdf-object-parser
  ((Start pdf-data) $1)
  ((pdf-data trailer) $1)
  ((pdf-data object) $1)
  ((trailer :xref first last xrefs :trailer dictionary) (setf *force-eof* t)(list $2 $3 $4 $6))
  ((first integer) $1)
  ((last integer) $1)
  ((xrefs xref xrefs) (cons $1 $2))
  ((xrefs xref) (list $1))
  ((xref integer integer xref-type) (list $1 $2 $3))
  ((xref-type :n) :n)
  ((xref-type :f) :f)
  ((object number gen-number :obj content :endobj) (setf *force-eof* t)(list $1 $2 $4))
  ((number integer) $1)
  ((gen-number integer) $1)
  ((content val) $1)
  ((integer :integer) *lex-val*)
  ((token :token) *lex-val*)
  ((dictionary :start-dict key-val* :end-dict) (%make-dictionary $2))
  ((dictionary :start-dict :end-dict) (%make-dictionary nil))
  ((key-val* key-val key-val*) (cons $1 $2))
  ((key-val* key-val) (list $1))
  ((key-val key val) (cons $1 $2))
  ((key-val key object-ref) (cons $1 $2))
  ((key token) $1)
  ((val token) $1)
  ((val integer) $1)
  ((val :name) *lex-val*)
  ((val :string) *lex-val*)
  ((val :true) :true)
  ((val :false) :false)
  ((val stream) $1)
  ((val dictionary) $1)
  ((val array) $1)
  ((array :start-array values :end-array) (convert-to-array $2))
  ((values val values) (cons $1 $2))
  ((values :r values) (cons :r $2))
  ((values val) (list $1))
  ((values :r) (list :r))
  ((stream  start-stream :endstream) $1)
  ((start-stream :start-dict key-val* :end-dict :stream)(%make-stream $2))
  ((object-ref integer integer :R) (%make-obj-ref $1 $2))
  )

(defun %make-stream (key-vals)
  (let ((dict (make-instance 'pdf-stream :empty t)))
    (setf (dict-values dict) key-vals)
    (setf (content dict) (eat-stream (get-dict-value dict "/Length")))
    dict))

(defun %make-dictionary (key-vals)
  (make-instance 'dictionary :dict-values key-vals))

(defun %make-obj-ref (obj-number gen-number)
  (make-instance 'object-ref :obj-number obj-number :gen-number gen-number))

(defun convert-to-array (values)
  (loop with array = (make-array 10 :fill-pointer 0 :adjustable t) and gen and in-ref
	for val in (nreverse values)
	when (eq in-ref :gen) do (setf val (%make-obj-ref val gen))(setf in-ref nil)
	when (eq in-ref :r) do (setf gen val)(setf in-ref :gen)
	when (eq val :r) do (setf in-ref :r)
	unless in-ref do (vector-push-extend val array)
	finally return (nreverse array)))

(defun parse-object (position)
  (file-position *pdf-read-stream* position)
  (let* ((*force-eof* nil)
	 (data (pdf-object-parser 'pdf-lex))
	 (object (aref (objects *document*) (first data)))
	 (content (third data))
	 dict)
    (setf (gen-number object) (second data))
    (setf (content object) content)
    ))

(defun parse-raw-object (raw-obj)
  (let ((obj-num (obj-number raw-obj)))
    (when (typep raw-obj 'object-ref)
      (setf raw-obj (aref (objects *document*) obj-num)))
    (with-input-from-string (*pdf-read-stream* (content raw-obj))
      (let* ((*force-eof* nil)
	     (data (pdf-object-parser 'pdf-lex))
	     (new-obj (make-instance 'indirect-object :no-link t :obj-number obj-num
				     :gen-number (gen-number raw-obj) :content (third data))))
	(setf (aref (objects *document*) obj-num) new-obj)
	new-obj))))

(defun open-page (page-num before)
  (let* ((page-obj-num (obj-number (aref (pages *root-page*) page-num)))
	 (page (parse-raw-object (aref (objects *document*) page-obj-num)))
	 (dict (content page))
	 (resources (get-dict-value dict "/Resources")))
    (when (typep resources 'object-ref)
      (let* ((ref-obj-num (obj-number resources)))
	(setf resources (content (parse-raw-object resources)))))
    (let ((fonts (get-dict-value resources "/Font"))
	  (xobjects (get-dict-value resources "/Font"))
	  (content-stream (make-instance 'pdf-stream))
	  (original-content (get-dict-value dict "/Contents")))
      (change-class page 'page)
      (unless fonts
	(setf fonts (make-instance 'dictionary))
	(push (cons "/Font" fonts) (dict-values resources)))
      (unless xobjects
	(setf xobjects (make-instance 'dictionary))
	(push (cons "/XObject" xobjects) (dict-values resources)))
      (setf (bounds page)(get-dict-value dict "/MediaBox")
	    (resources page) resources
	    (font-objects page) fonts
	    (xobjects page) xobjects
	    (content-stream page) content-stream)
      (change-dict-value dict "/Contents"
			 (if (vectorp original-content)
			   (concatenate 'vector original-content 
					(vector (make-instance 'indirect-object :content content-stream)))
			   (vector original-content
				   (make-instance 'indirect-object :content content-stream))))
      page)))

(defun read-raw-object (position number gen sorted-positions)
  (file-position *pdf-read-stream* position)
  (let* ((last-pos (find position sorted-positions :test #'<))
	 (length (- last-pos position))
	 (content (make-string length)))
    (read-sequence content *pdf-read-stream*)
    (make-instance 'raw-object :content content :no-link t
		   :obj-number number :gen-number gen)))

(defmacro with-existing-document ((file) &body body)
  `(let* ((*document* (parse-pdf ,file))
	  (*root-page* (root-page *document*)))
    ,@body))
(export 'with-existing-document)

(defmacro with-existing-page ((page-number before) &body body)
  `(let* ((*page* (open-page ,page-number ,before)))
    (setf (content (content-stream *page*))
     (with-output-to-string (*page-stream*)
			    ,@body))))
(export 'with-existing-page)


#|

(pdf:with-existing-document (#P"/tmp/2042.pdf")
    (pdf:with-page ()
      (let ((helvetica (make-instance 'pdf:font)))
	(pdf:add-fonts-to-page helvetica)
	(pdf:in-text-mode
	 (pdf:set-font helvetica 36.0)
	 (pdf:move-text 100 800)
	 (pdf:draw-text "cl-pdf: Example 1"))
	(pdf:translate 230 500)
	(loop repeat 101
	      for i = 0.67 then (* i 1.045)
	      do (pdf:in-text-mode
		  (pdf:set-font helvetica i)
		  (pdf:move-text (* i 3) 0)
		  (pdf:draw-text "rotation"))
	      (pdf:rotate 18))))
    (pdf:write-document #P"/t.pdf"))

(pdf:with-existing-document (#P"/tmp/2042.pdf")
    (pdf:with-existing-page (0 nil)
      (let ((helvetica (make-instance 'pdf:font)))
	(pdf:add-fonts-to-page helvetica)
	(pdf:in-text-mode
	 (pdf:set-font helvetica 36.0)
	 (pdf:move-text 100 800)
	 (pdf:draw-text "cl-pdf: Example 1"))
	(pdf:translate 230 500)
	(loop repeat 101
	      for i = 0.67 then (* i 1.045)
	      do (pdf:in-text-mode
		  (pdf:set-font helvetica i)
		  (pdf:move-text (* i 3) 0)
		  (pdf:draw-text "rotation"))
	      (pdf:rotate 18))))
    (pdf:with-page ()
      (let ((helvetica (make-instance 'pdf:font)))
	(pdf:add-fonts-to-page helvetica)
	(pdf:in-text-mode
	 (pdf:set-font helvetica 36.0)
	 (pdf:move-text 100 800)
	 (pdf:draw-text "cl-pdf: Example 1"))
	(pdf:translate 230 500)
	(loop repeat 101
	      for i = 0.67 then (* i 1.045)
	      do (pdf:in-text-mode
		  (pdf:set-font helvetica i)
		  (pdf:move-text (* i 3) 0)
		  (pdf:draw-text "rotation"))
	      (pdf:rotate 18))))
    (pdf:write-document #P"/t.pdf"))

|#