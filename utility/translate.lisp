(in-package "UTILITY")

(defvar *default-translation-context-stack* ())
(defvar *translation-hashtable* (make-hash-table :test #'equal))
(defvar *compile-translation* nil)

(export 'translate)

(defmacro translate (string &optional context)
  (unless context (setf context (first *default-translation-context-stack*)))
  (if *compile-translation*
      (%translate string context)
    `(%translate ,string ,context)))

(defun translate-reader (stream subchar arg)
  (declare (ignore arg subchar))
  (let ((first-char (peek-char nil stream t nil t)))
    (cond ((char= first-char #\space)
	   (read-char stream)	  ; skip over whitespace
	   (translate-reader stream nil nil))
	  ((char= first-char #\") ;read one string
	   `(utility:translate ,(read stream t nil t)))
	  ((char= first-char #\() ;read a list
	   (cons 'utility:translate (read stream t nil t)))
	  (t
	   (infix-error "Translate expression starts with ~A only \" or ( are accepted" first-char)))))

;(set-dispatch-macro-character #\# #\T #'translate-reader)

(defun %translate (string context)
  (let ((translation (gethash (cons string context) *translation-hashtable*)))
    (cond
     ((eq translation :none) string)
     ((not translation) (setf (gethash (cons string context) *translation-hashtable*) :none) string)
     (t translation))))

(defun deftranslation (string translated-string context)
  (setf (gethash (cons string context) *translation-hashtable*) translated-string))

(defun write-translations-to-file (filename)
  (with-open-file (s filename :direction :output :if-exists :supersede)
		  (format s "(in-package utility)~%~%")
		  (maphash #'(lambda (key object)
			       (format s "(deftranslation ~S ~S ~S)~%" (first key) object (second key)))
			   *translation-hashtable*)))

