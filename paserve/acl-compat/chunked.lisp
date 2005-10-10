;;;
;;; Streams with support for "chunked" transfer coding.  This module
;;; emulates the support for chunking found in Allegro Common Lisp's
;;; streams.  See RFC 2616 for a description of the "chunked" transfer
;;; coding.
;;;
;;; TODO:
;;;  - 

(defpackage :com.ljosa.chunked
  (:use :common-lisp #+LISPWORKS :stream)
  (:export :chunked-mixin :make-chunked-stream :*buffer-size* 
           :output-chunking :input-chunking :close-chunk))

(in-package :com.ljosa.chunked)

(defparameter *buffer-size* 1024 "Maximum chunk size")

(defvar *recursive* nil)

(defclass chunked-mixin ()
  ((output-chunking :initform nil :accessor output-chunking)
   (input-chunking :initform nil :accessor input-chunking)
   (output-buffer)
   (remaining-input :initform nil)))

(defmethod shared-initialize :after ((stream chunked-mixin) slots-for-initform
				     &rest initargs)
  (declare (ignore initargs slots-for-initform))
  (with-slots (output-buffer) stream
    (setf output-buffer (make-array (list *buffer-size*)
				    :element-type 'unsigned-byte
				    :fill-pointer 0))))

(define-condition excl::socket-chunking-end-of-file (condition)
  ((excl::format-arguments :initform nil)
   (excl::format-control :initform "~1@<The stream ~s had a chunking end of file~:@>")))

;; (defmethod stream-element-type ((stream chunked-mixin))
;;  (call-next-method))

(defun read-chunk-header (stream &aux (x 0) (*recursive* t))
  (tagbody
   s0 (let ((char (read-char stream)))
        (cond ((digit-char-p char 16) (setf x (+ (* 16 x) (digit-char-p char 16)))
                                      (go s0))
              ((eq #\; char) (go s1))
              ((eq #\; char) (go s2))
              (t (error "Parse error in state s0: ~S." char))))
   s1 (if (eq #\Return (read-char stream))
          (go s2)
        (go s1))
   s2 (let ((char (read-char stream)))
        (case char
          (#\Linefeed (go accept))
          (t (error "Parse error in state s2: ~S." char))))
   accept)
  x)

;; FIXME: What do do when the chunked input stream can't be parsed?

(defun gobble-crlf (stream &aux (*recursive* t))
  (flet ((expect (expected-char)
	   (let ((char (read-char stream)))
	     (unless (eq expected-char char)
	       (error "Expected ~C, got ~C." expected-char char)))))
    (expect #\Return)
    (expect #\Linefeed)))

(defmethod stream-read-char ((stream chunked-mixin))
  (with-slots (input-chunking remaining-input output-chunking) stream
    (cond (*recursive* (call-next-method))
          ((not input-chunking) (call-next-method))
          ((not remaining-input) (handler-case 
                                     (progn
                                       (setf remaining-input (read-chunk-header stream))
                                       (stream-read-char stream))
                                   (end-of-file () :eof)))
          ((> remaining-input 0) (decf remaining-input)
                                 (call-next-method))
          ((zerop remaining-input) (handler-case
                                       (progn
					 (gobble-crlf stream)
                                         (setf remaining-input (read-chunk-header stream))
                                         (cond ((zerop remaining-input)
						(setf input-chunking nil
						      output-chunking nil)
                                                (signal 'excl::socket-chunking-end-of-file :format-arguments stream)
                                                :eof)
                                               (t (stream-read-char stream))))
                                     (end-of-file () :eof))))))

(defmethod stream-unread-char ((stream chunked-mixin) character)
  (with-slots (input-chunking remaining-input) stream
      (cond (*recursive* (call-next-method))
            (input-chunking (incf remaining-input)
                            (call-next-method))
            (t (call-next-method)))))

(defmethod stream-read-line ((stream chunked-mixin))
  (loop
   with chars = nil
   for char = (stream-read-char stream)
   until (eq char #\Linefeed)
   do
   (if (eq char :eof)
     (if (null chars)
	 (error 'end-of-file :stream stream)
	 (return (coerce chars 'string)))
     (push char chars))
   finally (return (coerce (nreverse chars) 'string))))

(defmethod stream-read-sequence ((stream chunked-mixin) sequence start end)
  (loop
   for i from start below end
   do
   (let ((char (stream-read-char stream)))
     (case char
       (:eof (return i))
       (t (setf (elt sequence i) char))))
   finally (return i)))

(defmethod stream-clear-input ((stream chunked-mixin))
  (with-slots (input-chunking) stream
    (cond (*recursive* (call-next-method))
          (input-chunking nil)
          (t (call-next-method)))))

(defmethod stream-write-byte ((stream chunked-mixin) byte)
  (check-type byte unsigned-byte)
  (if *recursive*
      (call-next-method)
      (with-slots (output-buffer) stream
	(or (vector-push byte output-buffer)
	    (progn
	      (stream-force-output stream)
	      (stream-write-byte stream byte))))))

(defmethod stream-write-char ((stream chunked-mixin) character)
  (if *recursive*
      (call-next-method)
      (stream-write-byte stream (char-code character))))

(defmethod stream-write-sequence ((stream chunked-mixin) sequence start end)
  (loop
   for i from start below end
   do
   (let ((e (elt sequence i)))
     (etypecase e
       (integer (stream-write-byte stream e))
       (character (stream-write-char stream e))))))

(defmethod stream-write-string ((stream chunked-mixin) string &optional
				(start 0) (end (length string)))
  (stream-write-sequence stream string start end))

(defmethod write-crlf ((stream stream))
  (let ((*recursive* t))
    (write-char #\Return stream)
    (write-char #\Linefeed stream)))

(defmethod stream-force-output ((stream chunked-mixin))
  (with-slots (output-chunking output-buffer) stream
    (when (> (fill-pointer output-buffer) 0)
      (let ((*recursive* t))
	(when output-chunking
	  (let ((*print-base* 16))
	    (princ (fill-pointer output-buffer) stream))
	  (write-crlf stream))
	(write-sequence output-buffer stream)
	(setf (fill-pointer output-buffer) 0)
	(when output-chunking
	  (write-crlf stream)))))
  (call-next-method))

(defmethod stream-finish-output ((stream chunked-mixin))
  (unless *recursive*
    (force-output stream))
  (call-next-method))

(defmethod stream-clear-output ((stream chunked-mixin))
  (with-slots (output-chunking output-buffer) stream
    (if (and output-chunking (not *recursive*))
        (setf (fill-pointer output-buffer) 0)
      (call-next-method))))

(defmethod close ((stream chunked-mixin) &key abort)
  (unless abort
    (finish-output stream))
  (with-slots (output-chunking output-buffer) stream
    (when (and output-chunking
	       (> (fill-pointer output-buffer) 0))
      (close-chunk stream)))
  (call-next-method))

(defmethod close-chunk ((stream chunked-mixin))
  (finish-output stream)
  (with-slots (output-chunking input-chunking) stream
    (if output-chunking
	(let ((*recursive* t))
	  (princ 0 stream)
	  (write-crlf stream)
	  (write-crlf stream)
	  (finish-output stream)
	  (setf output-chunking nil
		input-chunking nil))
	(error "Chunking is not enabled for output on this stream: ~S."
	       stream))))

(provide :com.ljosa.chunked)

