;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LW Style Buffer Protocol for other Lisps     ;;;
;;; So far only 8bit byte and character IO works ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :gray-stream)

(defvar *default-input-buffer-size* 8192)
(defvar *default-output-buffer-size* 8192)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct buffer-state 
    (input-buffer (make-array *default-input-buffer-size* :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (*)))
    (input-index nil)
    (input-limit *default-input-buffer-size* :type fixnum)
    (output-buffer (make-array *default-output-buffer-size* :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (*)))
    (output-index 0)
    (output-limit *default-output-buffer-size* :type fixnum)))

;; Can be used to implement resourcing of buffers later
(defun %allocate-buffer-state (&optional (input-limit *default-input-buffer-size*) (output-limit *default-output-buffer-size*))
  (declare (ignore input-limit output-limit))
  (make-buffer-state))

(defun %deallocate-buffer-state (state)
  (declare (ignore state)))

;; Can be used to implement unbuffered encapsulating streams later
(defclass native-lisp-stream-mixin ()
  ((lisp-stream :initarg :lisp-stream
		:reader native-lisp-stream))
  (:documentation "Stream mixin that encapsulates a native stream."))

(defclass buffered-stream-mixin (native-lisp-stream-mixin)
  ((buffer-state :initform (%allocate-buffer-state)))
  (:documentation "Stream mixin that provides buffering for a native lisp stream."))

;; fundamental-bivalent-xxx-streams can be used to implement buffered
;; and unbuffered bivalent streams.  At the moment, we only implement
;; buffered ones.
(defclass fundamental-bivalent-input-stream
    (fundamental-character-input-stream fundamental-binary-input-stream)
  ())

(defclass fundamental-bivalent-output-stream
    (fundamental-character-output-stream fundamental-binary-output-stream)
  ())

(defclass buffered-bivalent-input-stream
    (buffered-stream-mixin fundamental-bivalent-input-stream)
  ())

(defclass buffered-bivalent-output-stream
    (buffered-stream-mixin fundamental-bivalent-output-stream)
  ())

(defclass buffered-bivalent-stream
    (buffered-bivalent-input-stream buffered-bivalent-output-stream)
  ())

(defmacro with-stream-output-buffer ((buffer index limit) stream &body forms)
  (let ((state (gensym "BUFFER-STATE-")))
  `(let ((,state (slot-value ,stream 'buffer-state)))
     (symbol-macrolet ((,buffer ,(list 'buffer-state-output-buffer state))
                       (,index ,(list 'buffer-state-output-index state))
                       (,limit ,(list 'buffer-state-output-limit state)))
       ,@forms))))

;;; Encapsulated native streams

(defmethod close ((stream native-lisp-stream-mixin) &key abort)
  (close (native-lisp-stream stream) :abort abort))

(defmethod stream-listen ((stream native-lisp-stream-mixin))
  (listen (native-lisp-stream stream)))

(defmethod open-stream-p ((stream native-lisp-stream-mixin))
  (common-lisp::open-stream-p (native-lisp-stream stream)))

(defmethod stream-clear-output ((stream native-lisp-stream-mixin))
  (clear-output (native-lisp-stream stream)))

;;; Input streams

(declaim (inline %reader-function-for-sequence))
(defun %reader-function-for-sequence (sequence)
  (typecase sequence
    (string #'read-char)
    ((array unsigned-byte (*)) #'read-byte)
    ((array signed-byte (*)) #'read-byte)
    (otherwise #'read-byte)))

(defun read-elements (socket-stream sequence start end reader-fn)
  (let* ((len (length sequence))
         (chars (- (min (or end len) len) start)))
    (loop for i upfrom start
          repeat chars
          for char = (funcall reader-fn socket-stream)
          if (eq char :eof) do (return-from read-elements i)
          do (setf (elt sequence i) char))
    (+ start chars)))

(defmacro with-stream-input-buffer ((buffer index limit) stream &body forms)
  (let ((state (gensym "BUFFER-STATE-")))
  `(let ((,state (slot-value ,stream 'buffer-state)))
     (symbol-macrolet ((,buffer ,(list 'buffer-state-input-buffer state))
                       (,index ,(list 'buffer-state-input-index state))
                       (,limit ,(list 'buffer-state-input-limit state)))
       ,@forms))))

(defgeneric stream-fill-buffer (stream))
(defmethod stream-fill-buffer ((stream buffered-stream-mixin))
  ;; Implement b/nb semantics: block until at least one byte is read,
  ;; but not until the whole buffer is filled.  This means it takes at
  ;; most n calls to this function to fill a buffer of length n, even
  ;; with a slow connection.
  (with-stream-input-buffer (buffer index limit) stream
    (let* ((the-stream (native-lisp-stream stream))
           (read-bytes
            (loop with byte
               for n-read from 0 below limit
               while (and (if (< 0 n-read) (listen the-stream) t)
                          (setf byte (read-byte the-stream nil nil)))
               do (setf (aref buffer n-read) byte)
               count t)))
      (if (zerop read-bytes)
          nil
          (setf index 0
                limit read-bytes)))))

(defmethod stream-read-byte ((stream buffered-bivalent-input-stream))
  (with-stream-input-buffer (buffer index limit) stream
     (unless (and index (< index limit))
       (when (null (stream-fill-buffer stream))
	 (return-from stream-read-byte :eof)))
     (prog1 (aref buffer index)
       (incf index))))

(defmethod stream-read-char ((stream buffered-bivalent-input-stream))
  (let ((byte (stream-read-byte stream)))
    (if (eq byte :eof)
        :eof
      (code-char byte))))

(defmethod stream-read-char-no-hang ((stream buffered-bivalent-input-stream))
  (if (listen stream)
      (read-char stream)
      nil))

(defmethod stream-unread-char ((stream buffered-bivalent-input-stream) character)
  (with-stream-input-buffer (buffer index limit) stream
      (let ((new-index (1- index)))
        (when (minusp new-index)
          (error "Cannot unread char ~A" character))
        (setf (aref buffer new-index) (char-code character)
              index new-index)))
  nil)

(defmethod stream-peek-char ((stream buffered-bivalent-input-stream))
  (let ((char (stream-read-char stream)))
    (unless (eq char :eof)
      (stream-unread-char stream char))
    char))


(defmethod stream-read-line ((stream buffered-bivalent-input-stream))
  (let ((res (make-array 80 :element-type 'character :fill-pointer 0)))
    (loop
     (let ((ch (stream-read-char stream)))
       (cond ((eq ch :eof)
	      (return (values (copy-seq res) t)))
	     ((char= ch #\Linefeed)
              (return (values (copy-seq res) nil)))
             (t
              (vector-push-extend ch res)))))))


(defmethod stream-read-sequence ((stream buffered-bivalent-input-stream) sequence &optional start end)
  (read-elements stream sequence start end (%reader-function-for-sequence sequence)))

;;(defmethod stream-clear-input ((stream buffered-bivalent-input-stream))
;;  (clear-input (native-lisp-stream stream)))

(defmethod stream-element-type ((stream fundamental-bivalent-input-stream))
  '(or character (unsigned-byte 8)))

;;; Output streams

(declaim (inline %writer-function-for-sequence))
(defun %writer-function-for-sequence (sequence)
  (typecase sequence
    (string #'stream-write-char)
    ((array unsigned-byte (*)) #'stream-write-byte)
    ((array signed-byte (*)) #'stream-write-byte)
    (otherwise #'stream-write-byte)))

(defun write-elements (stream sequence start end writer-fn)
  (let* ((len (length sequence))
         (start (or start 0))
         (end (or end len)))
    (assert (<= 0 start end len))
    (etypecase sequence
      (simple-vector (loop for i from start below end
                           do (funcall writer-fn stream (svref sequence i))))
      (vector (loop for i from start below end
                    do (funcall writer-fn stream (aref sequence i))))
      (list (loop for i from start below end
                  for c in (nthcdr start sequence)
                  do (funcall writer-fn stream c))))))

(defgeneric stream-write-buffer (stream buffer start end))
(defmethod stream-write-buffer ((stream buffered-stream-mixin) buffer start end)
  (let ((lisp-stream (native-lisp-stream stream)))
    (write-sequence buffer lisp-stream :start start :end end)))

(defgeneric stream-flush-buffer (stream))
(defmethod stream-flush-buffer ((stream buffered-stream-mixin))
  (with-stream-output-buffer (buffer index limit) stream
    (when (plusp index)
      (stream-write-buffer stream buffer 0 index)
      (setf index 0))))

(defmethod stream-write-byte ((stream buffered-bivalent-output-stream) byte)
  (with-stream-output-buffer (buffer index limit) stream
    (unless (< index limit)
      (stream-flush-buffer stream))
    (setf (aref buffer index) byte)
    (incf index)))

(defmethod stream-write-char ((stream buffered-bivalent-output-stream) character)
  (stream-write-byte stream (char-code character)))

(defmethod stream-write-string ((stream buffered-bivalent-output-stream) string &optional (start 0) end)
  (write-elements stream string start end #'stream-write-char))

(defmethod stream-write-sequence ((stream buffered-stream-mixin) sequence
                                  &optional (start 0) end)
  (write-elements stream sequence start end (%writer-function-for-sequence sequence)))

(defmethod stream-element-type ((stream fundamental-bivalent-output-stream))
  '(or character (unsigned-byte 8)))

(defmethod stream-line-column ((stream fundamental-bivalent-output-stream))
  nil)

(defmethod stream-finish-output ((stream buffered-bivalent-output-stream))
  (stream-flush-buffer stream)
  (finish-output (native-lisp-stream stream)))

(defmethod stream-force-output ((stream buffered-bivalent-output-stream))
  (stream-flush-buffer stream)
  (force-output (native-lisp-stream stream)))

(defmethod stream-clear-output ((stream buffered-bivalent-output-stream))
  (with-stream-output-buffer (buffer index limit) stream
     (setf index 0
           limit 0))
  (call-next-method)                    ; Clear native stream also
  )


