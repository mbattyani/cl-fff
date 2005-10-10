;;;;										;
;;;; (c) 2002 by Jochen Schmidt.
;;;;
;;;; File:            chunked-stream-mixin.lisp
;;;; Revision:        0.1
;;;; Description:     ACL style HTTP1.1 I/O chunking
;;;; Date:            08.04.2002
;;;; Authors:         Jochen Schmidt
;;;; Tel:             (+49 9 11) 47 20 603
;;;; Email:           jsc@dataheaven.de
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;; 1. Redistributions of source code must retain the above copyright
;;;;    notice, this list of conditions and the following disclaimer.
;;;; 2. Redistributions in binary form must reproduce the above copyright
;;;;    notice, this list of conditions and the following disclaimer in the
;;;;    documentation and/or other materials provided with the distribution.
;;;;
;;;; THIS SOFTWARE IS PROVIDED "AS IS" AND THERE ARE NEITHER 
;;;; EXPRESSED NOR IMPLIED WARRANTIES -  THIS INCLUDES, BUT 
;;;; IS NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
;;;; AND FITNESS FOR A PARTICULAR PURPOSE.IN NO WAY ARE THE
;;;; AUTHORS LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;; SPECIAL, EXEMPLARY OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES ;
;;;; LOSS OF USE, DATA, OR PROFITS ; OR BUSINESS INTERRUPTION)
;;;; 
;;;; For further details contact the authors of this software.
;;;;
;;;;  Jochen Schmidt        
;;;;  Zuckmantelstr. 11     
;;;;  91616 Neusitz         
;;;;  GERMANY               
;;;;
;;;; Nuernberg, 08.Apr.2002 Jochen Schmidt
;;;;

(in-package :de.dataheaven.chunked-stream-mixin)

(defun buffer-ref (buffer index)
  #+lispworks (schar buffer index)
  #-lispworks (aref buffer index))

(defun (setf buffer-ref) (new-value buffer index)
  #-lispworks (setf (aref buffer index) (char-code new-value))
  #+lispworks (setf (schar buffer index) new-value))

(defclass chunked-stream-mixin ()
  ((output-chunking-p :initform nil :accessor output-chunking-p)
   (chunk-input-avail :initform nil
                      :documentation
                      "Number of octets of the current chunk that are
not yet read into the buffer, or nil if input chunking is disabled")
   (real-input-limit :initform 0
                     :documentation
                     "Index of last octet read into buffer
(input-limit points to index of last octet in the current chunk)")))

(defgeneric input-chunking-p (stream))
(defmethod input-chunking-p ((stream chunked-stream-mixin))
  (not (null (slot-value stream 'chunk-input-avail))))

(defgeneric (setf input-chunking-p) (new-value stream))
(defmethod (setf input-chunking-p) (new-value (stream chunked-stream-mixin))
  (setf (slot-value stream 'chunk-input-avail) (and new-value 0)))

(define-condition acl-compat.excl::socket-chunking-end-of-file (condition)
  ((acl-compat.excl::format-arguments :initform nil :initarg :format-arguments)
   (acl-compat.excl::format-control :initform "A chunking end of file occured"
                                    :initarg :format-control)))


;;;;;;;;;;;;;;;;;;;;;;
;;; Input chunking ;;;
;;;;;;;;;;;;;;;;;;;;;;

;; Input chunking is not tested so far!

(defgeneric initialize-input-chunking (stream))
(defmethod initialize-input-chunking ((stream chunked-stream-mixin))
  "This method initializes input chunking. The real-input-limit is nil
in the beginnings because it got not saved yet. Chunk-input-avail is
obviously 0 because no chunk-data got read so far."
  (gray-stream:with-stream-input-buffer (input-buffer input-index input-limit)
      stream
    (with-slots (real-input-limit chunk-input-avail) stream
      (setf
       ;; Bytes read from stream (valid data in buffer up to here)
       real-input-limit input-limit
       ;; Bytes available in current chunk block after buffer contents
       ;; runs out (trivially zero before first chunk block read)
       chunk-input-avail 0
       ;; Last buffer position that can be read before new data has to
       ;; be fetched from stream (we must begin with parsing a chunk
       ;; immediately; hence set to a value that guarantees this)
       input-limit 0                    ; or input-index?
       ))))

;; Lispworks fix by Edi Weitz (paserve-help 2003-11-28)
#+lispworks
(defmacro %with-stream-input-buffer ((input-buffer input-index input-limit) stream &body body)
  `(with-slots ((,input-buffer stream::input-buffer)
                (,input-index stream::input-index)
                (,input-limit stream::input-limit))
      (slot-value ,stream 'stream::buffer-state)
    ,@body))

(defmethod gray-stream:stream-fill-buffer ((stream chunked-stream-mixin))
  "Refill buffer from stream."
  ;; STREAM-FILL-BUFFER gets called when the input-buffer contains no
  ;; more data (the index is bigger than the limit). We call out to
  ;; the real buffer filling mechanism by calling the next specialized
  ;; method. This method is responsible to update the buffer state in
  ;; coordination with the chunk-header.
  (with-slots (chunk-input-avail real-input-limit) stream
    (#-lispworks gray-stream:with-stream-input-buffer
     #+lispworks %with-stream-input-buffer
     (input-buffer input-index input-limit) stream
       (labels
          ((pop-char ()
             (when (and (>= input-index input-limit) ; need new data
                        (not (call-next-method))) ; couldn't get it
               (error "Unexpected end-of-file while reading chunk block"))
             (prog1 #-lispworks (code-char (buffer-ref input-buffer input-index))
                    #+lispworks (buffer-ref input-buffer input-index)
                    (incf input-index)))
           (read-chunk-header ()
             (let ((chunk-length 0))
               (tagbody
                initial-crlf (let ((char (pop-char)))
                               (cond ((digit-char-p char 16)
                                      (decf input-index) ; unread char
                                      (go chunk-size))
                                     ((eq #\Return char)
                                      (if (eq (pop-char) #\Linefeed)
                                          (go chunk-size)
                                        (error "End of chunk-header corrupted: Expected Linefeed")))
                                     (t (error "End of chunk-header corrupted: Expected Carriage Return or a digit"))))

                chunk-size (let ((char (pop-char)))
                             (cond ((digit-char-p char 16)
                                    (setf chunk-length
                                          (+ (* 16 chunk-length)
                                             (digit-char-p char 16)))
                                    (go chunk-size))
                                   (t (decf input-index) ; unread char
                                      (go skip-rest))))

                skip-rest (if (eq #\Return (pop-char))
                              (go check-linefeed)
                            (go skip-rest))

                check-linefeed (let ((char (pop-char)))
                                 (case char
                                   (#\Linefeed (go accept))
                                   (t (error "End of chunk-header corrupted: LF expected, ~A read." char))))

                accept)
               chunk-length)))

         (cond ((not (input-chunking-p stream))
                ;; Chunking not active; just fill buffer normally
                (call-next-method))
               ((zerop chunk-input-avail)
                ;; We are at the beginning of a new chunk.
                (when real-input-limit (setf input-limit real-input-limit))
                (let* ((chunk-length (read-chunk-header))
                       (end-of-chunk (+ input-index chunk-length)))
                  (if (zerop chunk-length)
                      ;; rfc2616 indicates that input chunking is
                      ;; turned off after zero-length chunk is read
                      ;; (see section 19.4.6) -- turn off chunking
                      (progn (signal 'acl-compat.excl::socket-chunking-end-of-file
                                     :format-arguments stream)
                             (setf (input-chunking-p stream) nil)
                             ;; TODO: whoever handles
                             ;; socket-chunking-end-of-file (client.cl
                             ;; in AllegroServe's case) should read the
                             ;; trailer (see section 3.6).  All we can
                             ;; reasonably do here is turn off
                             ;; chunking, or throw information away.
                             )
                    ;; Now set up stream attributes so that read methods
                    ;; call refill-buffer both at end of chunk and end of
                    ;; buffer
                    (progn
                      (setf real-input-limit input-limit
                            input-limit (min real-input-limit end-of-chunk)
                            chunk-input-avail (max 0 (- end-of-chunk
                                                        real-input-limit)))
                      input-limit))))
               (t
                ;; We are in the middle of a chunk; re-fill buffer
                (if (call-next-method)
                    (progn
                      (setf real-input-limit input-limit)
                      (setf input-limit
                            (min real-input-limit chunk-input-avail))
                      (setf chunk-input-avail
                            (max 0 (- chunk-input-avail real-input-limit)))
                      input-limit)
                    (error "Unexpected end-of-file in the middle of a chunk"))))))))


;;;;;;;;;;;;;;;;;;;;;;;
;;; Output chunking ;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; This constant is the amount of bytes the system reserves for the chunk-header
;; It is calculated as 4 bytes for the chunk-size in hexadecimal digits and a CR followed
;; by a LF
(defconstant +chunk-header-buffer-offset+ 6)

(defgeneric initialize-output-chunking (stream))
(defmethod initialize-output-chunking ((stream chunked-stream-mixin))
  "This method initializes output chunking. Actual contents in the output-buffer
   get flushed first. A chunk has a header at the start and a CRLF at the end.
   The header is the length of the (data) content in the chunk as a string in hexadecimal
   digits and a trailing CRLF before the real content begins. We assume that the content
   of a chunk is never bigger than #xFFFF and therefore reserve 6 bytes at the beginning
   of the buffer for the header. We reduce the buffer limit by 2 so that we have always
   room left in the buffer to attach a CRLF."
  (unless (output-chunking-p stream)
    (force-output stream)
    (gray-stream:with-stream-output-buffer (buffer index limit) stream
      (setf index +chunk-header-buffer-offset+)
      (setf (buffer-ref buffer (- +chunk-header-buffer-offset+ 2)) #\Return
            (buffer-ref buffer (1- +chunk-header-buffer-offset+)) #\Linefeed)
      (decf limit 2)
      (setf (output-chunking-p stream) t))))

(defmethod gray-stream:stream-flush-buffer ((stream chunked-stream-mixin))
  "When there is pending content in the output-buffer then compute the chunk-header and flush
   the buffer"
  (if (output-chunking-p stream)
      (gray-stream:with-stream-output-buffer (output-buffer output-index output-limit) stream
        (when (> output-index +chunk-header-buffer-offset+)
          (let* ((chunk-header (format nil "~X" (- output-index +chunk-header-buffer-offset+)))
                 (start (- +chunk-header-buffer-offset+ 2 (length chunk-header))))
            (loop for c across chunk-header
                  for i upfrom start
                  do (setf (buffer-ref output-buffer i) c))
            (setf (buffer-ref output-buffer output-index) #\Return
                  (buffer-ref output-buffer (1+ output-index)) #\Linefeed)
            (gray-stream:stream-write-buffer stream output-buffer start (+ output-index 2))
            (setf output-index +chunk-header-buffer-offset+))))
    (call-next-method)))


(defmethod close ((stream chunked-stream-mixin) &key abort)
  (unless abort
    (disable-output-chunking stream))
  (call-next-method))


(defgeneric disable-output-chunking (stream))
(defmethod disable-output-chunking ((stream chunked-stream-mixin))
  "When we disable chunking we first try to write out a last pending chunk and after that
   reset the buffer-state to normal mode. To end the game we write out a chunk-header with
   a chunk-size of zero to notify the peer that chunking ends."
  (when (output-chunking-p stream)
    (force-output stream)
    (gray-stream:with-stream-output-buffer (buffer index limit) stream
      (setf index 0)
      (incf limit 2))
    (setf (output-chunking-p stream) nil
          (input-chunking-p stream) nil)
    (format stream "0~A~A~A~A" #\Return #\Linefeed #\Return #\Linefeed)
    (force-output stream)))




