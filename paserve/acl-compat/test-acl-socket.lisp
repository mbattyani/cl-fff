;;; Unit tests for the ACL-SOCKET compatibility package.

(in-package cl-user)

(require :acl-socket)

(use-package '(acl-socket))

(defun test1 ()
  (let ((stream (make-socket :connect :active :remote-host "127.0.0.1" :remote-port 2500)))
    (when stream
      (read-line stream)
      (format stream "helo foo")
      (write-char #\Return stream)
      (write-char #\Linefeed stream)
      (finish-output stream)
      (read-line stream)
      (close stream))))

(defun test2 ()
  (let ((stream (make-socket :connect :active :remote-host "127.0.0.1" :remote-port 2500)))
    (when stream
      (socket-control stream :output-chunking t)
      (read-line stream)
      (format stream "helo foo")
      (write-char #\Return stream)
      (write-char #\Linefeed stream)
      (finish-output stream)
      (read-line stream)
      (close stream))))

(defun test3 ()
  (let ((stream (make-socket :connect :active :remote-host "127.0.0.1" :remote-port 2500)))
    (when stream
      (socket-control stream :input-chunking t)
      (prog1
          (read-line stream)
        (close stream)))))

(defun test4 ()
  (let ((stream (or (make-socket :connect :active :remote-host "127.0.0.1" :remote-port 2500)
                    (error "Failed to connect."))))
    (socket-control stream :input-chunking t)
    (format t "File number 1: ")
    #1=(handler-case
	   (loop
	    for char = (read-char stream nil stream)
	    until (eq char stream)
	    do (write-char char))
	 (excl::socket-chunking-end-of-file (e) (socket-control stream :input-chunking t)))
    (format t "~%File number 2: ")
    #1#
    (terpri)
    (values)))


        
  



