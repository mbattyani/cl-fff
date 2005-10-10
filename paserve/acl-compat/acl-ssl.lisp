(in-package :ssl)
;;;;;;;;;;;;;;;;;;;;;
;;; ACL style API ;;;
;;;;;;;;;;;;;;;;;;;;;

(defmethod make-ssl-client-stream ((socket integer) &rest options)
  (destructuring-bind (&key (format :binary)) options
    (when (minusp socket)
      (error "not a proper socket descriptor"))
    (let ((ssl-socket (make-instance 'ssl-internal:ssl-client-socket :fd socket)))
      (case format
        (:binary (make-instance 'binary-ssl-stream 
                                :ssl-socket ssl-socket))
        (:text (make-instance 'character-ssl-stream
                              :ssl-socket ssl-socket))
        (otherwise (error "Unknown ssl-stream format"))))))

#+lispworks
(defmethod make-ssl-client-stream ((lw-socket-stream comm:socket-stream) &rest options)
  (apply #'make-ssl-client-stream (comm:socket-stream-socket lw-socket-stream) options))

#+cormanlisp
(defmethod make-ssl-client-stream (stream  &rest options)
  (apply #'make-ssl-client-stream (sockets:socket-descriptor (cl::stream-handle stream)) options))

(defmethod make-ssl-server-stream ((socket integer) &rest options)
  (destructuring-bind (&key certificate key other-certificates (format :binary)) options
    (when (minusp socket)
      (error "not a proper socket descriptor"))
        (let ((ssl-socket (make-instance 'ssl-internal:ssl-server-socket
                                         :fd socket
                                         :rsa-privatekey-file (or key certificate)
                                         :certificate-file (or certificate key))))
      (case format
        (:binary (make-instance 'binary-ssl-stream 
                                :ssl-socket ssl-socket))
        (:text (make-instance 'character-ssl-stream
                              :ssl-socket ssl-socket))
        (otherwise (error "Unknown ssl-stream format"))))))

(defmethod make-ssl-server-stream ((socket ssl-stream-mixin) &rest options)
  (warn "SSL socket ~A reused" socket)
  socket)

#+lispworks
(defmethod make-ssl-server-stream ((lw-socket-stream comm:socket-stream) &rest options)
  (apply #'make-ssl-server-stream (comm:socket-stream-socket lw-socket-stream) options))


#+ignore
(defmethod make-ssl-server-stream ((acl-socket acl-socket::server-socket) &rest options)
  (apply #'make-ssl-server-stream 
         (comm::get-fd-from-socket (acl-socket::passive-socket acl-socket)) options))

#+ignore
(defmethod make-ssl-server-stream ((lw-socket-stream acl-socket::chunked-socket-stream) &rest options)
  (apply #'make-ssl-server-stream (comm:socket-stream-socket lw-socket-stream) options))

