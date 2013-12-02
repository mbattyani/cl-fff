(in-package #:interface)

(setf clws:*debug-on-server-errors*  t)
(setf clws:*debug-on-resource-errors* t)
(setf clws:*log-level* t)

(defparameter *clws-address* "127.0.0.1")
(defparameter *clws-port* 1339)
(defparameter *clients* (make-hash-table))

(defclass echo-resource (clws:ws-resource)
  )

(defun start-clws-server (&optional (clws-address *clws-address*) (clws-port *clws-port*))
  (let ((bindings (list (cons '*standard-output* *standard-output*))))
    (bt:make-thread (lambda ()
                      (clws:run-server clws-port :addr clws-address))
                    :name "websockets server"
                    :initial-bindings bindings)))

(defmethod clws::resource-client-connected ((res echo-resource) client)
  ;; (format *debug-io* "connection from ~s : ~s~%" (clws:client-host client) (clws:client-port client))
  t)

(defmethod clws::resource-client-disconnected ((res echo-resource) client)
  ;; (format *debug-io* "Client disconnected ~s : ~s~%" (clws:client-host client) (clws:client-port client))
  (let* ((interface-id (gethash client *clients*)))
    (remhash interface-id *http-links*)
    (remhash client *clients*)))

(defmethod clws::resource-received-text ((res echo-resource) client message)
  ;; (format t "got frame ~s from client ~s" message client)
  (let* ((msg (cl-json:decode-json-from-string message))
         (type (cdr (assoc :type msg :test #'string=)))
         (value (cdr (assoc :value msg :test #'string=)))
         (interface-id (gethash client *clients*))
         (*http-link* (when interface-id (gethash interface-id *http-links*))))
    (cond
      ((string= type "id")
       (assert (or (null (gethash value *clients*))
                   (= (string= (gethash value *clients*)
                               value))))
       (setf (gethash client *clients*) value))
      ((string= type "4")
       (let* ((action-func (gethash type *action-funcs*)))
         (destructuring-bind (item-name item-value) value
         (when *http-link*
           (handler-bind
               ((error #'(lambda (e)
                           (let ((bt '())
                                 error-string)
                             (mp:map-process-backtrace mp:*current-process*
                                                       #'(lambda (o)
                                                           (push o bt)))
                             (with-standard-io-syntax
                               (let ((*print-readably* nil))
                                 (setf error-string
                                       (html:html-to-string
                                        (:html
                                          (:head)
                                          (:body
                                           (:fformat "Error : ~a~%" e) :br
                                           (:format "Backtrace:<br>~{ ~S <br>~}" (nreverse bt))))))))))))
             (funcall action-func *http-link* item-name item-value))) ;; change slot!
         
         (let ((object-modified (interface::object (find *http-link* interface::*dispatchers* :test #'eq :key #'interface::interface))))
           (let (clients-to-update)
             (maphash (lambda (client id)
                        (let ((http-link (gethash id *http-links*)))
                          (mapcar (lambda (x)
                                    (when (eq (object x)
                                              object-modified)
                                      (pushnew client clients-to-update)))
                                  (dirty-dispatchers http-link))))
                      *clients*)
           (mapcar (lambda (c)
                     (let ((msg (list (cons "itemname" item-name)
                                      (cons "itemvalue" item-value))))
                       (clws:write-to-client-text c (with-output-to-string (x)
                                                      (cl-json:encode-json msg x))))
                     )
                   clients-to-update)
           )))))
      ((string= type "8")
       (let* ((action-func (gethash type *action-funcs*)))
         (destructuring-bind (item-name item-value) value 
           (funcall action-func *http-link* item-name item-value)
           (send-to-interface "var x_ = parent;" *http-link*)
           (loop
              for instruction in (output-queue *http-link*)
              for msg = (list (cons "action" "eval")
                              (cons "data" instruction))
              do
                (clws:write-to-client-text client (with-output-to-string (x)
                                                    (cl-json:encode-json msg x))))
         )))
      (t (error "Only type 4 is implemented")))))

(defmethod clws::resource-client-disconnected ((resource echo-resource) client)
  ;; (format t "Client disconnected from resource ~A: ~A~%" resource client)
  ;; instead #'http-link-timer
  (let ((id (gethash client *clients*)))
    (when id ;; some pages dont have register ids
      (let ((http-link (gethash id *http-links*)))
        (remhash client *clients*)
        (remove-http-link http-link)
        (setf *dispatchers* (remove http-link *dispatchers* :key #'interface))))))


(export 'ws-start)
(defun ws-start (&optional (clws-address *clws-address*) (clws-port *clws-port*))
  (start-clws-server clws-address clws-port)
  (clws:register-global-resource "/"
                                 (make-instance 'echo-resource)
                                 #'clws:any-origin)
  (bordeaux-threads:make-thread
   (lambda ()
     (clws:run-resource-listener (clws:find-global-resource "/")))
   :name "resource-listener"))
