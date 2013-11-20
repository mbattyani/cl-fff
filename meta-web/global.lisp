(in-package #:meta-web)

(defparameter *graph-file-prefix* (asdf:system-relative-pathname :meta-web "./web-resources/"))

(defvar *current-class* nil)
(defvar *current-slot* nil)
(defvar *current-slot-attribute* nil)

(defvar *database-pool* nil)
(defvar *meta-store* nil)
(defvar *save-database* t)
(defvar *in-store-timer* nil)
(defvar *projects-list* nil)

(defparameter *page* nil)
(defparameter *pages* (make-hash-table :test #'equal))
(defparameter *web-root-name* "home")
(defparameter *static-pages-root* "/fcweb/")
(defparameter *static-pages-url-root* "/asp/fcweb/")
(defparameter *source-pages-root* #-win32"/tmp/fcweb/src/" #+win32"f:/fcweb/src/")
(defparameter *source-pages-default* "~/repository-static/xxx.html")

(defparameter *database-ip* "127.0.0.1")
(defparameter *database-name* "MetaFractal")
(defparameter *database-user* "lisp")
(defparameter *database-pwd* "")
(defparameter *mod-lisp-port* 3100)
(defparameter *local-port* 25142)

(setf interface:*server-name* "127.0.0.1")
(setf interface:*clws-address* interface:*server-name*)
;(setf interface:*clws-port* 1339)

(defparameter *init-file*
  #+macosx #P"~/initfc.lisp"
  #+linux #P"~/initfc.lisp"
  #+win32 #P"s:/sources/svn/fractal/initfc.lisp")

(defparameter *web-directory* (asdf:system-relative-pathname :meta-web "./web-resources/"))

(defvar *create-store* nil)

(defmacro encode-page (page-name)
  (assert (stringp page-name))
  `(if (interface::new-cookie *request*)
       ,(interface::encode-page page-name)
       ,(concatenate 'string "/" page-name)))

(defparameter *hunchentoot-acceptor* nil)

(defun start-hunchentoot ()
  (setf *hunchentoot-acceptor* (make-instance 'hunchentoot:easy-acceptor :port *local-port*  :document-root (namestring *web-directory*)))
  (hunchentoot:start *hunchentoot-acceptor*))

(defun stop-hunchentoot ()
  (hunchentoot:stop *hunchentoot-acceptor*))

(defun create-mongo-store ()
  (meta::initialize-store *meta-store*))

;; mongo store
(defconstant +mongo-collection-name+ "fw-objects")

(defun clear-db ()
  (mapcar (lambda (x)
            (cl-mongo:db.delete +mongo-collection-name+ x))
          (cadr (cl-mongo:db.find +mongo-collection-name+ :all))))

#+nil
(defun project-list () ;;mongo only
  (or *projects-list*
      (sort (remove nil (mapcar #'(lambda (x)
                                    (let ((object (meta::load-object (cl-mongo:get-element "object-id" x) *meta-store*)))
                                      (when (typep object 'project)
                                        (push object *projects-list*)
                                        object)))
                                (cadr (cl-mongo:db.find +mongo-collection-name+ :all))))
            #'string< :key #'name)))

#+nil
(defun project-list () ;; postgres
  (or *projects-list*
      (sort (mapcar #'(lambda (x)
                        (meta::load-object (first x) *meta-store*))
                    (meta::sql-query "select id from project"))
            #'string< :key #'name)))


(defun create-store ()
  (meta::initialize-store *meta-store*)
  (when (or (not (typep *meta-store* 'meta:ascii-store))
            (not (typep *meta-store* 'meta:mongo-store)))
    (create-meta-classes *meta-store*)))

(defun start-apache () ;; apache
  (interface:sa *mod-lisp-port*))

(defun start (&key (webserver :hunchentoot) (database :postgres) (first-start nil) (mongo-db-name "mydb") (mongo-db-collection-name +mongo-collection-name+) debug
                (init-file nil)
                ascii-store-path)
  (assert (and (member webserver '(:hunchentoot :apache))
               (member database '(:postgres :mongo-db))))
  (setf interface:*web-server* webserver)
  (when debug
    (log:config debug))
  (when init-file
    (load init-file))
  (cond
    ((eq database :postgres)
     (meta::init-psql)
     (setf *database-pool* (meta:psql-create-db-pool *database-ip* *database-name* *database-user* *database-pwd*))
     (setf *meta-store* (make-instance 'meta:psql-store :db-pool *database-pool*))
     (setf (symbol-function 'project-list)
           (lambda () ;; postgres
             (or *projects-list*
                 (sort (mapcar #'(lambda (x)
                                   (meta::load-object (first x) *meta-store*))
                               (meta::sql-query "select id from project"))
                       #'string< :key #'name)))))
    ((eq database :mongo-db)
     (setf *meta-store* (make-instance 'meta:mongo-store :database-name mongo-db-name :collection-name mongo-db-collection-name))
     (setf *projects-list* nil)
     (setf (symbol-function 'project-list)
           (lambda () ;;mongo only
             (or *projects-list*
                 (sort (remove nil (mapcar #'(lambda (x)
                                               (let ((object (meta::load-object (cl-mongo:get-element "object-id" x) *meta-store*)))
                                                 (when (typep object 'project)
                                                   (push object *projects-list*)
                                                   object)))
                                           (cadr (cl-mongo:db.find +mongo-collection-name+ :all))))
                       #'string< :key #'name)))))
    (ascii-store-path
     (setf *meta-store*
           (make-instance 'meta::ascii-store :file-directory ascii-store-path))
     (setf *projects-list* nil #+nil(list (meta::load-named-object *meta-store* "project"))))
    )
  (when first-start
    (create-store))
  ;; (setf *clipboard* (make-instance 'interface::clipboard :store meta::*memory-store*))
  (interface:ws-start)
  (case webserver
    (:hunchentoot
     (setf (symbol-function 'interface::write-request)
           #'interface::write-hunchentoot-request) ;; FIXME hack
     (setf (symbol-function 'interface::write-header)
           #'interface::write-hunchentoot-header) ;; FIXME hack
     (start-hunchentoot))
    (:apache
     (start-apache))))

(defun stop (&optional (hunchentoot? t))
  (when hunchentoot?
    (stop-hunchentoot)))

(unless meta::*memory-store*
  (setf meta::*memory-store* (make-instance 'meta::void-store)))

(defun meta-store-timer-fn ()
  (unless *in-store-timer*
    (setf *in-store-timer* t)
    (unwind-protect
	 (when (and *meta-store* *save-database*)
	   (setf *save-database* nil)
	   (util:with-logged-errors (:ignore-errors nil) ; t
	     (meta::save-modified-objects *meta-store*))
	   (setf *save-database* t))
      (setf *in-store-timer* nil))))

(defun start-meta-store-timer ()
  (let ((timer (mp:make-timer 'meta-store-timer-fn)))
    (mp:schedule-timer-relative timer 10 10))) ; 30 30

(defvar *meta-store-timer* (start-meta-store-timer))

(defun prev-page-link ()
  (let ((previous (interface::get-previous-page interface::*request*)))
    (html:html
     (:when previous
       ((:a :href previous) (:translate '(:fr "Page précédente" :en "Previous Page")))))))

(defparameter *default-object-page-fr*
  '((prev-page-link) :br
    (:h2 "Objet : " (html:esc (meta::short-description interface::*object*))
     ((:span :style "font-size:12px;font-weight:400")
      " ("(:esc (meta::translated-class-name interface::*object*))")"))
    (:p (interface::gen-localize-html interface::*object* :home-url #e"projects"))
    (:object-view)))

(defparameter *default-object-page-en*
  '((prev-page-link) :br
    (:h2 "Object : " (html:esc (meta::short-description interface::*object*))
     ((:span :style "font-size:12px;font-weight:400")
      " ("(:esc (meta::translated-class-name interface::*object*))")"))
    (:p (interface::gen-localize-html interface::*object* :home-url #e"projects"))
    (:object-view)))

(defparameter *inspect-object-page-fr*
  '(((:form :method "post" :action #e"inspect")
     (interface::decode-posted-content interface::*request*)
     (:with-posted-params ((obj-id1 "objectid1" :int nil)
			   (obj-id2 "objectid2" :int nil)
			   (obj-id3 "objectid3" :int nil)
			   (store-id "storeid" :int nil))
       "id store :" ((:input :type "text" :name "storeid"))  :br
       "id objet 1:" ((:input :type "text" :name "objectid1")) :br
       "id objet 2:" ((:input :type "text" :name "objectid2")) :br
       "id objet 3:" ((:input :type "text" :name "objectid3")) :br
       ((:input :type "submit" :name "submit" :value "Inspecter")) :br
       (when (and obj-id1 store-id)
	 (let ((object (meta::load-object obj-id1 (meta::find-store store-id))))
	   (when object
	     (html:html
	      (:p "Objet : "
		  (html:ffmt "~a ~a" (meta::translated-class-name object) (meta::id object)) :br
		  (:when (meta::parent object)
		    "Inclus dans : " ((:a :href (interface::encode-object-url (meta::parent object))))
		    (html:ffmt "~a (~a)"
			       (meta::short-description (meta::parent object))
			       (meta::translated-class-name (meta::parent object)))))
	      (:object-view :object object)))))
       (when (and obj-id2 store-id)
	 (let ((object (meta::load-object obj-id2 (meta::find-store store-id))))
	   (when object
	     (html:html
	      (:p "Objet : "
		  (html:ffmt "~a ~a" (meta::translated-class-name object) (meta::id object)) :br
		  (:when (meta::parent object)
		    "Inclus dans : " ((:a :href (interface::encode-object-url (meta::parent object))))
		    (html:ffmt "~a (~a)"
			       (meta::short-description (meta::parent object))
			       (meta::translated-class-name (meta::parent object)))))
	      (:object-view :object object)))))
       (when (and obj-id3 store-id)
	 (let ((object (meta::load-object obj-id3 (meta::find-store store-id))))
	   (when object
	     (html:html
	      (:p "Objet : "
		  (html:ffmt "~a ~a" (meta::translated-class-name object) (meta::id object)) :br
		  (:when (meta::parent object)
		    "Inclus dans : " ((:a :href (interface::encode-object-url (meta::parent object))))
		    (html:ffmt "~a (~a)"
			       (meta::short-description (meta::parent object))
			       (meta::translated-class-name (meta::parent object)))))
	      (:object-view :object object)))))))))

(defparameter *inspect-object-page-en*
  '(((:form :method "post" :action #e"inspect")
     (interface::decode-posted-content interface::*request*)
     (:with-posted-params ((obj-id "objectid" :int nil)
			   (store-id "storeid" :int nil))
       "object id :" ((:input :type "text" :name "objectid")) :br
       "store id :" ((:input :type "text" :name "storeid")) :br
       ((:input :type "submit" :name "submit" :value "Inspecter")) :br
       (when (and obj-id store-id)
	 (let ((interface::*object* (meta::load-object obj-id (meta::find-store store-id))))
	   (when interface::*object*
	     (html:html
	      (:p "Objet : "
		  (html:ffmt "~a ~a" (meta::translated-class-name interface::*object*) (meta::id interface::*object*)) :br
		  (:when (meta::parent interface::*object*)
		    "Inclus dans : " ((:a :href (interface::encode-object-url (meta::parent interface::*object*))))
		    (html:ffmt "~a (~a)"
			       (meta::short-description (meta::parent interface::*object*))
			       (meta::translated-class-name (meta::parent interface::*object*)))))
	      (:object-view)))))))))
