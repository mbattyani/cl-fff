(eval-when (:load-toplevel :compile-toplevel :execute)
(defpackage meta-web
 (:use common-lisp)
 (:nicknames metaw)
 (:shadowing-import-from meta-level defclass)
   )
)

(in-package meta-web)

(defparameter *graph-file-prefix*
  #+win32 "d:/program files/Apache Group/Apache/htdocs/fractal/"
  #+linux "/var/www/html/fractal/")

(defvar *current-class* nil)
(defvar *current-slot* nil)
(defvar *current-slot-attribute* nil)

(defvar *database-pool* nil); (clsql:find-or-create-connection-pool '("213.11.22.163" "MetaStore" "lisp" "") :postgresql))
(defvar *meta-store* nil); (make-instance 'meta::psql-store :db-pool *database-pool*))
(defvar *save-database* t)
(defvar *in-store-timer* nil)

(defun meta-store-timer-fn ()
  (unless *in-store-timer*
    (setf *in-store-timer* t)
    (unwind-protect
	 (when (and *meta-store* *save-database*)
	   (setf *save-database* nil)
	   (util:with-logged-errors (:ignore-errors t)
	     (meta::save-modified-objects *meta-store*))
	   (setf *save-database* t))
      (setf *in-store-timer* nil))))

(defun start-meta-store-timer ()
  (let ((timer (mp:make-timer 'meta-store-timer-fn)))
    (mp:schedule-timer-relative timer 30 30)))

(defvar *meta-store-timer* (start-meta-store-timer))

(defun prev-page-link ()
  (let ((previous (interface::get-previous-page interface::*request*)))
    (html:html
     (:when previous
       ((:a :href previous) (:translate '(:fr "Page précédente" :en "Previous Page")))))))

(defparameter *default-object-page-fr*
  '((prev-page-link) :br
    (:h2 "Objet : " (html:esc (meta::short-description interface::*object*)))
    (:p
     (html:ffmt "(~a ~a)"
      (meta::translated-class-name interface::*object*) (meta::id interface::*object*)) :br
     (:when (meta::parent interface::*object*)
       "Inclus dans : " ((:a :href (interface::encode-object-url (meta::parent interface::*object*)))
			 (html:ffmt "~a (~a)"
				    (meta::short-description (meta::parent interface::*object*))
				    (meta::translated-class-name (meta::parent interface::*object*))))))
    (:object-view)))

(defparameter *default-object-page-en*
  '((prev-page-link) :br
    (:h2 "Object : " (html:esc (meta::short-description interface::*object*)))
    (:p 
     (html:ffmt "(~a ~a)"
      (meta::translated-class-name interface::*object*) (meta::id interface::*object*)) :br
     (:when (meta::parent interface::*object*)
       "Included in : " ((:a :href (interface::encode-object-url (meta::parent interface::*object*)))
			 (html:ffmt "~a (~a)"
				    (meta::short-description (meta::parent interface::*object*))
				    (meta::translated-class-name (meta::parent interface::*object*))))))
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
