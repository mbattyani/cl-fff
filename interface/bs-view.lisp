(in-package #:interface)


;;;**** push-button *****
(defclass bs-push-button (html-item)
  ((action-func :initform nil :accessor action-func :initarg :action-fn)))

(defmethod make-set-value-javascript ((item bs-push-button) value slot)
  )

(defmethod push-button-slot-tag ((frontend bootstrap) attributes form)
  (let ((item (make-instance 'bs-push-button :action-fn (first form))))
    `(html:html ((:input :type "submit" :name ,(name item)
		  :insert-string ,(format nil "onclick='Fck(~s, 0);'" (name item))
		  ,@attributes)))))

;;; ****** fn-link *******
(defclass bs-fn-link (html-item)
  ((html-fn :accessor html-fn :initform nil :initarg :html-fn)
   (choices-fn :accessor choices-fn :initform nil :initarg :choices-fn)
   (action-fn :initform nil :accessor action-fn :initarg :action-fn)
   (fc-function :initform nil :accessor fc-function :initarg :fc-function)))

(defclass bs-fn-link-dispatcher (object-dispatcher)
  ())

(defmethod make-dispatcher (interface object (item bs-fn-link))
  (make-instance 'bs-fn-link-dispatcher :interface interface :object object :item item))

(defmethod initialize-instance :after ((dispatcher bs-fn-link-dispatcher)
                                       &rest init-options &key &allow-other-keys)
  (let* ((item (item dispatcher))
	 (object (object dispatcher))
	 (fn (fc-function item))
	 (*object* object))
    (push (list fn
		#'(lambda (action value)
		      (when (eq action :status-changed)
			(mark-dirty-status dispatcher value)))
		dispatcher)
	  (meta::listeners object))
    (setf (disabled dispatcher) (meta::slot-disabled-p object fn))))

(defmethod update-dispatcher-item ((dispatcher bs-fn-link-dispatcher) &optional force)
  (let* ((*dispatcher* dispatcher)
	 (*object* (object dispatcher))
	 (item (item dispatcher))
	 (interface (interface dispatcher)))
    (when (or force (dirty-status dispatcher))
      (send-to-interface (make-set-status-javascript item (disabled dispatcher)
						     (fc-function item)) interface)
      (setf (dirty-status dispatcher) nil))))

(defmethod fire-action ((dispatcher bs-fn-link-dispatcher) value click-str)
  (let* ((*dispatcher* dispatcher)
	 (*object* (object dispatcher))
	 (item (item dispatcher))
	 (fn (fc-function item))
	 (function (action-fn (item dispatcher))))
    (when function
      (funcall function (object dispatcher))
      (send-to-interface (make-set-status-javascript item (meta::slot-disabled-p *object* fn) fn)
			 (interface dispatcher)))))

(defmethod safely-convert-string-to-value ((dispatcher bs-fn-link-dispatcher) value)
  (values value t))

(defmethod try-change-slot ((dispatcher bs-fn-link-dispatcher) value)
  (funcall (action-fn (item dispatcher)) (object dispatcher) value))

(defmethod visible-p ((item bs-fn-link))
  (or (force-visible item)(visible-p (fc-function item))))

(defmethod make-set-value-javascript ((item bs-fn-link) value slot)
  )

(defmethod make-set-status-javascript ((item bs-fn-link) status slot)
  (if status
      (concatenate 'string "x_.f8252h('" (name item) "');")
      (concatenate 'string "x_.f8252s('" (name item) "');")))

(defmethod fn-link-tag ((frontend bootstrap) attributes form)
  (destructuring-bind (fc-function . attrs) attributes
    (when (symbolp fc-function)
      (setf fc-function (find fc-function (meta::effective-functions *current-class*) :key 'meta::name)))
    (let* ((fn-name (meta::name fc-function))
	   (item (make-instance 'bs-fn-link
				:choices-fn (meta::get-object-func fc-function)
				:html-fn (or (meta::get-value-html-fn fc-function) 'std-fn-pick-obj-html-fn)
				:action-fn fn-name
				:force-visible (getf attrs :force-visible)
				:fc-function fc-function))
           (class (or (getf attrs :class) "btn btn-default")))
      (setf attrs (copy-list attrs))
      (remf attrs :force-visible)
      (remf attrs :class)
      `(html:html
        ((:button :id ,(concatenate 'string (name item) "d") :disabled "disabled"
                  :class ,class :style "display:none;" ,@attrs) ,@form)
        ((:button :id ,(name item) :class ,class
                  :insert-string ,(if (or (choices-fn item) (meta::get-value-html-fn fc-function))
                                      (format nil "onclick=\"open1('/pick-val.html','250px','500px','~a');\"" (name item))
                                      (format nil "onclick='f825foc(~s);'" (name item)))
                  ,@attrs) ,@form)))))

;((:a :href "" :id ,(name item)
;     :insert-string ,(format nil "onclick='f825foc(~s);'" (name item)) ,@attrs) ,@form)

(defun bs-fn-link-tag2 (attributes form)
  (destructuring-bind (fc-function . attrs) attributes
    (when (symbolp fc-function)
      (setf fc-function (find fc-function (meta::effective-functions *current-class*) :key 'meta::name)))
    (let* ((fn-name (meta::name fc-function))
	   (item (make-instance 'bs-fn-link
				:choices-fn (meta::get-object-func fc-function)
				:html-fn (or (meta::get-value-html-fn fc-function) 'bs-std-fn-pick-obj-html-fn)
				:action-fn fn-name
				:force-visible (getf attrs :force-visible)
				:fc-function fc-function)))
      (setf attrs (copy-list attrs))
      (remf attrs :force-visible)
      `(html:html
	((:a :id ,(concatenate 'string (name item) "d") :disabled "true"
	  :style "display:none;" ,@attrs) ,@(first form))
	((:a :id ,(name item)
	  :insert-string ,(if (choices-fn item)
			      (format nil "HREF=\"javascript:open1('/pick-val.html','250px','500px','~a');\"" (name item))
			      (format nil "HREF='javascript:f825foc(~s);'" (name item)))
	  ,@attrs) ,@(second form))))))

;((:a :href "" :id ,(name item)
;     :insert-string ,(format nil "onclick='f825foc(~s);'" (name item)) ,@attrs) ,@form)

(defun bs-std-fn-pick-obj-html-fn (dispatcher)
  (let* ((item (item dispatcher))
	 (item-name (name item))
	 ;(object (object dispatcher))
	 (fc-function (fc-function (item dispatcher))))
    (html:html
     (:head
      (:title (:translate (meta::get-value-title fc-function) :default '(:en "Choose an object" :fr "Choisissez un objet")))
      ((:link :rel "stylesheet" :type "text/css" :href "/cal.css")))
     (:body
      :br
      (:h1 (:translate (meta::get-value-title fc-function) :default '(:en "Choose an object" :fr "Choisissez un objet")))
      (:jscript "window.focus();var shot;function f42(d){if (!shot) {opener.Fch('" item-name "',d);"
                "window.setTimeout('window.close();', 600); shot = true;}};")
#+nil      (:jscript "function f42(d){window.opener.Fch('" item-name "',d);"
                "window.close();}; window.focus();")
      (:p (:translate (meta::get-value-text fc-function)))
      (when dispatcher
	(when t;(meta::null-allowed fc-function)
	  (html:html "&nbsp;&nbsp;"
		     ((:a :href "javascript:f42('nil');") (:translate '(:en "None of these choices" :fr "Aucun de ces choix"))) :br :br))
	(loop for object in (funcall (meta::get-object-func fc-function)(object dispatcher))
	      do (html:html "&nbsp;&nbsp;"
			    ((:a :fformat (:href "javascript:f42('~a');" (encode-object-id object)))
			     (html:esc (meta::short-description object))) :br)))
      ((:div :align "center")((:a :class "call" :href "javascript:window.close();")
			      (:translate '(:en "Close" :fr "Fermer"))))))))

