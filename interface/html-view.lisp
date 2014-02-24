(in-package #:interface)

(defmethod initialize-instance :after ((item html-item) &rest init-options &key &allow-other-keys)
  (setf (id item) *next-id*)
  (incf *next-id*)
  (setf (tab-id item)(incf *tab-id*))
  (setf (name item) (format nil "G~a" (id item)))
  (when *root-item*
    (setf (gethash (name item) (all-items *root-item*)) item)))

(defun disabled-p (item)
  (declare (ignore item))
  (error "does nothing"))

(defmethod visible-p ((item html-item))
  (or (force-visible item)
      (visible-p (slot item))))

(defmethod modifiable-p ((obj html-item))
  (if (slot obj)
      (modifiable-p (slot obj))
      t))

;(defun with-class-tag (attributes form)
;  (destructuring-bind ((class country-lang name) . forms) form
;    (let ((*current-class* (if (symbolp class) (find-class class) class))
;	  (*country-language* country-lang)
;	  (*root-item* name))
;      (cons 'progn (mapcar #'(lambda (e) (html:html-gen e)) forms)))))

;(html:add-func-tag :with-class 'with-class-tag)

;;; ******* object dialog *******
(defun object-dialog-tag (attributes form)
  (declare (ignore attributes))
  (destructuring-bind ((dx dy &key title (border :etched)) description) form
    (let* ((panel-item (make-instance 'ui-root :border border :text title :floating-position t))
	   (*top-level-item* panel-item))
      (setf (sub-panes panel-item)(apply-layout description))
      (optimize-layouts panel-item)
      (compute-layout panel-item 0 0 dx dy)
      (compute-tab-order panel-item)
      (html:html-gen (write-construction panel-item nil :html)))))

(html:add-func-tag :object-dialog 'object-dialog-tag)


;;; ******** linked html *********
(defclass linked-html (html-item)
  ((func :initform nil :accessor func :initarg :func)))

(defmethod visible-p ((item linked-html))
  t)

(defmethod make-set-value-javascript ((item linked-html) value slot)
   (funcall (func item)))

(defmethod make-set-status-javascript ((item linked-html) status slot)
  "")

(defun linked-html-tag (attributes forms)
  (loop with func = (second attributes)
	for slot-name in (first attributes)
	for slot = (find (symbol-name slot-name)
			 (c2mop:class-slots *current-class*) :test #'string=
			 :key #'c2mop:slot-definition-name)
	do (make-instance 'linked-html :slot slot :func func))
  (cons 'html::optimize-progn (mapcar 'html:html-gen forms)))

(html:add-func-tag :linked-html 'linked-html-tag)
;(:linked-html ((slot1 slot2 ... slotn) func) form)

;;;**** push-button *****
(defclass html-push-button (html-item)
  ((action-func :initform nil :accessor action-func :initarg :action-fn)))

(defmethod make-set-value-javascript ((item html-push-button) value slot)
  )

(defmethod push-button-slot-tag ((frontend html) attributes form)
  (let ((item (make-instance 'html-push-button :action-fn (first form))))
    `(html:html ((:input :type "submit" :name ,(name item)
		  :insert-string ,(format nil "onclick='Fck(~s, 0);'" (name item))
		  ,@attributes)))))

(html:add-func-tag :push-button 'push-button-slot-tag t)

;;; ****** fn-link *******
(defclass html-fn-link (html-item)
  ((html-fn :accessor html-fn :initform nil :initarg :html-fn)
   (choices-fn :accessor choices-fn :initform nil :initarg :choices-fn)
   (action-fn :initform nil :accessor action-fn :initarg :action-fn)
   (fc-function :initform nil :accessor fc-function :initarg :fc-function)))

(defclass html-fn-link-dispatcher (object-dispatcher)
  ())

(defmethod make-dispatcher (interface object (item html-fn-link))
  (make-instance 'html-fn-link-dispatcher :interface interface :object object :item item))

(defmethod initialize-instance :after ((dispatcher html-fn-link-dispatcher) &rest init-options &key &allow-other-keys)
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

(defmethod update-dispatcher-item ((dispatcher html-fn-link-dispatcher) &optional force)
  (let* ((*dispatcher* dispatcher)
	 (*object* (object dispatcher))
	 (item (item dispatcher))
	 (interface (interface dispatcher)))
    (when (or force (dirty-status dispatcher))
      (send-to-interface (make-set-status-javascript item (disabled dispatcher)
						     (fc-function item)) interface)
      (setf (dirty-status dispatcher) nil))))

(defmethod fire-action ((dispatcher html-fn-link-dispatcher) value click-str)
  (let* ((*dispatcher* dispatcher)
	 (*object* (object dispatcher))
	 (item (item dispatcher))
	 (fn (fc-function item))
	 (function (action-fn (item dispatcher))))
    (when function
      (funcall function (object dispatcher))
      (send-to-interface (make-set-status-javascript item (meta::slot-disabled-p *object* fn) fn)
			 (interface dispatcher)))))

(defmethod safely-convert-string-to-value ((dispatcher html-fn-link-dispatcher) value)
  (values value t))

(defmethod try-change-slot ((dispatcher html-fn-link-dispatcher) value)
  (funcall (action-fn (item dispatcher)) (object dispatcher) value))

(defmethod visible-p ((item html-fn-link))
  (or (force-visible item)(visible-p (fc-function item))))

(defmethod make-set-value-javascript ((item html-fn-link) value slot)
  )

(defmethod make-set-status-javascript ((item html-fn-link) status slot)
  (if status
      (concatenate 'string "x_.f8252h('" (name item) "');")
      (concatenate 'string "x_.f8252s('" (name item) "');")))

(defmethod fn-link-tag ((frontend html) attributes form)
  (destructuring-bind (fc-function . attrs) attributes
    (when (symbolp fc-function)
      (setf fc-function (find fc-function (meta::effective-functions *current-class*) :key 'meta::name)))
    (let* ((fn-name (meta::name fc-function))
	   (item (make-instance 'html-fn-link
				:choices-fn (meta::get-object-func fc-function)
				:html-fn (or (meta::get-value-html-fn fc-function) 'std-fn-pick-obj-html-fn)
				:action-fn fn-name
				:force-visible (getf attrs :force-visible)
				:fc-function fc-function)))
      (setf attrs (copy-list attrs))
      (remf attrs :force-visible)
      `(html:html
        ((:a :id ,(concatenate 'string (name item) "d") :disabled "true"
             :style "display:none;" ,@attrs) ,@form)
        ((:a :id ,(name item)
             :insert-string ,(if (or (choices-fn item) (meta::get-value-html-fn fc-function))
                                 (format nil "HREF=\"javascript:open1('/pick-val.html','250px','500px','~a');\"" (name item))
                                 (format nil "HREF='javascript:f825foc(~s);'" (name item)))
             ,@attrs) ,@form)))))

;((:a :href "" :id ,(name item)
;     :insert-string ,(format nil "onclick='f825foc(~s);'" (name item)) ,@attrs) ,@form)

(html:add-func-tag :fn-link 'fn-link-tag t)

(defmethod fn-link-tag2 ((frontend html) attributes form)
  (destructuring-bind (fc-function . attrs) attributes
    (when (symbolp fc-function)
      (setf fc-function (find fc-function (meta::effective-functions *current-class*) :key 'meta::name)))
    (let* ((fn-name (meta::name fc-function))
	   (item (make-instance 'html-fn-link
				:choices-fn (meta::get-object-func fc-function)
				:html-fn (or (meta::get-value-html-fn fc-function) 'std-fn-pick-obj-html-fn)
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

(html:add-func-tag :fn-link2 'fn-link-tag2 t)

(defun std-fn-pick-obj-html-fn (dispatcher)
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

;;; ***** slot-table ****

(defun slot-table-tag (attributes form)
  (destructuring-bind (&key no-table ) attributes
    `(html:html ,@(make-std-object-slots-view *frontend* *current-class* form no-table))))

(html::add-func-tag :slot-table 'slot-table-tag)

;;; ***** function-table ****

(defun obj-fn-table (attributes form)
  (destructuring-bind (&key no-table) attributes
  `(html:html ,@(make-std-object-functions-view *current-class* form))))

(html::add-func-tag :obj-fn-table 'obj-fn-table)

;;; ***** obj-fn ****

(defun obj-fn-tag (attributes forms)
  (destructuring-bind (function . attrs) attributes
    (setf function (find function (meta::effective-functions *current-class*)))
    (when function
      (unless forms (setf forms `((:translate (meta::user-name function)))))
      `(html:html ((:fn-link ,function ,@attrs) ,@forms)))))

(html::add-func-tag :obj-fn 'obj-fn-tag)

;;; **** when-groups *****
(defun when-group-tag (attributes forms)
  (declare (ignore attributes))
  `(html:html
    (:when (intersection *user-groups* ,(car forms))
      ,@(cdr forms))))

(html::add-func-tag :when-groups 'when-group-tag)

;;; **** unless-groups *****
(defun unless-groups-tag (attributes forms)
  (declare (ignore attributes))
  `(html:html
    (:unless (intersection *user-groups* ,(car forms))
      ,@(cdr forms))))

(html::add-func-tag :unless-groups 'unless-groups-tag)

;;; **** when-frontends *****
(defun when-frontends-tag (attributes forms)
  (declare (ignore attributes))
  `(html:html
    (:when (find *frontend* ,(car forms))
      ,@(cdr forms))))

(html::add-func-tag :when-frontends 'when-frontends-tag)

;;; **** unless-frontends *****
(defun unless-frontends-tag (attributes forms)
  (declare (ignore attributes))
  `(html:html
    (:unless (find *frontend* ,(car forms))
      ,@(cdr forms))))

(html::add-func-tag :unless-frontends 'unless-frontends-tag)
