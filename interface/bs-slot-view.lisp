(in-package #:interface)

;;; ***** slot-name ************

;; (defun bs-slot-name-tag (attributes form)
;;   (declare (ignore attributes))
;;   (let ((slot (find (symbol-name (first form)) (c2mop:class-slots *current-class*)
;; 		    :test #'string= :key #'c2mop:slot-definition-name)))
;;     (unless slot (error (format nil "Unknown slot : ~a" (first form))))
;;     `(write-string ,(get-user-name slot) html:*html-stream*)))

;(html:add-func-tag :slot-name 'bs-slot-name-tag)

;;;***** slot edit **************

(defclass bs-edit (html-item)
  ())

(defmethod make-set-value-javascript ((item bs-edit) value slot)
  (when (meta::choices slot)
    (setf value (meta::translate (second (assoc value (meta::choices slot))))))
  (let ((j-value (html:quote-javascript-string
                  (cond
                   ((and (not value) (meta-level::dont-display-null-value slot)) "")
                   ((stringp value) value)
                   (t (funcall (meta::value-to-string-fn slot) value))))))
    (if (modifiable-p *dispatcher*)
	(concatenate 'string "x_.f826svi('" (name item) "', '" j-value "');")
	(concatenate 'string "x_.f826si('" (name item) "', '" j-value "');"))))

(defmethod make-set-status-javascript ((item bs-edit) status slot)
  (when (modifiable-p *dispatcher*)
    (if status
	(concatenate 'string "x_.f8252h('" (name item) "');")
	(concatenate 'string "x_.f8252s('" (name item) "');"))))

(defmethod slot-edit-tag ((frontend bootstrap) attributes form)
  (declare (ignore form))
  (destructuring-bind (slot-name . attrs) attributes
    (let ((slot (find (symbol-name slot-name) (c2mop:class-slots *current-class*)
		      :test #'string= :key #'c2mop:slot-definition-name)))
      (unless slot (error (format nil "Unknown slot : ~a" slot-name)))
      (let* ((edit (make-instance 'bs-edit :tooltip (meta::tooltip slot) :slot slot
                                  :force-visible (getf attrs :force-visible))))
	(remf attrs :force-visible)
	(if (modifiable-p slot)
            `(html:html
              ((:input :type "text" :id ,(name edit) :class "form-control" :style "display:none;" :insert-string
                       ,(format nil "onchange='Fch(~s,~a.value);'" (name edit) (name edit)) ,@attrs))
              ((:p class "form-control-static" :id ,(concatenate 'string (name edit) "d") :style "display:none;")))
            `(html:html ((:p class "form-control-static" :id ,(concatenate 'string (name edit) "d") ,@attrs))))
        ))))

;(html:add-func-tag :slot-edit 'slot-edit-tag t)

;;;***** slot span **************

(defclass bs-span (html-item)
  ((format-fn :accessor format-fn :initarg :format-fn :initform nil)))

(defmethod make-set-value-javascript ((item bs-span) value slot)
  (when (meta::choices slot)
    (setf value (meta::translate (second (assoc value (meta::choices slot))))))
  (let ((j-value (if (format-fn item)
                     (funcall (format-fn item) value)
                     (html:quote-javascript-string 
                      (cond
                       ((and (not value) (meta-level::dont-display-null-value slot)) "")
                       ((stringp value) value)
                       (t (funcall (meta::value-to-string-fn slot) value)))))))
    (concatenate 'string "x_.fgt('" (name item) "').innerHTML='" j-value "';")))

(defmethod make-set-status-javascript ((item bs-span) status slot)
  )

(defmethod slot-span-tag ((frontend bootstrap) attributes form)
  (declare (ignore form))
  (destructuring-bind (slot-name . attrs) attributes
    (let ((slot (find (symbol-name slot-name) (c2mop:class-slots *current-class*)
		      :test #'string= :key #'c2mop:slot-definition-name)))
      (unless slot (error (format nil "Unknown slot: ~a" slot-name)))
      (let* ((edit (make-instance 'bs-span :tooltip (meta::tooltip slot)
                                             :slot slot :format-fn (getf attrs :format-fn))))
        (remf attrs :format-fn)
	`(html:html ((:span :id ,(name edit) ,@attrs)))))))

;(html:add-func-tag :slot-span 'slot-span-tag)

;;**** Multiline Edit ***************************

(defclass bs-medit (bs-edit)
  ())

(defmethod slot-medit-tag ((frontend bootstrap) attributes form)
  (declare (ignore form))
  (destructuring-bind (slot-name . attrs) attributes
    (let ((slot (find (symbol-name slot-name) (c2mop:class-slots *current-class*)
		      :test #'string= :key #'c2mop:slot-definition-name)))
      (unless slot (error (format nil "Unknown slot : ~a" slot-name)))
      (let ((edit (make-instance 'bs-medit :tooltip (meta::tooltip slot) :slot slot
				 :force-visible (getf attrs :force-visible))))
	(setf attrs (copy-list attrs))
	(remf attrs :force-visible)
	(if (modifiable-p slot)
            `(html:html ((:textarea :id ,(name edit) :rows ,(getf attrs :rows "3") :class "form-control" :style "display:none;"
                                    :insert-string ,(format nil "onchange='Fch(~s,~a.value);'" (name edit)(name edit)) ,@attrs))
                        ((:p class "form-control-static" :id ,(concatenate 'string (name edit) "d") :style "display:none;")))
            `(html:html  ((:p class "form-control-static" :id ,(concatenate 'string (name edit) "d") :style "display:none;"))))))))

;(html:add-func-tag :slot-medit 'slot-medit-tag)

;;; ********* Date Edit ************

(defclass bs-date (html-item)
  ((show-time :accessor show-time :initform nil :initarg :show-time)
   (show-date :accessor show-date :initform t :initarg :show-date)))

(defmethod make-set-value-javascript ((item bs-date) value slot)
  (if value
      (multiple-value-bind (s mn h d m y) 
          (if meta::*GMT-time* (decode-universal-time value 0)(decode-universal-time value))
	(let ((j-value (if (not (show-time item))
                           (format nil "~2,'0d/~2,'0d/~d" d m y)
			   (if (show-date item)
                               (format nil "~2,'0d/~2,'0d/~d ~2,'0d:~2,'0d:~2,'0d" d m y h mn s)
                               (format nil "~2,'0d:~2,'0d:~2,'0d" h mn s)))))
	  (concatenate 'string "x_.f826si('" (name item) "', '" j-value "');")))
      (concatenate 'string "x_.f826si('" (name item) "', '');")))

(defmethod make-set-status-javascript ((item bs-date) status slot)
  (when (modifiable-p *dispatcher*)
    (if status
	(concatenate 'string
		     "x_.fgt('" (name item) "l').style.visibility='hidden';")
	(concatenate 'string
		     "x_.fgt('" (name item) "l').style.visibility='inherit';"))))

(defmethod slot-date-edit-tag ((frontend bootstrap) attributes form)
  (declare (ignore form))
  (destructuring-bind (slot-name . attrs) attributes
    (let ((slot (find (symbol-name slot-name) (c2mop:class-slots *current-class*)
		      :test #'string= :key #'c2mop:slot-definition-name)))
      (unless slot (error (format nil "Unknown slot : ~a" slot-name)))
      (let ((edit (make-instance 'bs-date :tooltip (meta::tooltip slot) :slot slot
				 :show-time (getf attrs :show-time)
                                 :show-date (getf attrs :show-date t))))
	(setf attrs (copy-list attrs))
	(remf attrs :show-time)
	(remf attrs :show-date)
	`(html:html (:if (modifiable-p ,slot)
			 (:progn
			   ((:span :id ,(concatenate 'string (name edit) "d"))) " &nbsp;"
                           " "
                           #+nil((:a :id ,(concatenate 'string (name edit) "l") :href "#openModalCalendar" :onclick ,(format nil "set_src('calendar_iframe', '/calendar.html', '~a')" (name edit)))
                            "Change...")
                           ((:modal-button :id ,(concatenate 'string (name edit) "l") :target "#openModalCalendar"
                                           :onclick
                                           #+nil,(format nil "$('#selectedTarget').load(make_src('/calendar.html', '~a'));" (name edit))
                                           ,(format nil "set_src('calendar_iframe', '/calendar.html', '~a')" (name edit))) "Change...")
                           ((:modal-window :id "openModalCalendar")
                            (:body
                             ((:iframe :width "200px" :height "280px" :id "calendar_iframe" :name "calendar_iframe")))))
			 ((:span :id ,(concatenate 'string (name edit) "d")))))))))

;(html:add-func-tag :slot-date-edit 'slot-date-edit-tag)

(defun bs-html-month (item day month year show-time)
  (let ((first-week-day (first-week-day month year))
	(last-day (last-day month year))
	(time (if show-time 
		  " '+document.getElementById('hour').value+':'+document.getElementById('mn').value+':'+document.getElementById('sec').value);"
		  "');"))
        (div-class (princ (gensym "divclass"))))
    (html:html
     #+nil (:jscript "window.focus();function f42(d){if (d == '') window.opener.Fch('" item "','nil');else window.opener.Fch('" item "',d+'/" month "/" year time
	       "window.close();};")
      ((:script :src "https://code.jquery.com/jquery.js"))
      (:jscript "function f42(d){if (d == '') parent.Fch('" item "','nil');else parent.Fch('" item "',d+'/" month "/" year time "parent.$('#openModalCalendar').modal('hide');};")
       ;f42($(this).text());
          
     ((:table :class "calt" :align "center")
      (:tr (dolist (day (getf *day-names* *country-language* *default-day-names*))
             (html:html ((:th :class "calh") day))))
      "<tr>"
      (loop for d from (- first-week-day)
	    for col from 0
	    until (and (> d last-day)(= (mod col 7) 0))
	    do (html:html (:if (or (< d 1)(> d last-day))
			    ((:td :class "cald"))
			    ((:td :class "cald" :insert-string (if (= day d) "style='background-color:#ffffff';" ""))
                             ((:div :fformat (:class "~a" div-class)) d)
                             #+nil((:a :fformat (:href "javascript:f42(~a);" d)) d)))
			  (:when (= (mod col 7) 6) "</tr>"))))
     (:jscript "$('div." div-class "').click(function(){f42($(this).text())});")
     )))

(defun bs-calendar-request-handler (request)
      (decode-posted-content request)
      (let* ((link-name (cdr (assoc "link" (posted-content request) :test 'string=)))
	     (link (gethash link-name *http-links*))
	     (item (cdr (assoc "item" (posted-content request) :test 'string=)))
	     (year (cdr (assoc "year" (posted-content request) :test 'string=)))
	     (month (cdr (assoc "month" (posted-content request) :test 'string=)))
             (day nil)
	     (hour (cdr (assoc "hour" (posted-content request) :test 'string=)))
	     (mn (cdr (assoc "mn" (posted-content request) :test 'string=)))
	     (sec (cdr (assoc "sec" (posted-content request) :test 'string=)))
	     (dispatcher (when link (gethash item (dispatchers link))))
	     (*session* (session link))
	     (*user* (user *session*))
	     (*country-language* (country-language *session*)))
	(setf year  (when year (parse-integer year)))
	(setf month (when month (parse-integer month)))
	(setf hour (when hour (parse-integer hour :junk-allowed t)))
	(setf mn (when mn (parse-integer mn :junk-allowed t)))
	(setf sec (when sec (parse-integer sec :junk-allowed t)))
	(multiple-value-bind (s min h d m y) 
            (if meta::*GMT-time*
                (decode-universal-time (or (funcall (get-value-fn dispatcher) (object dispatcher))
                                           (get-universal-time)) 0)
                (decode-universal-time (or (funcall (get-value-fn dispatcher) (object dispatcher))
                                           (get-universal-time))))
	  (setf year (or year y))
	  (setf month (or month m))
	  (setf day d)
	  (setf hour (or hour h))
	  (setf mn (or mn min))
	  (setf sec (or sec s)))
	(with-output-to-request (request)
	  (html::html-to-stream
	   *request-stream*
	   "<!doctype html>"
           (:html
             (:head
              ((:meta :http-Equiv "Cache-Control" :Content "no-cache"))
              ((:meta :http-Equiv "Pragma" :Content "no-cache"))
              ((:meta :http-Equiv "Expires" :Content "0"))
              (:title (:translate '(:en "Choose a day" :fr "Choisissez une date" :sp "Elija una fecha")))
              ((:link :rel "stylesheet" :type "text/css" :href "/static/cal.css"))
              (:jscript "function f42(d){}"))
             (:body
              :br
              (:h1 (:translate '(:en "Choose a day" :fr "Choisissez une date" :sp "Elija una fecha")))
              ((:form :name "go" :method "post" :action "/calendar.html")
               ((:input :name "item" :type "hidden" :value item))
               ((:div :align "center")
                ((:input :name "link" :type "hidden" :value link-name))
                ((:select :name "month" :onchange "document.forms['go'].submit();")
                 (loop for m from 1 to 12
                    for name in (getf *month-names* *country-language* *default-month-names*)
                    do (if (= month m)
                           (html:ffmt "<option value=~d SELECTED>~d" m name)
                           (html:ffmt "<option value=~d>~d" m name))))
                "&nbsp;&nbsp;"
                ((:select :name "year" :onchange "document.forms['go'].submit();")
                 (loop for a from 1901 below 2100
                    do (if (= year a)
                           (html:ffmt "<option value=~d SELECTED>~d" a a)
                           (html:ffmt "<option value=~d>~d" a a)))))
               (html-month item day month year (show-time (item dispatcher)))
	      
               ((:div :align "center")
                (:when (show-time (item dispatcher))
                  ((:input :name "hour" :id "hour" :value (princ-to-string hour) :style "width:20px;"))
                  ":"
                  ((:input :name "mn" :id "mn" :value (princ-to-string mn) :style "width:20px;"))
                  ":"
                  ((:input :name "sec" :id "sec" :value (princ-to-string sec) :style "width:20px;")) :br
                  (html:html "&nbsp;&nbsp;"
                             ((:a :fformat (:href "javascript:f42(~d);" day))
                              (:translate '(:en "Submit" :fr "Envoyer" :sp "Submit"))) :br))
                (when (and dispatcher (meta::null-allowed (slot dispatcher)))
                  (html:html "&nbsp;&nbsp;"
                             ((:a :href "javascript:f42('');")
                              (:translate '(:en "No date" :fr "Aucune date" :sp "Ninguna fecha"))) :br))
                (#+nil(:button :type "button" :class "close" :data-dismiss "modal" :aria-hidden "true" )
                 (:a :class "call" :href "javascript:parent.$('#openModalCalendar').modal('hide');") ;window.close();
                 (:translate '(:en "Close" :fr "Fermer" :sp "Cerrar"))))
               ))))))
      t)

;(interface::add-named-url "/calendar.html" 'calendar-request-handler)

;;; ***** Combo *************
(defclass bs-combo (html-item)
  ())

(defmethod make-set-value-javascript ((item bs-combo) value slot)
  (let ((position (position value (meta::choices slot) :key #'first :test #'equal)))
    (unless position (setf position -1))
    (html:fast-format nil "x_.fgt('~a').selectedIndex='~a';" (name item) position)))

(defmethod slot-combo-tag ((frontend bootstrap) attributes form)
  (declare (ignore form))
  (destructuring-bind (slot-name . attrs) attributes
    (let ((slot (find (symbol-name slot-name) (c2mop:class-slots *current-class*)
		      :test #'string= :key #'c2mop:slot-definition-name)))
      (unless slot (error (format nil "Unknown slot : ~a" slot-name)))
      (let ((combo (make-instance 'bs-combo :tooltip (meta::tooltip slot) :slot slot))
	    (choices (loop for (nil string) in (meta::choices slot) collect (meta::translate string)))) ;value
	`(html:html
	  ((:select :id ,(name combo) :class "form-control"
	    :insert-string
	    (if (modifiable-p ,slot)
		,(format nil "onchange=\"Fch('~a',~a.value);\""(name combo)(name combo))
		"disabled='true'")
	    ,@attrs)
	   ,@(loop for choice in choices
		   for i from 0
		   collect (format nil "<option value = ~d>~a" i choice))))))))

;(html:add-func-tag :slot-combo 'slot-combo-tag t)

;;**** Check Box ***************************

(defclass bs-check-box (html-item)
  ())

(defmethod make-set-value-javascript ((item bs-check-box) value slot)
  (html:fast-format nil "x_.fgt('~a').checked=~a;" (name item) (if value "true" "false")))

(defmethod slot-check-box-tag ((frontend bootstrap) attributes form)
  (declare (ignore form))
  (destructuring-bind (slot-name . attrs) attributes
    (let ((slot (find (symbol-name slot-name) (c2mop:class-slots *current-class*)
		      :test #'string= :key #'c2mop:slot-definition-name)))
      (unless slot (error (format nil "Unknown slot : ~a" slot-name)))
      (let ((check-box (make-instance 'bs-check-box :tooltip (meta::tooltip slot) :slot slot)))
	`(html:html ((:input :type "checkbox" :id ,(name check-box)
                             :insert-string
                             (if (modifiable-p ,slot)
                                 ,(format nil "onclick='Fch(~s,~a.checked);'"
                                          (name check-box)(name check-box))
                                 "disabled='true'")
                             ,@attrs)))))))

;(html:add-func-tag :slot-check-box 'slot-check-box-tag t)

;;; slot-obj-link

(defclass bs-obj-link (html-pick-val)
  ())

(defmethod make-set-value-javascript ((item bs-obj-link) value slot)
  (html:fast-format nil "x_.fgt('~a').href='~a';x_.fgt('~a').innerHTML='~a';"
		    (name item)
		    (if (meta::fc-object-p value) (encode-object-url value) "javascript:void(0)")
		    (name item)
		    (html:quote-javascript-string
		     (if value
		       (meta::short-description value)
		       (meta::translated-void-link-text slot)))))

(defmethod slot-obj-link-tag ((frontend bootstrap) attributes form)
  (declare (ignore form))
  (destructuring-bind (slot-name . attrs) attributes
    (let ((slot (find (symbol-name slot-name) (c2mop:class-slots *current-class*)
		      :test #'string= :key #'c2mop:slot-definition-name)))
      (unless slot (error (format nil "Unknown slot : ~a" slot-name)))
      (let ((obj-link (make-instance 'html-obj-link :tooltip (meta::tooltip slot) :slot slot
				     :choices-fn (meta::get-object-func slot)
				     :html-fn (or (meta::get-value-html-fn slot) 'bs-std-pick-obj-html-fn))))
	`(html:html
          ((:div :class "input-group")
           ((:p :class "form-control-static")
            ((:a :href "" :id ,(name obj-link) ,@attrs)))
           "&nbsp;"
           ,@(when (action-link obj-link)
                   `((:when (modifiable-p ,slot)
                       ((:span :class "input-group-btn")
                        ((:a :class "btn btn-default" :data-toggle "modal"
                             :id ,(action-link obj-link)  :type "button"
                             :data-target "#GlobalModal" #+nil "#global_modal"
                             :onclick #+nil(format nil "set_src('global_iframe','/pick-val.html', '~a');" (name obj-link))
                             ,(format nil "show_remote_modal_content('~a','/pick-val.html', '~a');"
                                               (meta::translate (meta::get-value-title slot)
                                                                :default '(:en "Choose an object" :fr "Choisissez un objet" :sp "Elija un objeto"))
                                             (name obj-link)))
                         ((:span :class "glyphicon glyphicon-expand")"&nbsp;Change..."))))))))))))

(defun bs-std-pick-obj-html-fn (dispatcher)
  (let* ((item (item dispatcher))
	 (item-name (name item))
	 ;(object (object dispatcher))
	 (slot (slot dispatcher)))
    (html:html
     ((:div :class "modal-dialog")
      ((:div :class "modal-content")
       ((:div :class "modal-header")
        ((:h4 :class "modal-title"))
        (:translate (meta::get-value-title slot)
                    :default '(:en "Choose an object" :fr "Choisissez un objet" :sp "Elija un objeto")))
       ((:div :class "modal-body")
        (:p (:translate (meta::get-value-text slot)))
        (:jscript "function f42(d){Fch('" item-name "',d);"
                  "$('#GlobalModal').modal('hide');};")
        (when dispatcher
          (when (meta::null-allowed (slot dispatcher))
            (html:html "&nbsp;&nbsp;"
                       ((:a :href "javascript:f42('nil');")
                        (:translate '(:en "None of these choices" :fr "Aucun de ces choix"
                                      :sp "Ninguna de estas opciones"))) :br :br))
          (loop for object in (funcall (meta::get-object-func (slot dispatcher))(object dispatcher))
             do (html:html "&nbsp;&nbsp;"
                           ((:a :fformat (:href "javascript:f42('~a');" (encode-object-id object)))
                            (html:esc (meta::short-description object))) :br))))
       ((:div :class "modal-footer")
        ((:button :type "button" :class "btn btn-default" :data-dismiss "modal") "Close")))))))

(defun bs-std-pick-treeview-html-fn (dispatcher)
  (flet ((draw-item (node)
	   (let* ((name (first node))
		  (text (first name))
		  (value (second name)))
	     (if value
		 (html:html "&nbsp;&nbsp;"
			    ((:a :fformat (:href "javascript:f42('~a');"
						 (if (stringp value)
						     (html:quote-javascript-string value)
						     value)))
			     (html:esc text)))
		 (html:html "&nbsp;&nbsp;"
			    (html:esc text))))))
    (let* ((item (interface::item dispatcher))
	   (item-name (interface::name item))
	   (object (interface::object dispatcher))
	   (slot (interface::slot dispatcher))
	   (null-allowed (meta::null-allowed (slot dispatcher))))
      (html:html
     ((:div :class "modal-dialog")
      ((:div :class "modal-content")
       ((:div :class "modal-header")
        ((:h4 :class "modal-title"))
        (:translate (meta::get-value-title slot) :default '(:en "Choose a value" :fr "Choisissez une valeur" :sp "Elija un valor")))
       ((:div :class "modal-body")
        (:p (:translate (meta::get-value-text slot)))
        (:jscript "var shot;function f42(d){if (!shot) {parent.Fch('" item-name "',d);"
                       "shot = true;parent.$('#global_modal').modal('hide');}};")
        (when dispatcher
          (bs-simple-tree (funcall (meta::get-object-func (slot dispatcher))(object dispatcher)))
          (when (meta::null-allowed (slot dispatcher))
            (html:html "&nbsp;&nbsp;"
                       ((:a :href "javascript:f42('nil');")
                        (:translate '(:en "None of these choices" :fr "Aucun de ces choix"
                                      :sp "Ninguna de estas opciones"))) :br :br))))
       ((:div :class "modal-footer")
        ((:button :type "button" :class "btn btn-default" :data-dismiss "modal") "Close"))))))))

(defvar %bs-tree-id% 0)

(defun bs-simple-tree (tree)
  (incf %bs-tree-id%)
  (html:html
   ((:div :class "panel-group" :fformat (:id "tree~d" %bs-tree-id%))
    ((:div :class "panel panel-default")
     ((:div :class "panel-heading")
      ((:h4 :class "panel-title")
       ((:a :data-toggle "collapse" :fformat (:data-parent "#tree~d" %bs-tree-id%) :href "#"))
       (:esc (caar tree))))
     (html:html
      ((:div :class "panel-collapse collapse.in")
       ((:div :class "panel-body")
        ((:div :class "list-group")
         (loop for ((text object-id) . rest) in (cdr tree)
            for node in (cdr tree)
            do (if rest
                   (bs-simple-tree node)
                   (when object-id
                     (html:html
                      ((:a :class "list-group-item" :fformat (:href "javascript:f42('~a');" object-id))
                       (html:esc text))))))))))))))

;;**** slot-list ***************************

#|
(defclass bs-slot-list (html-item)
  ((action-func  :initform nil :accessor action-func :initarg :action-fn)
   (html-fn :accessor html-fn :initform nil :initarg :html-fn)
   (choices-fn :accessor choices-fn :initform nil :initarg :choices-fn)
   (list-format :accessor list-format :initform nil)
   (table-class  :initform ""  :accessor table-class)))

(defclass bs-slot-list-dispatcher (slot-dispatcher)
  ((start :accessor start :initform 0)
   (max-nb :accessor max-nb :initform 25)
   (list-format :accessor list-format :initform nil)
   (objects-to-delete :accessor objects-to-delete :initform nil)
   (selected-objects-idx :accessor selected-objects-idx :initform '())
   (sub-classes :accessor sub-classes :initform nil)))

(defmethod make-dispatcher (interface object (item bs-slot-list))
  (let ((dispatcher (make-instance 'bs-slot-list-dispatcher :interface interface :object object :item item)))
    (setf (list-format dispatcher)
	  (or (list-format item)
	      (find-best-list-format (meta::value-type (slot dispatcher)) *country-language* *user-groups* *frontend*)))
    dispatcher))

(defmethod make-set-status-javascript ((item bs-slot-list) status slot)
  "")

(defmethod bs-slot-list-action-fn (object value click-str)
  (let* ((slot (slot *dispatcher*))
         (*dispatcher* *dispatcher*)
	 (list (funcall (get-value-fn *dispatcher*) object))
	 (list-length (length list)))    
    (if (item-state *dispatcher*)
        (let* ((choice (elt (item-state *dispatcher*) (1- (abs value))))
               (list (funcall (get-value-fn *dispatcher*) object)))
          (if (plusp value)
              (funcall (set-value-fn *dispatcher*) (cons choice list) object)
              (funcall (set-value-fn *dispatcher*) (delete choice list) object)))
        (progn
          (collect-selected-objects-idx (when (find #\= click-str)
                                          (subseq click-str (1+ (position #\= click-str)))))
          (if (<= -4 value -1)
              (let ((start (start *dispatcher*))
                    (max-nb (max-nb *dispatcher*)))
                (case value
                  (-1 (setf start 0))
                  (-2 (setf start (max 0 (- start max-nb))))
                  (-3 (when (< (+ start max-nb) list-length)(incf start max-nb)))
                  (-4 (setf start (* max-nb (truncate list-length max-nb)))))
                (when (/= start (start *dispatcher*))
                  (setf (start *dispatcher*) start)
                  (mark-dirty-value *dispatcher*)))
              (if (and (= value -8)
                       *user*) ; copy
                  (copy-to-clipboard (clipboard *user*) (collect-list-objects list) object slot)
                  (when (modifiable-p *dispatcher*)
                    (cond
                      ((or (not value)
                           (<= 0 value 10000))
                       (unless (sub-classes *dispatcher*)
                         (setf (sub-classes *dispatcher*)
                               (remove-if-not 'visible-p (meta::instanciable-sub-classes (meta::value-type slot)))))
                       (when (= (length (sub-classes *dispatcher*)) 1)
                         (setf value 1))
                       (if (> value 0)
                           (let ((new-obj
                                  (make-instance (elt (sub-classes *dispatcher*) (1- value))
                                                 :parent (unless (meta::linked-value slot) object)
                                                 :store (meta::object-store object))))
                             (when (meta::process-new-object-fn slot)
                               (setf new-obj (funcall (meta::process-new-object-fn slot) new-obj object)))
                             (funcall (set-value-fn *dispatcher*) (if (meta::new-objects-first slot)
                                                                      (cons new-obj list)
                                                                      (nconc list (list new-obj)))
                                      object)
                             (send-url-to-interface (encode-object-url new-obj) (interface *dispatcher*)))
                           (send-to-interface
                            (html:fast-format nil "window.open1('/obj-new.html', '250px', '250px', '~a');"
                                              (name (item *dispatcher*))))))
                      ((= value -5)     ; (not (find 0 (selected-objects-idx *dispatcher*))))
                       (dolist (idx (collect-list-idx))
                         (unless (<= idx 0)
                           (rotatef (elt list (1- idx))(elt list idx))))
                       (map-into (selected-objects-idx *dispatcher*) #'(lambda (x) (1- x)) (selected-objects-idx *dispatcher*))
                       (funcall (set-value-fn *dispatcher*) list object))
                      ((= value -6)     ; (not (find 0 (selected-objects-idx *dispatcher*))))
                       (dolist (idx (nreverse (collect-list-idx)))
                         (unless (>= (1+ idx)  list-length)
                           (rotatef (elt list (1+ idx))(elt list idx))))
                       (map-into (selected-objects-idx *dispatcher*) #'(lambda (x) (1+ x)) (selected-objects-idx *dispatcher*))
                       (funcall (set-value-fn *dispatcher*) list object))
                      ((= value 30000)
                       (let ((to-delete (objects-to-delete *dispatcher*)))
                         (dolist (object to-delete)
                           (setf list (delete object list :count 1))))
                       (setf (selected-objects-idx *dispatcher*) nil)
                       (funcall (set-value-fn *dispatcher*) list object)
                       (setf (objects-to-delete *dispatcher*) nil))
                      ((= value 30001)
                       (setf (objects-to-delete *dispatcher*) nil))
                      ((= value -7)
                       (setf (objects-to-delete *dispatcher*) (collect-list-objects list))
                       #+nil
                       (send-to-interface
                        (html:fast-format nil "set_src('global_iframe', '/obj-del.html', '~a');$('#global_modal').modal('show');" (name (item *dispatcher*))))      
                       (send-to-interface
                        (html:fast-format nil "show_modal_content('Sure?', );"))
                       #+nil(send-to-interface
                        (html:fast-format nil "window.open1('/obj-del.html', '250px', '250px', '~a');"
                                          (name (item *dispatcher*)))))
                      ((and (= value -9) *user*) ; cut
                       (copy-to-clipboard (clipboard *user*) (collect-list-objects list) object slot)
                       (setf (objects-to-delete *dispatcher*)(collect-list-objects list))
                       (send-to-interface
                        (html:fast-format nil "set_src('global_iframe', '/obj-del.html', '~a');$('#global_modal').modal('show');" (name (item *dispatcher*)))
                        #+nil(html:fast-format nil "x_.open1('/obj-del.html', '250px', '250px', '~a');"
                                          (name (item *dispatcher*)))))
                      ((and (= value -10) *user*) ; paste
                       (paste-clipboard (clipboard *user*) object slot))))))))))
  
(defmethod make-set-value-javascript ((item bs-slot-list) list slot)
  (let ((*user* (user (or *session* (session (interface *dispatcher*))))))
    (let ((length (length list))
	  (max-nb (max-nb *dispatcher*))
	  (start (start *dispatcher*)))
      (when (> start (- length 4))
	(setf start (* max-nb (truncate length max-nb))))
      (when (< start 0) (setf start 0))
      (setf (start *dispatcher*) start)
      (html:fast-format nil "x_.fgt('~a').innerHTML='~a';"(name item)
	(html:quote-javascript-string
	 (html:html-to-string
	  (funcall (list-format-fn (list-format *dispatcher*)) start (nthcdr start list) max-nb length)))))))

;; moved from view -start
(defun std-list-checkbox (index start-index)
  (when t ;(modifiable-p *dispatcher*)
    (decf index start-index)
    (html::html ((:input :type "checkbox"
			 :fformat (:id "~aC~d" (name (item *dispatcher*)) index)
			 :optional (:checked (and (find index (selected-objects-idx *dispatcher*)) "true")))))))



(make-instance 'slot-list-format :name "default-format"
	       :header-fn #'(lambda (container-obj)
                              (declare (ignore container-obj))
                              )
	       :list-format-fn 'std-list-format-fn)
;; end

;;;css-classes suffixes:
;;; "" =  global
;;; "h" = header-table, "hr" = header row, "h1"..."hxx" = column xx header
;;; "t" = table, "r" = row, "c1"..."cxx" = column xx td style
;;; "b" = buttons 

(defvar *clipboard-view-context* nil)

(defun slot-list-tag (attributes forms)
  (destructuring-bind (slot-name &key (height nil) (width "100%") (class "stdlist") add-fn-only) attributes
    (declare (ignore height))
    (let ((slot (find (symbol-name slot-name) (c2mop:class-slots *current-class*)
		      :test #'string= :key #'c2mop:slot-definition-name)))
      (unless slot (error (format nil "Unknown slot : ~a" slot-name)))
      (let ((item (make-instance 'bs-slot-list :tooltip (meta::tooltip slot) :slot slot
				 :choices-fn (meta::get-object-func slot)
				 :html-fn (meta::get-value-html-fn slot)
				 :action-fn 'slot-list-action-fn :list-format (meta::list-format slot)))
	    (sub-obj-name (format nil #T(:en "Add ~a" :fr "Ajouter ~a" :sp "Agregar ~a")
				  (meta::translate (meta::user-name (find-class (meta::value-type slot)))))))
	(setf (table-class item) (concatenate 'string class "t"))
	(if add-fn-only
	    `(html:html
	      ,@(when t
		      `((:when t; (modifiable-p ,slot)
			  ((:span :align "right")
			   #+nil((:a :href ,(format nil "javascript:open1('/obj-pick2.html', '250px', '500px', '~a');" (name item)))
			    ,sub-obj-name)
                           ((:modal-button :id "obj-pick-button" :target "#global_modal"
                                           :onclick ,(format nil "set_src('global_iframe','/obj-pick2.html', '~a');" (name item)))
                            ,sub-obj-name)
                           )))))
	    `(html:html
	      ((:div :class ,class :style ,(format nil "width:~a;" width )) ;height:~a; height
	       ,@forms
	       ((:div :class ,(concatenate 'string class "h")
		      :style ,(format nil "width:100%;height:100%;overflow:auto;")
		      :id ,(name item)))
	       
	       ((:table :class ,(concatenate 'string class "h") :align "left" :width "100%")
		(:tr
		 (:td
		  ((:a :href ,(format nil "javascript:Fck('~a', -1);" (name item)(name item)))
		   ((:img :border "0" :src "/static/sl1.jpg" :width "22" :height "18" :title "First Page")))
		  ((:a :href ,(format nil "javascript:Fck('~a', -2);" (name item)(name item)))
		   ((:img :border "0" :src "/static/sl2.jpg" :width "22" :height "18" :title "Previous Page")))
		  ((:a :href ,(format nil "javascript:Fck('~a', -3);" (name item)(name item)))
		   ((:img :border "0" :src "/static/sl3.jpg" :width "22" :height "18" :title "Next Page")))
		  ((:a :href ,(format nil "javascript:Fck('~a', -4);" (name item)(name item)))
		   ((:img :border "0" :src "/static/sl4.jpg" :width "22" :height "18" :title "Last Page")))
		  (:when (modifiable-p ,slot)
                    ((:img :border "0" :src "/static/v.jpg" :width "22" :height "18" :title ""))
                    ((:a :href ,(format nil "javascript:Fck('~a', f854('~a',-5));" (name item)(name item)))
                     ((:img :border "0" :src "/static/up.jpg" :width "22" :height "18" :title "Move Up")))
                    ((:a :href ,(format nil "javascript:Fck('~a', f854('~a',-6));" (name item)(name item)))
                     ((:img :border "0" :src "/static/d.jpg" :width "22" :height "18" :title "Move Down")))
                    ((:a :href ,(format nil "javascript:Fck('~a', f854('~a',-7));" (name item)(name item)))
                     ((:img :border "0" :src "/static/k.jpg" :width "22" :height "18" :title "Remove"))))
		  ((:a :href ,(format nil "javascript:Fck('~a', f854('~a',-8));" (name item)(name item)))
		   ((:img :border "0" :src "/static/cp.jpg" :width "22" :height "18" :title "Copy")))
		  (:when (modifiable-p ,slot)
		    ((:a :href ,(format nil "javascript:Fck('~a', f854('~a',-9));" (name item)(name item)))
		     ((:img :border "0" :src "/static/cu.jpg" :width "22" :height "18" :title "Cut"))))
		  (:when (and (modifiable-p ,slot) *user*)
                    (let ((*clipboard-dest-context-item* ,item))
                      (html:html (:object-view :object (clipboard *user*) :name "clipbd")))))
		 ((:td :align "right")
		  ,@(when (meta::get-object-func slot)
			  `((:when (modifiable-p ,slot)
			      ((:span :align "right")
			       #+nil((:a :href ,(format nil "javascript:open1('/obj-pick2.html', '250px', '500px', '~a')"
					     (name item)))
				,sub-obj-name)
                               ((:modal-button :target "#global_modal"
                                 :onclick ,(format nil "set_src('global_iframe','/obj-pick2.html', '~a');" (name item)))
                                ,sub-obj-name)
                               ))))
		  ,@(when (and (meta::can-create-new-object slot) (not (meta::get-object-func slot)))
			  `((:when (modifiable-p ,slot)
			      ((:span :align "right")
			       ((:a :href ,(format nil "javascript:f825foc('~a');" (name item)))
				,sub-obj-name)))))))))))))))

(interface::add-named-url "/obj-pick2.html" 'pick2-request-handler)

(interface::add-named-url "/obj-new.html" 'obj-new-request-handler)

;(defun ask-yes-no-question (request title question yes-id no-id)

(interface::add-named-url "/obj-del.html" 'obj-del-request-handler)

(html:add-func-tag :slot-list 'slot-list-tag)
;;;syntax ((:slot-list :class "css" :width width :height height)
;;;         ("col-title" col-width col-forms)

;;***  pick-val ****************************

(defclass html-pick-val (html-item)
  ((html-fn :accessor html-fn :initform nil :initarg :html-fn)
   (choices-fn :accessor choices-fn :initform nil :initarg :choices-fn)
   (action-link :accessor action-link :initform nil :initarg :action-link)))

(defmethod initialize-instance :after ((item html-pick-val) &rest init-options &key &allow-other-keys)
  (setf (action-link item) (when (choices-fn item) (concatenate 'string (name item) "l"))))

(defclass html-pick-val-dispatcher (slot-dispatcher)
  ((state :accessor state :initform nil)
   (choices-fn :accessor choices-fn :initform nil :initarg :choices-fn)
   ))

(defmethod make-dispatcher (interface object (item html-pick-val))
  (make-instance 'html-pick-val-dispatcher :interface interface :object object :item item
		 :choices-fn (choices-fn item)))

(defmethod make-set-status-javascript ((item html-pick-val) status slot)
  (when (modifiable-p *dispatcher*)
    (when (action-link item)
      (if status
	  (concatenate 'string "x_.fgt('" (action-link item) "').style.visibility='hidden';")
	  (concatenate 'string "x_.fgt('" (action-link item) "').style.visibility='inherit';")))))

(defun slot-pick-val-tag (attributes form)
  (declare (ignore form))
  (destructuring-bind (slot-name . attrs) attributes
    (let ((slot (find (symbol-name slot-name) (c2mop:class-slots *current-class*)
		      :test #'string= :key #'c2mop:slot-definition-name)))
      (unless slot (error (format nil "Unknown slot : ~a" slot-name)))
      (let ((item (make-instance 'html-pick-val :tooltip (meta::tooltip slot) :slot slot
				 :choices-fn (meta::get-object-func slot)
				 :html-fn (or (meta::get-value-html-fn slot) 'std-pick-val-html-fn))))
	`(html:html ((:input :type "text" :id ,(name item) :disabled "true" ,@attrs))
	  (:when (modifiable-p ,slot)
            ((:modal-button :id ,(action-link item)
                            :target "#global_modal"
                            :onclick ,(format nil "set_src('global_iframe','/pick-val.html', '~a');" (name item)))
                            (:translate '(:en "Change" :fr "Changer" :sp "Cambio")))
	    #+nil((:a :id ,(action-link item)
		 :href ,(format nil "javascript:open1('/pick-val.html', '250px', '500px', '~a');"
				(name item))) (:translate '(:en "Change" :fr "Changer" :sp "Cambio")))))))))

(html:add-func-tag :slot-pick-val 'slot-pick-val-tag)

;;; pick-color

(defclass html-pick-color (html-pick-val)
  ())

(defmethod make-set-value-javascript ((item html-pick-color) value slot)
  (setf value (if value (html:quote-javascript-string value) ""))
  (html:fast-format nil "x_.fgt('~a').style.backgroundColor='~a';x_.fgt('~a').value='~a';"
		    (name item) value (name item) value))

(defun slot-pick-color-tag (attributes form)
  (declare (ignore form))
  (destructuring-bind (slot-name . attrs) attributes
    (let ((slot (find (symbol-name slot-name) (c2mop:class-slots *current-class*)
		      :test #'string= :key #'c2mop:slot-definition-name)))
      (unless slot (error (format nil "Unknown slot : ~a" slot-name)))
      (let ((item (make-instance 'html-pick-color :tooltip (meta::tooltip slot) :slot slot
				 :choices-fn t
				 :html-fn (or (meta::get-object-func slot) 'std-pick-color-html-fn))))
	`(html:html ((:input :type "text" :id ,(name item) :readonly "true" ,@attrs))
	  (:when (modifiable-p ,slot)
	    #+nil((:a :id ,(action-link item)
		 :href ,(format nil "javascript:open1('/pick-val.html', '400px', '500px', '~a')"
				(name item))) 
             ((:img :border "0" :src "/static/ch.png" :width "16" :height "16" :align "baseline" :title "Change")))
            ((:modal-button :id ,(action-link item)
                            :target "#global_modal"
                            :onclick ,(format nil "set_src('global_iframe','/pick-val.html', '~a');" (name item)))
                                ((:img :border "0" :src "/static/ch.png" :width "16" :height "16" :align "baseline" :title "Change")))
            ))))))

(html:add-func-tag :slot-pick-color 'slot-pick-color-tag)

;;; slot-pick-multi-val

(defclass html-pick-multi-val (html-pick-val)
  ((action-func  :initform nil :accessor action-func :initarg :action-fn)
   (current-list :initform nil :accessor current-list)))

(defmethod make-set-value-javascript ((item html-pick-multi-val) value slot)
  (if value
      (with-output-to-string (s)
	(html:fast-format s "x_.fgt('~a').innerHTML='" (name item))
	(loop for (val . rest) on value do
	      (write-string (html:quote-javascript-string (meta::short-description val)) s)
	      (when rest (write-string ", " s)))
	(write-string "';" s))
      (html:fast-format nil "x_.fgt('~a').innerHTML='';" (name item))))

(defmethod html-pick-multi-val-action-fn (object value click-str)
  (let* ((choice (elt (item-state *dispatcher*) (1- (abs value))))
	 (list (funcall (get-value-fn *dispatcher*) object)))
    (if (plusp value)
	(funcall (set-value-fn *dispatcher*) (cons choice list) object)
	(funcall (set-value-fn *dispatcher*) (delete choice list) object))))

(html:add-func-tag :slot-pick-mval 'slot-pick-multi-val-tag)

;;;; File upload

(let ((counter 0))
  (defun handle-file (post-parameter hunchentoot-request)
    "Taken from hunchentoot-test"
    (when (and post-parameter
               (listp post-parameter))
      (destructuring-bind (path file-name content-type) post-parameter
        (let ((new-path (make-pathname :name (format nil "hunchentoot-test-~A"
                                                     (incf counter))
                                       :type nil
                                       :defaults *tmp-test-directory*)))
          ;; strip directory info sent by Windows browsers
          (when (search "Windows" (hunchentoot:user-agent hunchentoot-request) :test 'char-equal)
            (setq file-name (cl-ppcre:regex-replace ".*\\\\" file-name "")))
          (rename-file path (ensure-directories-exist new-path))
          (push (list new-path file-name content-type) *tmp-test-files*))))))

(defun file-upload-page ()
  (html:html
    (:html
      (:head (:title "Hunchentoot file upload test"))
      (:body
       (:h2 "File upload test")
       ((:form :method "post" :enctype "multipart/form-data")
        (:p "Chose file: " ((:input :type "file"
                                    :name "file")))
        (:p ((:input :type "submit"))))
       (when *tmp-test-files*
         (html:html
           (:p
            ((:table :border 1 :cellpadding 2 :cellspacing 0)
             (:tr ((:td :colspan 3) (:b "Uploaded files")))
             (loop
                for (path file-name nil) in *tmp-test-files* ;; third value is MIME-type
                for i from 1
                collect
                  (html:html (:tr ((:td :align "right") i)
                                  (:td ((:a :href (format nil "files/~A?path=~A"
                                                          (hunchentoot:url-encode file-name)
                                                          (hunchentoot:url-encode (namestring path))))
                                        file-name)))))))))))))
    
(defun file-upload-test-request-handler (request)
  (let ((hunchentoot-request (hunchentoot-request request)))
    ;; (print (hunchentoot:post-parameters (hunchentoot-request request)))
    (when (hunchentoot::post-parameter "file" hunchentoot-request)
      (handle-file (hunchentoot:post-parameter "file") hunchentoot-request))
    (interface::with-output-to-request (request html:*html-stream*)
      (file-upload-page))))

(interface::add-named-url "/upload.html" 'file-upload-test-request-handler)

#+nil
("posted-content" . "-----------------------------198810850710137454481039072866
Content-Disposition: form-data; name=\"file\"; filename=\".X0-lock\"
Content-Type: application/octet-stream

      6026

-----------------------------198810850710137454481039072866--
")

;;;; Modal window

(defun modal-button-tag (attributes form)
  (destructuring-bind (&key target (role "button") (class "btn") (data-toggle "modal") (onclick "") (id "")) attributes
    `(html:html
       ((:a :data-target ,target :role ,role :class ,class :data-toggle ,data-toggle :onclick ,onclick :id ,id) ,@form))))

(html:add-func-tag :modal-button 'modal-button-tag)

(defun modal-window-tag (attributes form)
  (destructuring-bind (&key id (role "button") (class "modal fade in") (tabindex "-1") aria-labelledby) attributes
    (let ((header (cdr (assoc :header form)))
          (body (cdr (assoc :body form)))
          (footer (cdr (assoc :footer form))))
      `(html:html
         ((:div :id ,id :class ,class :tabindex ,tabindex :role ,role :aria-labelledby ,aria-labelledby :aria-hidden "true")
          ((:div :class "modal-dialog")
           ((:div :class "modal-content")
            ((:div :class "modal-header")
             ((:button :type "button" :class "close" :data-dismiss "modal" :aria-hidden "true" ) "×")
             ,@header)
            ((:div :class "modal-body")
             ,@body)
            ((:div :class "modal-footer")
             ,@footer))))))))

(html:add-func-tag :modal-window 'modal-window-tag)

;; usage:
;; ((:modal-button :target "#myModal") "Launch demo modal")
;;
;; ((:modal-window :id "myModal")
;;  (:header ((:h3 :id "myModalLabel") "Modal header"))
;;  (:body (:p "body1") (:p "body2"))
;;  (:footer ((:button :class "btn" :data-dismiss "modal" :aria-hidden "true") "Close")
;;            ((:button :class "btn btn-primary") "Save changes")))

;; Modal Example

(defun modal-test-request-handler (request)
  (interface::with-output-to-request (request html:*html-stream*)
    (html:html
      (:html
        (:head
         (:title "Modal upload test")
         ((:link :rel "stylesheet" :href "//netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap.min.css"))
         ((:link :rel "stylesheet" :href "//netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap-theme.min.css")))
        (:body
         (:h2 "Modal test")
         ((:modal-button :target "#myModal") "Launch demo modal")
         ((:a :onclick "javascript:$('#myModal').modal('show');" :href "javascript:void(0);") "JS test")
         ((:modal-window :id "myModal")
          (:header ((:h3 :id "myModalLabel") "Modal header"))
          (:body (:p "body1") (:p "body2"))
          (:footer ((:button :class "btn" :data-dismiss "modal" :aria-hidden "true") "Close")
                   ((:button :class "btn btn-primary") "Save changes")))
         ((:script :type "text/javascript"  :src "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"))
         ((:script :type "text/javascript"  :src "http://netdna.bootstrapcdn.com/bootstrap/3.0.0/js/bootstrap.min.js")))))))

(interface::add-named-url "/modal.html" 'modal-test-request-handler)
|#
