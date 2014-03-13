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
      (format nil "x_.$('#~a').data('DateTimePicker').setDate(~d);"
              (name item)
              (* (- value #.(encode-universal-time 0 0 0 1 1 1970)) 1000))
      #+nil
      (multiple-value-bind (s mn h d m y) 
          (if meta::*GMT-time* (decode-universal-time value 0)(decode-universal-time value))
	(let ((iso-date (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" y m d h mn s)
                #+nil(if (not (show-time item))
                            (format nil "~4,'0d-~2,'0d-~2,'0d" y m d)
                            (if (show-date item)
                                (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" y m d h mn s)
                                (format nil "~2,'0d:~2,'0d:~2,'0d" h mn s)))))
	  (concatenate 'string "x_.$('#" (name item) "').data('DateTimePicker').setDate('" iso-date "');")))
      (concatenate 'string "x_.$('#" (name item) "').data('DateTimePicker').setDate('');")))

(defmethod make-set-status-javascript ((item bs-date) status slot)
  (when (modifiable-p *dispatcher*)
    (concatenate 'string "x_.$('#" (name item) "').data('DateTimePicker')"
                 (if status ".disable();" ".enable();"))))

(defmethod slot-date-edit-tag ((frontend bootstrap) attributes form)
  (declare (ignore form))
  (destructuring-bind (slot-name . attrs) attributes
    (let ((slot (find (symbol-name slot-name) (c2mop:class-slots *current-class*)
		      :test #'string= :key #'c2mop:slot-definition-name)))
      (unless slot (error (format nil "Unknown slot : ~a" slot-name)))
      (let ((edit (make-instance 'bs-date :tooltip (meta::tooltip slot) :slot slot
				 :show-time (getf attrs :show-time)
                                 :show-date (getf attrs :show-date t))))
	`(html:html
          ((:div :class "input-group date" :id ,(name edit))
           ((:input :type "text" :class "form-control"))
           ((:span :class "input-group-addon")((:span :class "glyphicon glyphicon-calendar"))))
          (:jscript "$(function () { $('#" ,(name edit) "').datetimepicker("
                    ,@(when (not (show-date edit)) '("{pickDate: false}"))
                    ,@(when (not (show-time edit)) '("{pickTime: false}"))
                ");
                     $('#" ,(name edit) "').data('DateTimePicker').disable();
                     $('#" ,(name edit)"').on('change.dp',function (e) {
               Fch('" ,(name edit) "', 'ctime'+$('#" ,(name edit) "').data('DateTimePicker').getDate());});});"))))))

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
                             :onclick ,(format nil "show_remote_modal_content('~a','/pick-val.html', '~a');"
                                               (meta::translate (meta::get-value-title slot)
                                                                :default '(:en "Choose an object" :fr "Choisissez un objet" :sp "Elija un objeto"))
                                             (name obj-link)))
                         ((:span :class "glyphicon glyphicon-edit")"&nbsp;Change..."))))))))))))

(defun bs-std-pick-obj-html-fn (dispatcher)
  (let* ((item (item dispatcher))
	 (item-name (name item))
	 ;(object (object dispatcher))
	 (slot (slot dispatcher)))
    (html:html
     ((:div :class "modal-header")
      ((:button :type "button" :class "close pull-right" :data-dismiss "modal") "&times;")
      ((:h4 :class "modal-title")
       (:translate (meta::get-value-title slot)
                   :default '(:en "Choose an object" :fr "Choisissez un objet" :sp "Elija un objeto"))))
     ((:div :class "modal-body")
      (:p (:translate (meta::get-value-text slot)))
      (:jscript "function f42(d){Fch('" item-name "',d);$('#GlobalModal').modal('hide');};")
      ((:div :class "list-group")
       (when dispatcher
         (loop for object in (funcall (meta::get-object-func (slot dispatcher))(object dispatcher))
            do (html:html
                ((:a :class "list-group-item" :fformat (:href "javascript:f42('~a');" (encode-object-id object)))
                 (html:esc (meta::short-description object)))))
         (when (meta::null-allowed (slot dispatcher))
           (html:html "&nbsp;&nbsp;"
                      ((:a :href "javascript:f42('nil');")
                       (:translate '(:en "None of these choices" :fr "Aucun de ces choix" :sp "Ninguna de estas opciones"))))))))
     ((:div :class "modal-footer")
      ((:button :type "button" :class "btn btn-default" :data-dismiss "modal") "Close")))))


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
			     (:esc text)))
		 (html:html "&nbsp;&nbsp;" (:esc text))))))
    (let* ((item (interface::item dispatcher))
	   (item-name (interface::name item))
	   (object (interface::object dispatcher))
	   (slot (interface::slot dispatcher))
	   (null-allowed (meta::null-allowed (slot dispatcher))))
      (html:html
       ((:link :rel "stylesheet" :href "/static/css/treeview.css"))
       ((:div :class "modal-header")
        ((:button :type "button" :class "close pull-right" :data-dismiss "modal") "&times;")
        ((:h4 :class "modal-title")
         (:translate (meta::get-value-title slot) :default '(:en "Choose a value" :fr "Choisissez une valeur" :sp "Elija un valor"))))
       ((:div :class "modal-body")
        (:p (:translate (meta::get-value-text slot)))
        (:jscript "function f42(d){Fch('" item-name "',d);$('#GlobalModal').modal('hide');};")
        (when dispatcher
          (bs-simple-tree (funcall (meta::get-object-func (slot dispatcher))(object dispatcher)))
          (when (meta::null-allowed (slot dispatcher))
            (html:html "&nbsp;&nbsp;"
                       ((:a :href "javascript:f42('nil');")
                        (:translate '(:en "None of these choices" :fr "Aucun de ces choix"
                                      :sp "Ninguna de estas opciones")))))))
       ((:div :class "modal-footer")
        ((:button :type "button" :class "btn btn-default" :data-dismiss "modal") "Close"))
       ((:script :src "/static/js/treeview.js"))))))

(defvar %bs-tree-id% 0)

(defun bs-simple-tree (tree)
  (html:html
   ((:div :class "tree")
    (:ul
     (loop for ((text object-id) . rest) in (cdr tree)
        for node in (cdr tree)
        do (cond ((or rest (and (not rest) (not object-id)))
                  (bs-simple-tree-node node))
                 (object-id
                  (html:html
                   (:li ((:a :fformat (:href "javascript:f42('~a');" object-id)) (:esc text)))))))))))

(defun bs-simple-tree-node (node)
  (html:html
   (:li (:span ((:i :class "glyphicon glyphicon-minus")) "&nbsp;"(:esc (caar node)))
        (:ul
         (loop for ((text object-id) . rest) in (cdr node)
            for node in (cdr node)
            do (cond ((or rest (and (not rest) (not object-id)))
                      (bs-simple-tree-node node))
                     (object-id
                      (html:html
                       (:li (:span ((:a :fformat (:href "javascript:f42('~a');" object-id)) (:esc text))))))))))))

;;***  pick-val ****************************

(defclass bs-pick-val (html-item)
  ((html-fn :accessor html-fn :initform nil :initarg :html-fn)
   (choices-fn :accessor choices-fn :initform nil :initarg :choices-fn)
   (action-link :accessor action-link :initform nil :initarg :action-link)))

(defmethod initialize-instance :after ((item bs-pick-val) &rest init-options &key &allow-other-keys)
  (setf (action-link item) (when (choices-fn item) (concatenate 'string (name item) "l"))))

(defclass bs-pick-val-dispatcher (slot-dispatcher)
  ((state :accessor state :initform nil)
   (choices-fn :accessor choices-fn :initform nil :initarg :choices-fn)
   ))

(defmethod make-dispatcher (interface object (item bs-pick-val))
  (make-instance 'bs-pick-val-dispatcher :interface interface :object object :item item
		 :choices-fn (choices-fn item)))

(defmethod make-set-value-javascript ((item bs-pick-val) value slot)
  (setf value (if value (html:quote-javascript-string (format nil "~a" value)) ""))
  (html:fast-format nil "x_.fgt('~a').value='~a';" (name item) value))

(defmethod make-set-status-javascript ((item bs-pick-val) status slot)
  (when (modifiable-p *dispatcher*)
    (when (action-link item)
      (if status
	  (concatenate 'string "x_.fgt('" (action-link item) "').disabled=true;")
	  (concatenate 'string "x_.fgt('" (action-link item) "').disabled=false;")))))

(defmethod slot-pick-val-tag ((frontend bootstrap) attributes form)
  (declare (ignore form))
  (destructuring-bind (slot-name . attrs) attributes
    (let ((slot (find (symbol-name slot-name) (c2mop:class-slots *current-class*)
		      :test #'string= :key #'c2mop:slot-definition-name)))
      (unless slot (error (format nil "Unknown slot : ~a" slot-name)))
      (let* ((item (make-instance 'bs-pick-val :tooltip (meta::tooltip slot) :slot slot
                                  :choices-fn (meta::get-object-func slot)
                                  :html-fn (or (meta::get-value-html-fn slot) 'bs-std-pick-val-html-fn)))
             (on-click (format nil "show_remote_modal_content('','/pick-val.html', '~a');" (name item))))
        `(html:html
          ((:div :class "input-group")
           ((:input :type "text" :class "form-control" :disabled "true" :id ,(name item) ,@attrs))
           ,@(when (action-link item)
                   `((:when (modifiable-p ,slot)
                       ((:span :class "input-group-btn")
                        ((:button :class "btn btn-default" :id ,(action-link item) :type "button" :onclick ,on-click)
                         ((:span :class "glyphicon glyphicon-edit")"&nbsp;Change..."))))))))))))

(defun bs-std-pick-val-html-fn (dispatcher)
  (let* ((item (item dispatcher))
	 (item-name (name item))
	 ;(object (object dispatcher))
	 (slot (slot dispatcher)))
    (html:html
     ((:div :class "modal-header")
      ((:button :type "button" :class "close pull-right" :data-dismiss "modal") "&times;")
      ((:h4 :class "modal-title")
       (:translate (meta::get-value-title slot)
                   :default '(:en "Choose an object" :fr "Choisissez un objet" :sp "Elija un objeto"))))
     ((:div :class "modal-body")
      (:p (:translate (meta::get-value-text slot)))
      (:jscript "function f42(d){Fch('" item-name "',d);$('#GlobalModal').modal('hide');};")
      ((:div :class "list-group")
       (when dispatcher
         (loop for (text value) in (funcall (choices-fn dispatcher)(object dispatcher))
            do (html:html
                ((:a :class "list-group-item"
                     :fformat (:href "javascript:f42('~a');"
                                     (if (stringp value) (html:quote-javascript-string value) value)))
                 (:esc text))))))
      (when (meta::null-allowed (slot dispatcher))
        (html:html "&nbsp;&nbsp;"
                   ((:a :href "javascript:f42('nil');")
                    (:translate '(:en "None of these choices" :fr "Aucun de ces choix" :sp "Ninguna de estas opciones"))))))
     ((:div :class "modal-footer")
      ((:button :type "button" :class "btn btn-default" :data-dismiss "modal") "Close")))))

;;; pick-color

(defclass bs-pick-color (bs-pick-val)
  ())

(defmethod make-set-value-javascript ((item bs-pick-color) value slot)
  (setf value (if value (html:quote-javascript-string value) ""))
  (html:fast-format nil "x_.fgt('~a').style.backgroundColor='~a';x_.fgt('~a').value='~a';"
		    (name item) value (name item) value))

(defmethod make-set-status-javascript ((item bs-pick-color) status slot)
  (when (modifiable-p *dispatcher*)
    (when (action-link item)
      (if status
	  (concatenate 'string "x_.fgt('" (action-link item) "').disabled=true;")
	  (concatenate 'string "x_.fgt('" (action-link item) "').disabled=false;")))))

(defmethod slot-pick-color-tag ((frontend bootstrap) attributes form)
  (declare (ignore form))
  (destructuring-bind (slot-name . attrs) attributes
    (let ((slot (find (symbol-name slot-name) (c2mop:class-slots *current-class*)
		      :test #'string= :key #'c2mop:slot-definition-name)))
      (unless slot (error (format nil "Unknown slot : ~a" slot-name)))
      (let* ((item (make-instance 'bs-pick-color :tooltip (meta::tooltip slot) :slot slot
                                  :choices-fn t
                                  :html-fn (or (meta::get-value-html-fn slot) 'bs-std-pick-color-html-fn)))
             (on-click (format nil "show_remote_modal_content('','/pick-val.html', '~a');" (name item))))
        `(html:html
          ((:div :class "input-group")
           ((:input :type "text" :class "form-control" :disabled "true" :id ,(name item) ,@attrs))
           (:when (modifiable-p ,slot)
             ((:span :class "input-group-btn")
              ((:button :class "btn btn-default" :id ,(action-link item) :type "button" :onclick ,on-click)
               ((:span :class "glyphicon glyphicon-edit")"&nbsp;Change..."))))))))))
  

(defun bs-std-pick-color-html-fn (dispatcher)
  (flet ((color-td (r g b)
	   (let ((color (format nil "#~2,'0x~2,'0x~2,'0x" r g b)))
	     (html:html ((:td :bgcolor color :fformat (:onclick "f42('~a');" color))"&nbsp;&nbsp;&nbsp;")))))
    (let* ((item (item dispatcher))
           (item-name (name item))
                                        ;(object (object dispatcher))
           (slot (slot dispatcher))
           (colors nil))
      (dotimes (r 6)
        (dotimes (g 6)
          (dotimes (b 6)
            (push (list (* r 51) (* g 51) (* b 51)(luminance r g b)) colors))))
      (setf colors (sort colors #'> :key 'fourth))
      (html:html
       ((:div :class "modal-header")
        ((:button :type "button" :class "close pull-right" :data-dismiss "modal") "&times;")
        ((:h4 :class "modal-title")
         (:translate (meta::get-value-title slot)
                     :default '(:en "Choose a color" :fr "Choisissez une couleur"))))
       ((:div :class "modal-body")
        (:p (:translate (meta::get-value-text slot)))
        (:jscript "function f42(d){Fch('" item-name "',d);$('#GlobalModal').modal('hide');};")
        ((:div :class "list-group")
         (:when dispatcher
           ((:table :class "pcolt" :align "center")
            (loop for x below 18
	       for row = (loop repeat 12 collect (pop colors))
	       for bl = (round (* 255 (- 1 (/ x 17))))
	       do
	       (html:html
		(:tr
		 (color-td  bl bl bl)
		 (color-td  bl  0  0)
		 (color-td   0 bl  0)
		 (color-td   0  0 bl)
		 (color-td   0 bl bl)
		 (color-td  bl  0 bl)
		 (color-td  bl bl  0)
		 (loop for (r g b nil) in row
		       do (color-td r g b))))))))
      (when (meta::null-allowed (slot dispatcher))
        (html:html "&nbsp;&nbsp;"
                   ((:a :href "javascript:f42('nil');")
                    (:translate '(:en "None of these choices" :fr "Aucun de ces choix" :sp "Ninguna de estas opciones"))))))
     ((:div :class "modal-footer")
      ((:button :type "button" :class "btn btn-default" :data-dismiss "modal") "Close"))))))

;;; slot-pick-multi-val

(defclass bs-pick-multi-val (bs-pick-val)
  ((action-func  :initform nil :accessor action-func :initarg :action-fn)
   (current-list :initform nil :accessor current-list)))

(defmethod make-set-value-javascript ((item bs-pick-multi-val) value slot)
  (if value
      (with-output-to-string (s)
	(html:fast-format s "x_.fgt('~a').value='" (name item))
	(loop for (val . rest) on value do
	      (write-string (html:quote-javascript-string (meta::short-description val)) s)
	      (when rest (write-string ", " s)))
	(write-string "';" s))
      (html:fast-format nil "x_.fgt('~a').value='';" (name item))))

(defun bs-pick-multi-val-action-fn (object value click-str)
  (let* ((choice (elt (item-state *dispatcher*) (1- (abs value))))
	 (list (funcall (get-value-fn *dispatcher*) object)))
    (if (plusp value)
	(funcall (set-value-fn *dispatcher*) (cons choice list) object)
	(funcall (set-value-fn *dispatcher*) (delete choice list) object))))

(defmethod slot-pick-multi-val-tag ((frontend bootstrap) attributes form)
  (declare (ignore form))
  (destructuring-bind (slot-name . attrs) attributes
    (let ((slot (find (symbol-name slot-name) (c2mop:class-slots *current-class*)
		      :test #'string= :key #'c2mop:slot-definition-name)))
      (unless slot (error (format nil "Unknown slot : ~a" slot-name)))
      (let* ((item (make-instance 'bs-pick-multi-val :tooltip (meta::tooltip slot) :slot slot
                                  :choices-fn (or (meta::get-object-func slot) 'std-get-mval-choices)
                                  :action-fn 'bs-pick-multi-val-action-fn
                                  :html-fn (or (meta::get-value-html-fn slot) 'bs-std-pick-multi-val-html-fn))))
        `(html:html
          ((:div :class "input-group")
           ((:input :type "text " :class "form-control" :disabled "true" :id ,(name item) ,@attrs))
           ,@(when (action-link item)
                   `((:when (modifiable-p ,slot)
                       ((:span :class "input-group-btn")
                        ((:button :class "btn btn-default" :id ,(action-link item) :type "button"
                                  :onclick ,(format nil "show_remote_modal_content('','/pick-val.html', '~a');" (name item)))
                         ((:span :class "glyphicon glyphicon-edit"))"&nbsp;Change...")))))))))))
      #+nil
        `(html:html
          ((:span :id ,(name item) ,@attrs))
          (:when (modifiable-p ,slot)
          ((:modal-button :id ,(action-link item)
                          :target "#global_modal"
                          :onclick ,(format nil "set_src('global_iframe','/pick-val.html', '~a');" (name item)))
           ((:img :border "0" :src "/static/ch.png" :width "16" :height "16" :align "top" :title "Change")))))

(defun bs-std-pick-multi-val-html-fn (dispatcher)
  (let* ((item (interface::item dispatcher))
	 (item-name (interface::name item))
	 (object (interface::object dispatcher))
	 (slot (interface::slot dispatcher))
	 (slot-value (funcall (get-value-fn dispatcher) object))
	 (choices ()))
    (html:html
     ((:div :class "modal-header")
      ((:button :type "button" :class "close pull-right" :data-dismiss "modal") "&times;")
      ((:h4 :class "modal-title")
       (:translate (meta::get-value-title slot)
                   :default '(:en "Choose" :fr "Choisissez" :sp "Elija"))))
     ((:div :class "modal-body")
      (:p (:translate (meta::get-value-text slot)))
      (:jscript "function f42(i,st){if (st) Fck('" item-name "',i);"
		"else Fck('" item-name "',-i);};")
      ((:form :class "form-horizontal")
       (when dispatcher
         (loop for (text value) in (funcall (choices-fn dispatcher)(object dispatcher))
              for i from 0
            do (html:html
                ((:div :class "form-group")
                       ((:div :class "checkbox col-sm-offset-3 col-sm-7 col-md-7 col-lg-6")
                        (:label
                         ((:input :type "checkbox" :fformat (:id "CB~d" i)
                                  :fformat (:onclick "f42(~d,CB~a.checked);" (1+ i) i)
                                  :insert-string (if (member value slot-value :test #'equal) "CHECKED" "")))
                         (:esc text)))))
              (push value choices))
         (setf (item-state dispatcher) (nreverse choices)))))
     ((:div :class "modal-footer")
      ((:button :type "button" :class "btn btn-default" :data-dismiss "modal") "Close")))))

(defun bs-obj-del-request-handler (request)
  (decode-posted-content request)
  (let ((link (cdr (assoc "link" (posted-content request) :test 'string=)))
        (item (cdr (assoc "item" (posted-content request) :test 'string=)))
        (dispatcher nil))
    (when link (setf link (gethash link *http-links*)))
    (when (and link item) (setf dispatcher (gethash item (dispatchers link))))
    (let* ((*session* (session link))
           (*user* (user *session*))
           (*country-language* (country-language *session*))
           (not-linked-value (not (meta::linked-value (slot dispatcher)))))
      (with-output-to-request (request)
        (html::html-to-stream
         *request-stream*
         ((:div :class "modal-header")
          ((:button :type "button" :class "close pull-right" :data-dismiss "modal") "&times;")
          ((:h4 :class "modal-title")
           (:if (> (length (objects-to-delete dispatcher)) 1)
                (:translate '(:en "Do you want to remove:"
                              :sp "Está seguro de querer eliminar:"
                              :fr "Voulez vous vraiment supprimer:"))
                (:translate '(:en "Do you want to remove:"
                              :sp "Está seguro de querer eliminar:"
                              :fr "Voulez vous vraiment supprimer:")))))
         ((:div :class "modal-body")
                                        ;   (:p (:translate (meta::get-value-text slot)))
          (:jscript "function f42(d){Fck('" item "',d);$('#GlobalModal').modal('hide');};")
          (dolist (object (objects-to-delete dispatcher))
            (html:html "&nbsp;&nbsp;&nbsp;&nbsp;" (html:esc (meta:short-description object)) :br)))
         ((:div :class "modal-footer")
          (:when not-linked-value
            ((:div :align "center")
             ((:p :class "bg-danger")
              ((:span :class "glyphicon glyphicon-warning-sign")) " This cannot be undone.")))
          ((:button :type "button" :class (if not-linked-value "btn btn-danger" "btn btn-primary")
                    :onclick "f42('30000')" :data-dismiss "modal") "Yes - Remove")
          ((:button :type "button" :class "btn btn-default" :onclick "f42('30001')" :data-dismiss "modal") "No - Cancel"))))))
    t)

(interface::add-named-url "/bs-obj-del.html" 'bs-obj-del-request-handler)

