(in-package interface)

;;; ***** slot-name ************

(defun slot-name-tag (attributes form)
  (let ((slot (find (symbol-name (first form)) (clos:class-slots *current-class*)
		    :test #'string= :key #'clos:slot-definition-name)))
    (unless slot (error (format nil "Slot inconnu : ~a" (first form))))
    `(write-string ,(get-user-name slot) html:*html-stream*)))

(html:add-func-tag :slot-name 'slot-name-tag)

;;;***** slot edit **************

(defclass html-edit (html-item)
  ())

(defmethod make-set-value-javascript ((item html-edit) value slot)
  (when (meta::choices slot)
    (setf value (meta::translate (second (assoc value (meta::choices slot))))))
  (let ((j-value (html:quote-javascript-string
		  (if (stringp value) value (write-to-string value)))))
    (if (modifiable-p *dispatcher*)
	(concatenate 'string "x_.f826svi('" (name item) "', '" j-value "');")
	(concatenate 'string "x_.f826si('" (name item) "', '" j-value "');"))))


(defmethod make-set-status-javascript ((item html-edit) status slot)
  (when (modifiable-p *dispatcher*)
    (if status
	(concatenate 'string "x_.f8252h('" (name item) "');")
	(concatenate 'string "x_.f8252s('" (name item) "');"))))

(defun slot-edit-tag (attributes form)
  (destructuring-bind (slot-name . attrs) attributes
    (let ((slot (find (symbol-name slot-name) (clos:class-slots *current-class*)
		      :test #'string= :key #'clos:slot-definition-name)))
      (unless slot (error (format nil "Slot inconnu : ~a" slot-name)))
      (let* ((edit (make-instance 'html-edit :tooltip (meta::tooltip slot) :slot slot
                                  :force-visible (getf attrs :force-visible)))
	     (name nil)
	     (fire nil))
	(remf attrs :force-visible)
	`(html:html (:if (modifiable-p ,slot)
			 (:progn
			   ((:input :type "text" :id ,(name edit) :style "display:none;" :insert-string
				    ,(format nil "onchange='Fch(~s,~a.value);'"
					     (name edit) (name edit))
				    ,@attrs))
			   ((:span :id ,(concatenate 'string (name edit) "d") :style "display:none;")))
			 ((:span :id ,(concatenate 'string (name edit) "d") ,@attrs))))))))

(html:add-func-tag :slot-edit 'slot-edit-tag)

;;;***** slot span **************

(defclass html-span (html-item)
  ())

(defmethod make-set-value-javascript ((item html-span) value slot)
  (when (meta::choices slot)
    (setf value (meta::translate (second (assoc value (meta::choices slot))))))
  (let ((j-value (html:quote-javascript-string
		  (if (stringp value) value (write-to-string value)))))
    (concatenate 'string "x_.fgt('" (name item) "').innerHTML='" j-value "';")))


(defmethod make-set-status-javascript ((item html-span) status slot)
  )

(defun slot-span-tag (attributes form)
  (destructuring-bind (slot-name . attrs) attributes
    (let ((slot (find (symbol-name slot-name) (clos:class-slots *current-class*)
		      :test #'string= :key #'clos:slot-definition-name)))
      (unless slot (error (format nil "Slot inconnu : ~a" slot-name)))
      (let* ((edit (make-instance 'html-span :tooltip (meta::tooltip slot) :slot slot)))
	`(html:html ((:span :id ,(name edit) ,@attrs)))))))

(html:add-func-tag :slot-span 'slot-span-tag)

;;**** Multiline Edit ***************************

(defclass html-medit (html-edit)
  ())

(defun slot-medit-tag (attributes form)
  (destructuring-bind (slot-name . attrs) attributes
    (let ((slot (find (symbol-name slot-name) (clos:class-slots *current-class*)
		      :test #'string= :key #'clos:slot-definition-name)))
      (unless slot (error (format nil "Slot inconnu : ~a" slot-name)))
      (let ((edit (make-instance 'html-medit :tooltip (meta::tooltip slot) :slot slot
				 :force-visible (getf attrs :force-visible))))
	(setf attrs (copy-list attrs))
	(remf attrs :force-visible)
	`(html:html (:if (modifiable-p ,slot)
			 (:progn
			   ((:textarea :id ,(name edit) :rows ,(getf attrs :rows "3") :style "display:none;"
				       :cols ,(getf attrs :cols "50") :insert-string
				       ,(format nil "onchange='Fch(~s,~a.value);'" (name edit)(name edit))
				       ,@attrs))
			   ((:span :id ,(concatenate 'string (name edit) "d") :style "display:none;")))
			 ((:span :id ,(concatenate 'string (name edit) "d")))))))))

(html:add-func-tag :slot-medit 'slot-medit-tag)

;;; ********* Date Edit ************

(defclass html-date (html-item)
  ((show-time :accessor show-time :initform nil :initarg :show-time)))

(defmethod make-set-value-javascript ((item html-date) value slot)
  (if value
      (multiple-value-bind (s mn h d m y) 
          (if meta::*GMT-time* (decode-universal-time value 0)(decode-universal-time value))
	(let ((j-value (if (show-time item)
			   (format nil "~2,'0d/~2,'0d/~d ~2,'0d:~2,'0d:~2,'0d" d m y h mn s)
			   (format nil "~2,'0d/~2,'0d/~d" d m y))))
	  (concatenate 'string "x_.f826si('" (name item) "', '" j-value "');")))
      (concatenate 'string "x_.f826si('" (name item) "', '');")))

(defmethod make-set-status-javascript ((item html-date) status slot)
  (when (modifiable-p *dispatcher*)
    (if status
	(concatenate 'string
		     "x_.fgt('" (name item) "l').style.visibility='hidden';")
	(concatenate 'string
		     "x_.fgt('" (name item) "l').style.visibility='inherit';"))))

(defun slot-date-edit-tag (attributes form)
  (destructuring-bind (slot-name . attrs) attributes
    (let ((slot (find (symbol-name slot-name) (clos:class-slots *current-class*)
		      :test #'string= :key #'clos:slot-definition-name)))
      (unless slot (error (format nil "Slot inconnu : ~a" slot-name)))
      (let ((edit (make-instance 'html-date :tooltip (meta::tooltip slot) :slot slot
				 :show-time (getf attrs :show-time))))
	(setf attrs (copy-list attrs))
	(remf attrs :show-time)
	`(html:html (:if (modifiable-p ,slot)
			 (:progn
			   ((:span :id ,(concatenate 'string (name edit) "d"))) " &nbsp;"
			   ((:a :id ,(concatenate 'string (name edit) "l")
				:href ,(format nil "javascript:open1('/asp/calendar.html', '250px', '280px', '~a')"
					       (name edit))) (:translate '(:en "calendar" :fr "calendrier" :sp "calendario")))
			   )
			 ((:span :id ,(concatenate 'string (name edit) "d")))))))))

(html:add-func-tag :slot-date-edit 'slot-date-edit-tag)

(defun first-week-day (month year)
  (multiple-value-bind (s m h d m y dw)
      (decode-universal-time 
       (encode-universal-time 1 1 1 1 month year 0) 0)
    dw))

(defun last-day (month year)
  (if (= month 2)
    (if (or (and (= (mod year 4) 0)(/= (mod year 100) 0))(= (mod year 400) 0)) 29 28)
    (aref #(31 28 31 30 31 30 31 31 30 31 30 31) (1- month))))

(defvar *month-fr* '("Janvier" "Février" "Mars" "Avril" "Mai" "Juin"
		     "Juillet" "Août" "Septembre" "Octobre" "Novembre" "Décembre"))

(defparameter *month-names* '(:fr ("Janvier" "Février" "Mars" "Avril" "Mai" "Juin"
                                   "Juillet" "Août" "Septembre" "Octobre" "Novembre" "Décembre")
                              :sp ("Enero" "Febrero" "Marzo" "Abril" "Mayo" "Junio" 
                                   "Julio" "Agosto" "Septiembre" "Octubre"  "Noviembre" "Diciembre")
                              :en ("January" "February" "March" "April" "May" "June"
                                   "July" "August" "September" "October" "November" "Décember")))

(defvar *default-month-names* '("January" "February" "March" "April" "May" "June"
                                "July" "August" "September" "October" "November" "December"))

(defparameter *day-names* '(:fr ("Di" "Lu" "Ma" "Me" "Je" "Ve" "Sa")
                            :sp ("Do" "Lu" "Ma" "Mi" "Ju" "Vi" "Sa")
                            :en ("Su" "Mo" "Tu" "We" "Th" "Fr" "Sa")))

(defvar *default-day-names* '("January" "February" "March" "April" "May" "June"
                                "July" "August" "September" "October" "November" "Décember"))

(defun html-month (item day month year show-time)
  (let ((first-week-day (first-week-day month year))
	(last-day (last-day month year))
	(time (if show-time 
		  " '+document.getElementById('hour').value+':'+document.getElementById('mn').value+':'+document.getElementById('sec').value);"
		  "');")))
    (html:html
     (:jscript "window.focus();function f42(d){if (d == '') window.opener.Fch('" item "','nil');else window.opener.Fch('" item "',d+'/" month "/" year time
	       "window.close();};")
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
                             ((:a :fformat (:href "javascript:f42(~a);" d)) d)))
			  (:when (= (mod col 7) 6) "</tr>")))))))

(defun calendar-request-handler (request)
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
	   "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">"
	   (:html
	    (:head
	     (:title (:translate '(:en "Choose a day" :fr "Choisissez une date" :sp "Elija una fecha")))
	     ((:link :rel "stylesheet" :type "text/css" :href "/cal.css")))
	    (:body
	     :br
	     (:h1 (:translate '(:en "Choose a day" :fr "Choisissez une date" :sp "Elija una fecha")))
	     ((:form :name "go" :method "post" :action "/asp/calendar.html")
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
	       ((:a :class "call" :href "javascript:window.close();")
		(:translate '(:en "Close" :fr "Fermer" :sp "Cerrar"))))
	      ))))))
      t)

(interface::add-named-url "/asp/calendar.html" 'calendar-request-handler)

;;; ***** Combo *************
(defclass html-combo (html-item)
  ())

(defmethod make-set-value-javascript ((item html-combo) value slot)
  (let ((position (position value (meta::choices slot) :key #'first :test #'equal)))
    (unless position (setf position -1))
    (html:fast-format nil "x_.fgt('~a').selectedIndex='~a';" (name item) position)))

(defun slot-combo-tag (attributes form)
  (destructuring-bind (slot-name . attrs) attributes
    (let ((slot (find (symbol-name slot-name) (clos:class-slots *current-class*)
		      :test #'string= :key #'clos:slot-definition-name)))
      (unless slot (error (format nil "Slot inconnu : ~a" slot-name)))
      (let ((combo (make-instance 'html-combo :tooltip (meta::tooltip slot) :slot slot))
	    (choices (loop for (value string) in (meta::choices slot) collect (meta::translate string))))
	`(html:html
	  ((:select :id ,(name combo)
	    :insert-string
	    (if (modifiable-p ,slot)
		,(format nil "onchange=\"Fch('~a',~a.value);\""(name combo)(name combo))
		"disabled='true'")
	    ,@attrs)
	   ,@(loop for choice in choices
		   for i from 0
		   collect (format nil "<option value = ~d>~a" i choice))))))))

(html:add-func-tag :slot-combo 'slot-combo-tag)

;;**** Check Box ***************************

(defclass html-check-box (html-item)
  ())

(defmethod make-set-value-javascript ((item html-check-box) value slot)
  (html:fast-format nil "x_.fgt('~a').checked=~a;" (name item) (if value "true" "false")))

(defun slot-check-box-tag (attributes form)
  (destructuring-bind (slot-name . attrs) attributes
    (let ((slot (find (symbol-name slot-name) (clos:class-slots *current-class*)
		      :test #'string= :key #'clos:slot-definition-name)))
      (unless slot (error (format nil "Slot inconnu : ~a" slot-name)))
      (let ((check-box (make-instance 'html-check-box :tooltip (meta::tooltip slot) :slot slot)))
	`(html:html ((:input :type "checkbox" :id ,(name check-box)
		      :insert-string
		      (if (modifiable-p ,slot)
			  ,(format nil "onclick='Fch(~s,~a.checked);'"
				   (name check-box)(name check-box))
			  "disabled='true'")
		      ,@attrs)))))))

(html:add-func-tag :slot-check-box 'slot-check-box-tag)

;;**** slot-list ***************************

(defclass html-slot-list (html-item)
  ((action-func  :initform nil :accessor action-func :initarg :action-fn)
   (html-fn :accessor html-fn :initform nil :initarg :html-fn)
   (choices-fn :accessor choices-fn :initform nil :initarg :choices-fn)
   (list-format :accessor list-format :initform nil)
   (table-class  :initform ""  :accessor table-class)))

(defclass html-slot-list-dispatcher (slot-dispatcher)
  ((start :accessor start :initform 0)
   (max-nb :accessor max-nb :initform 25)
   (list-format :accessor list-format :initform nil)
   (objects-to-delete :accessor objects-to-delete :initform nil)
   (selected-objects-idx :accessor selected-objects-idx :initform '())
   (sub-classes :accessor sub-classes :initform nil)))

(defmethod make-dispatcher (interface object (item html-slot-list))
  (let ((dispatcher (make-instance 'html-slot-list-dispatcher :interface interface :object object :item item)))
    (setf (list-format dispatcher)
	  (or (list-format item)
	      (find-best-list-format (meta::value-type (slot dispatcher)) *country-language* *user-groups*)))
    dispatcher))

(defmethod make-set-status-javascript ((item html-slot-list) status slot)
  "")

(defun collect-selected-objects-idx (select-string)
  (setf (selected-objects-idx *dispatcher*)
	(when select-string
	  (loop for index from 0
	     for c across select-string
	     when (char= c #\t)
	     collect index))))

(defun collect-list-idx ()
  (let ((start (start *dispatcher*)))
    (mapcar #'(lambda (x) (+ start x)) (selected-objects-idx *dispatcher*))))

(defun collect-list-objects (list)
  (setf list (nthcdr (start *dispatcher*) list))
  (loop for idx in (selected-objects-idx *dispatcher*)
     collect (elt list idx)))

(defmethod slot-list-action-fn (object value click-str)
  (let* ((slot (slot *dispatcher*))
	 (list (funcall (get-value-fn *dispatcher*) object))
	 (list-length (length list)))
    (if (item-state *dispatcher*)
        (let* ((choice (elt (item-state *dispatcher*) (1- (abs value))))
               (list (funcall (get-value-fn *dispatcher*) object)))
          (if (plusp value)
              (funcall (set-value-fn *dispatcher*) (cons choice list) object)
              (funcall (set-value-fn *dispatcher*) (delete choice list) object)))
(progn
    (collect-selected-objects-idx (when (find #\= click-str) (subseq click-str (1+ (position #\= click-str)))))
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
        (if (and (= value -8) *user*) ; copy
            (copy-to-clipboard (clipboard *user*) (collect-list-objects list) object slot)
            (when (modifiable-p *dispatcher*)
              (cond
                ((or (not value) (<= 0 value 10000))
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
                      (html:fast-format nil "x_.open1('/asp/obj-new.html', '250px', '250px', '~a');"
                                        (name (item *dispatcher*))))))
                ((= value -5) ; (not (find 0 (selected-objects-idx *dispatcher*))))
                 (dolist (idx (collect-list-idx))
                   (unless (<= idx 0)
                     (rotatef (elt list (1- idx))(elt list idx))))
                 (map-into (selected-objects-idx *dispatcher*) #'(lambda (x) (1- x)) (selected-objects-idx *dispatcher*))
                 (funcall (set-value-fn *dispatcher*) list object))
                ((= value -6) ; (not (find 0 (selected-objects-idx *dispatcher*))))
                 (dolist (idx (nreverse (collect-list-idx)))
                   (unless (>= (1+ idx)  list-length)
                     (rotatef (elt list (1+ idx))(elt list idx))))
                 (map-into (selected-objects-idx *dispatcher*) #'(lambda (x) (1+ x)) (selected-objects-idx *dispatcher*))
                 (funcall (set-value-fn *dispatcher*) list object))
                ((= value 30000)
                 (dolist (object (objects-to-delete *dispatcher*))
                   (setf list (delete object list :count 1)))
                 (setf (selected-objects-idx *dispatcher*) nil)
                 (funcall (set-value-fn *dispatcher*) list object)
                 (setf (objects-to-delete *dispatcher*) nil))
                ((= value 30001)
                 (setf (objects-to-delete *dispatcher*) nil))
                ((= value -7)
                 (setf (objects-to-delete *dispatcher*)(collect-list-objects list))
                 (send-to-interface
                  (html:fast-format nil "x_.open1('/asp/obj-del.html', '250px', '250px', '~a');"
                                    (name (item *dispatcher*)))))
                ((and (= value -9) *user*) ; cut
                 (copy-to-clipboard (clipboard *user*) (collect-list-objects list) object slot)
                 (setf (objects-to-delete *dispatcher*)(collect-list-objects list))
                 (send-to-interface
                  (html:fast-format nil "x_.open1('/asp/obj-del.html', '250px', '250px', '~a');"
                                    (name (item *dispatcher*)))))
                ((and (= value -10) *user*) ; paste
                 (paste-clipboard (clipboard *user*) object slot))))))))))
  
(defmethod make-set-value-javascript ((item html-slot-list) list slot)
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

;;;css-classes suffixes:
;;; "" =  global
;;; "h" = header-table, "hr" = header row, "h1"..."hxx" = column xx header
;;; "t" = table, "r" = row, "c1"..."cxx" = column xx td style
;;; "b" = buttons 

(defvar *clipboard-view-context* nil)

(defun slot-list-tag (attributes forms)
  (destructuring-bind (slot-name &key (height nil) (width "100%") (class "stdlist") add-fn-only) attributes
    (let ((slot (find (symbol-name slot-name) (clos:class-slots *current-class*)
		      :test #'string= :key #'clos:slot-definition-name)))
      (unless slot (error (format nil "Slot inconnu : ~a" slot-name)))
      (let ((item (make-instance 'html-slot-list :tooltip (meta::tooltip slot) :slot slot
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
			   ((:a :href ,(format nil "javascript:open1('/asp/obj-pick2.html', '250px', '500px', '~a');"
					 (name item)))
			    ,sub-obj-name))))))
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
		   ((:img :border "0" :src "/sl1.jpg" :width "22" :height "18" :title "First Page")))
		  ((:a :href ,(format nil "javascript:Fck('~a', -2);" (name item)(name item)))
		   ((:img :border "0" :src "/sl2.jpg" :width "22" :height "18" :title "Previous Page")))
		  ((:a :href ,(format nil "javascript:Fck('~a', -3);" (name item)(name item)))
		   ((:img :border "0" :src "/sl3.jpg" :width "22" :height "18" :title "Next Page")))
		  ((:a :href ,(format nil "javascript:Fck('~a', -4);" (name item)(name item)))
		   ((:img :border "0" :src "/sl4.jpg" :width "22" :height "18" :title "Last Page")))
		  (:when (modifiable-p ,slot)
                    ((:img :border "0" :src "/v.jpg" :width "22" :height "18" :title ""))
                    ((:a :href ,(format nil "javascript:Fck('~a', f854('~a',-5));" (name item)(name item)))
                     ((:img :border "0" :src "/up.jpg" :width "22" :height "18" :title "Move Up")))
                    ((:a :href ,(format nil "javascript:Fck('~a', f854('~a',-6));" (name item)(name item)))
                     ((:img :border "0" :src "/d.jpg" :width "22" :height "18" :title "Move Down")))
                    ((:a :href ,(format nil "javascript:Fck('~a', f854('~a',-7));" (name item)(name item)))
                     ((:img :border "0" :src "/k.jpg" :width "22" :height "18" :title "Remove"))))
		  ((:a :href ,(format nil "javascript:Fck('~a', f854('~a',-8));" (name item)(name item)))
		   ((:img :border "0" :src "/cp.jpg" :width "22" :height "18" :title "Copy")))
		  (:when (modifiable-p ,slot)
		    ((:a :href ,(format nil "javascript:Fck('~a', f854('~a',-9));" (name item)(name item)))
		     ((:img :border "0" :src "/cu.jpg" :width "22" :height "18" :title "Cut"))))
		  (:when (and (modifiable-p ,slot) *user*)
                    (let ((*clipboard-dest-context-item* ,item))
                      (html:html (:object-view :object (clipboard *user*) :name "clipbd")))))
		 ((:td :align "right")
		  ,@(when (meta::get-object-func slot)
			  `((:when (modifiable-p ,slot)
			      ((:span :align "right")
			       ((:a :href ,(format nil "javascript:open1('/asp/obj-pick2.html', '250px', '500px', '~a')"
					     (name item)))
				,sub-obj-name)))))
		  ,@(when (and (meta::can-create-new-object slot) (not (meta::get-object-func slot)))
			  `((:when (modifiable-p ,slot)
			      ((:span :align "right")
			       ((:a :href ,(format nil "javascript:f825foc('~a');" (name item)))
				,sub-obj-name)))))))))))))))

(defun pick2-request-handler (request)
  (decode-posted-content request)
  (let ((link (cdr (assoc "link" (posted-content request) :test 'string=)))
        (item (cdr (assoc "item" (posted-content request) :test 'string=)))
        (*dispatcher* nil))
    (when link
      (setf link (gethash link *http-links*))
      (when (and link item)
        (let* ((*session* (session link))
               (*user* (user *session*))
               (*country-language* (country-language *session*)))
          (setf *dispatcher* (gethash item (dispatchers link)))
          (with-output-to-request (request)
            (html::html-to-stream
             *request-stream*
             "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">"
             (:html
              (if (html-fn (item *dispatcher*))
                  (funcall (html-fn (item *dispatcher*)) *dispatcher*)
                  (html:html
                   (:head
                    (:title (:translate '(:en "pick an object" :fr "Choisissez un objet" :sp "Elija un objeto")))
                    ((:link :rel "stylesheet" :type "text/css" :href "/cal.css")))
                   (:body
                    :br
                    (:h1 (:translate '(:en "pick an object" :fr "Choisissez un objet"  :sp "Elija un objeto")))
                    (:jscript "window.focus();var shot;function f42(d){if (!shot) {opener.Fad('" item "',d);"
                              "window.setTimeout('window.close();', 600); shot = true;}};")
                    (loop for object in (when *dispatcher*
                                          (funcall (meta::get-object-func (slot *dispatcher*))(object *dispatcher*)))
                       do (html:html "&nbsp;&nbsp;"
                                     ((:a :fformat (:href "javascript:f42('~a');" (encode-object-id object)))
                                      (html:esc (meta::short-description object))) :br))
                    ((:div :align "center")((:a :class "call" :href "javascript:window.close();")
                                            (:translate '(:en "Close" :fr "Fermer" :sp "Cerrar"))))))))))))))
    t)

(interface::add-named-url "/asp/obj-pick2.html" 'pick2-request-handler)

(defun obj-new-request-handler (request)
      (decode-posted-content request)
      (let ((link (cdr (assoc "link" (posted-content request) :test 'string=)))
	    (item (cdr (assoc "item" (posted-content request) :test 'string=)))
	    (dispatcher nil))
	(when link (setf link (gethash link *http-links*)))
	(when (and link item) (setf dispatcher (gethash item (dispatchers link))))
	(let* ((*session* (session link))
	       (*user* (user *session*))
	       (*country-language* (country-language *session*)))
	  (with-output-to-request (request)
	    (html::html-to-stream
	     *request-stream*
	     "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">"
	     (:html
	      (:head
	       (:title (:translate '(:en "Type of object to add" :fr "Type d'objet à ajouter")))
	       ((:link :rel "stylesheet" :type "text/css" :href "/cal.css")))
	      (:body
	       :br
	       (:h1 (:translate '(:en "Type of object" :fr "Type d'objet")))
               (:jscript "window.focus();var shot;function f42(d){if (!shot) {opener.Fck('" item "',d);"
                              "window.setTimeout('window.close();', 600); shot = true;}};")
	       (loop for object in (when dispatcher (sub-classes dispatcher))
		     for i from 1
		     do (html:html "&nbsp;&nbsp;"
				   ((:a :fformat (:href "javascript:f42('~a');" i))
				    (html:esc (meta::translate (meta::user-name object)))) :br))
	       ((:div :align "center")((:a :class "call" :href "javascript:window.close();")
				       (:translate '(:en "Close" :fr "Fermer" :sp "Cerrar")))))))))
	t))
(interface::add-named-url "/asp/obj-new.html" 'obj-new-request-handler)

;(defun ask-yes-no-question (request title question yes-id no-id)

(defun obj-del-request-handler (request)
      (decode-posted-content request)
      (let ((link (cdr (assoc "link" (posted-content request) :test 'string=)))
	    (item (cdr (assoc "item" (posted-content request) :test 'string=)))
	    (dispatcher nil))
	(when link (setf link (gethash link *http-links*)))
	(when (and link item) (setf dispatcher (gethash item (dispatchers link))))
	(let* ((*session* (session link))
	       (*user* (user *session*))
	       (*country-language* (country-language *session*)))
	  (with-output-to-request (request)
	    (html::html-to-stream
	     *request-stream*
	     "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">"
	     (:html
	      (:head
	       (:title (:translate '(:en "Delete" :fr "Suppression" :sp "Eliminar")))
	       ((:link :rel "stylesheet" :type "text/css" :href "/cal.css")))
	      (:body
	       :br
               (:jscript "window.focus();var shot;function f42(d){if (!shot) {opener.Fck('" item "',d);"
                              "window.setTimeout('window.close();', 600); shot = true;}};")
	       (:h1 (:if (> (length (objects-to-delete dispatcher)) 1)
			 (:translate '(:en "Do you want to remove these objects:"
                                       :sp "Está seguro de querer eliminar estos objetos:"
				       :fr "Voulez vous vraiment supprimer ces objets:"))
			 (:translate '(:en "Do you want to remove this object:"
                                       :sp "Está seguro de querer eliminar este objeto:"
				       :fr "Voulez vous vraiment supprimer cet objet:"))))
	       (:p
		(dolist (object (objects-to-delete dispatcher))
		  (html:html "&nbsp;&nbsp;&nbsp;&nbsp;" (html:esc (meta:short-description object)) :br)))
	       ((:div :align "center")
		((:a :class "call" :href "javascript:f42('30000');" )
		 (:translate '(:en "Yes" :fr "Oui" :sp "Si")))
		"&nbsp;&nbsp;&nbsp;&nbsp;"
		((:a :class "call" :href "javascript:f42('30001');" )
		 (:translate '(:en "No" :fr "Non" :sp "No")))
		))))))
	t))

(interface::add-named-url "/asp/obj-del.html" 'obj-del-request-handler)

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
  (destructuring-bind (slot-name . attrs) attributes
    (let ((slot (find (symbol-name slot-name) (clos:class-slots *current-class*)
		      :test #'string= :key #'clos:slot-definition-name)))
      (unless slot (error (format nil "Slot inconnu : ~a" slot-name)))
      (let ((item (make-instance 'html-pick-val :tooltip (meta::tooltip slot) :slot slot
				 :choices-fn (meta::get-object-func slot)
				 :html-fn (or (meta::get-value-html-fn slot) 'std-pick-val-html-fn))))
	`(html:html ((:input :type "text" :id ,(name item) :disabled "true" ,@attrs))
	  (:when (modifiable-p ,slot)
	    ((:a :id ,(action-link item)
		 :href ,(format nil "javascript:open1('/asp/pick-val.html', '250px', '500px', '~a');"
				(name item))) (:translate '(:en "Change" :fr "Changer" :sp "Cambio")))))))))

(html:add-func-tag :slot-pick-val 'slot-pick-val-tag)

(defun pick-request-handler (request)
      (decode-posted-content request)
      (let ((link (cdr (assoc "link" (posted-content request) :test 'string=)))
	    (item (cdr (assoc "item" (posted-content request) :test 'string=)))
	    (*dispatcher* nil))
	(when link
	  (setf link (gethash link *http-links*))
	  (when (and link item)
	    (setf *dispatcher* (gethash item (dispatchers link)))
	    (let* ((*session* (session link))
		   (*user* (user *session*))
		   (*country-language* (country-language *session*)))
	      (with-output-to-request (request)
		(html::html-to-stream
		 *request-stream*
		 "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">"
		 (:html
		  (when *dispatcher*
		    (funcall (html-fn (item *dispatcher*)) *dispatcher*)))))))))
      t)
(interface::add-named-url "/asp/pick-val.html" 'pick-request-handler)

(defun std-pick-val-html-fn (dispatcher)
  (let* ((item (interface::item dispatcher))
	 (item-name (interface::name item))
	 (object (interface::object dispatcher))
	 (slot (interface::slot dispatcher)))
    (html:html
     (:head
      (:title (:translate (meta::get-value-title slot) :default '(:en "Choose a value" :fr "Choisissez une valeur"  :sp "Elija un valor")))
      ((:link :rel "stylesheet" :type "text/css" :href "/cal.css")))
     (:body
      :br
      (:h1 (:translate (meta::get-value-title slot) :default '(:en "Choose a value" :fr "Choisissez une valeur" :sp "Elija un valor")))
      (:jscript "window.focus();function f42(d){window.opener.Fch('" item-name "',d);"
		"window.close();};")
      (:p (:translate (meta::get-value-text slot)))
      (when dispatcher
	(when (meta::null-allowed (slot dispatcher))
	  (html:html "&nbsp;&nbsp;"
		     ((:a :href "javascript:f42('nil');") (:translate '(:en "None of these choices" 
                                                                        :fr "Aucun de ces choix" 
                                                                        :sp "Ninguna de estas opciones"))) :br :br))
	(loop for (text value) in (funcall (choices-fn dispatcher)(object dispatcher))
	      do (html:html "&nbsp;&nbsp;"
			    ((:a :fformat (:href "javascript:f42('~a');" (if (stringp value)
									     (html:quote-javascript-string value)
									     value)))
			     (html:esc text)) :br)))
      ((:div :align "center")((:a :class "call" :href "javascript:window.close();")
			      (:translate '(:en "Close" :fr "Fermer" :sp "Cerrar"))))))))

(defun std-pick-treeview-html-fn (dispatcher)
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
       (:head
	(:title (:translate (meta::get-value-title slot) :default '(:en "Choose a value" :fr "Choisissez une valeur" :sp "Elija un valor")))
	((:link :rel "stylesheet" :type "text/css" :href "/cal.css")))
       (:style "
.d1  {overflow:visible; height:16px; font-family: Arial, Helvetica, sans-serif; font-size: 10pt;}
.d2  {overflow:visible; height:16px; font-family: Arial, Helvetica, sans-serif; font-size: 10pt;}
.ic  {height:16px; width:16px; border:0;}
.sp  {position:relative;}
")
       (:jscript
	"function fs(name)
{
  var item;
  item=document.getElementById(name);
  if (item)
     item.style.display = 'inline';
}
    
function fh(name)
{
  var item;
  item=document.getElementById(name);
  if (item)
     item.style.display = 'none';
}
")
       (:body
	:br
	(:h1 (:translate (meta::get-value-title slot) :default '(:en "Choose a value" :fr "Choisissez une valeur" :sp "Elija un valor")))
        (:if (typep dispatcher 'html-slot-list-dispatcher)
             (:jscript "window.focus();var shot;function f42(d){if (!shot) {opener.Fad('" item-name "',d);"
                       "window.setTimeout('window.close();', 600); shot = true;}};")
             (:jscript "window.focus();var shot;function f42(d){if (!shot) {opener.Fch('" item-name "',d);"
                       "window.setTimeout('window.close();', 600); shot = true;}};"))
	(:p (:translate (meta::get-value-text slot)))
	(when dispatcher
	  (draw-simple-tree (funcall (meta::get-object-func (slot dispatcher)) object) 0 t (not null-allowed) ()
			    :draw-node-fn #'draw-item :opened-node t)
	  (when null-allowed
	    (draw-simple-tree `((,(meta::translate '(:en "None of these choices" :fr "Aucun de ces choix" 
                                                                                 :sp "Ninguna de estas opciones")) "")) 0  nil t ()
			      :draw-node-fn #'draw-item)))
	:br :br
	((:div :align "center")
         ((:a :class "call" :href "javascript:window.close();")
          (:translate '(:en "Close" :fr "Fermer" :sp "Cerrar")))))))))

(defun std-fn-pick-treeview-html-fn (dispatcher)
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
	   (fc-fn (fc-function (item dispatcher))))
      (html:html
       (:head
	(:title (:translate (meta::get-value-title fc-fn) :default '(:en "Choose a value" :fr "Choisissez une valeur" :sp "Elija un valor")))
	((:link :rel "stylesheet" :type "text/css" :href "/cal.css")))
       (:style "
.d1  {overflow:visible; height:16px; font-family: Arial, Helvetica, sans-serif; font-size: 10pt;}
.d2  {overflow:visible; height:16px; font-family: Arial, Helvetica, sans-serif; font-size: 10pt;}
.ic  {height:16px; width:16px; border:0;}
.sp  {position:relative;}
")
       (:jscript
	"function fs(name)
{
  var item;
  item=document.getElementById(name);
  if (item)
     item.style.display = 'inline';
}
    
function fh(name)
{
  var item;
  item=document.getElementById(name);
  if (item)
     item.style.display = 'none';
}
")
       (:body
	:br
	(:h1 (:translate (meta::get-value-title fc-fn) :default '(:en "Choose a value" :fr "Choisissez une valeur" :sp "Elija un valor")))
        (:jscript "window.focus();var shot;function f42(d){if (!shot) {opener.Fch('" item-name "',d);"
                  "window.setTimeout('window.close();', 600); shot = true;}};")
	(:p (:translate (meta::get-value-text fc-fn)))
	(when dispatcher
	  (draw-simple-tree (funcall (meta::get-object-func fc-fn) object) 0 t t ()
			    :draw-node-fn #'draw-item :opened-node t))
	:br :br
	((:div :align "center")((:a :class "call" :href "javascript:window.close();")
				(:translate '(:en "Close" :fr "Fermer" :sp "Cerrar")))))))))

(defun std-pick-huge-treeview-html-fn (dispatcher)
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
	   (index (cdr (assoc "index" (posted-content *request*) :test 'string=)))
	   (level (cdr (assoc "level" (posted-content *request*) :test 'string=)))
	   (action (cdr (assoc "action" (posted-content *request*) :test 'string=)))
	   (path (item-state2 dispatcher))
	   (null-allowed (meta::null-allowed (slot dispatcher))))
      (setf index (ignore-errors (parse-integer index :junk-allowed t)))
      (setf level (ignore-errors (parse-integer level :junk-allowed t)))
      (when (and level index)
	(when (>= (length path) level)
	  (setf path (subseq path 0 level)))
	(if (string= action "o")
	  (setf path (nconc path (list index)))))
      (setf path (or path '(0)))
      (setf (item-state2 dispatcher) path)
      (html:html
       (:head
	(:title (:translate (meta::get-value-title slot) :default '(:en "Choose a value" :fr "Choisissez une valeur" :sp "Elija un valor")))
	((:link :rel "stylesheet" :type "text/css" :href "/cal.css")))
       (:style "
.d1  {overflow:visible; height:16px; font-family: Arial, Helvetica, sans-serif; font-size: 10pt;}
.d2  {overflow:visible; height:16px; font-family: Arial, Helvetica, sans-serif; font-size: 10pt;}
.ic  {height:16px; width:16px; border:0;}
.sp  {position:absolute;}
")
       (:jscript
	"function fs(name)
{
  var item;
  item=document.getElementById(name);
  if (item)
     item.style.display = 'inline';
}
    
function fh(name)
{
  var item;
  item=document.getElementById(name);
  if (item)
     item.style.display = 'none';
}
")
       (:body
	:br
	(:h1 (:translate (meta::get-value-title slot) :default '(:en "Choose a value" :fr "Choisissez une valeur" :sp "Elija un valor")))
	((:form :name "go" :method "post" :action "/asp/pick-val.html")
	 ((:input :id "item" :name "item" :type "hidden" :value item-name))
	 ((:input :id "link" :name "link" :type "hidden" :value (interface-id (interface dispatcher))))
	 ((:input :id "index" :name "index" :type "hidden"))
	 ((:input :id "level" :name "level" :type "hidden"))
	 ((:input :id "action":name "action" :type "hidden"))
	 (:jscript "window.focus();function f42(d){window.opener.Fch('" item-name "',d);"
		   "window.close();};"
		   "function f43(l,i,a){fgt('index').value=i;fgt('level').value=l;fgt('action').value=a;"
		   "document.forms['go'].submit();};")

	 (:p (:translate (meta::get-value-text slot)))
	 (when dispatcher
	   (draw-huge-tree (funcall (choices-fn dispatcher) object) 0 0 t (not null-allowed)
			   () path #'draw-item)
	   (when null-allowed
	     (draw-simple-tree `((,(meta::translate '(:en "None of these choices" :fr "Aucun de ces choix"
                                                                                  :sp "Ninguna de estas opciones")) "")) 0  nil t ()
			       :draw-node-fn #'draw-item)
	     #+ignore
	     (html:html "&nbsp;&nbsp;"
			((:a :href "javascript:f42('nil');") (:translate '(:en "None of these choices" :fr "Aucun de ces choix"
                                                                                                       :sp "Ninguna de estas opciones"))))))
	 :br :br
	 ((:div :align "center")((:a :class "call" :href "javascript:window.close();")
				 (:translate '(:en "Close" :fr "Fermer" :sp "Cerrar"))))))))))

;;; slot-obj-link

(defclass html-obj-link (html-pick-val)
  ())

(defmethod make-set-value-javascript ((item html-obj-link) value slot)
  (html:fast-format nil "x_.fgt('~a').href='~a';x_.fgt('~a').innerHTML='~a';"
		    (name item)
		    (if (meta::fc-object-p value) (encode-object-url value) "javascript:void(0)")
		    (name item)
		    (html:quote-javascript-string
		     (if value
		       (meta::short-description value)
		       (meta::translated-void-link-text slot)))))

(defun slot-obj-link-tag (attributes form)
  (destructuring-bind (slot-name . attrs) attributes
    (let ((slot (find (symbol-name slot-name) (clos:class-slots *current-class*)
		      :test #'string= :key #'clos:slot-definition-name)))
      (unless slot (error (format nil "Slot inconnu : ~a" slot-name)))
      (let ((obj-link (make-instance 'html-obj-link :tooltip (meta::tooltip slot) :slot slot
				     :choices-fn (meta::get-object-func slot)
				     :html-fn (or (meta::get-value-html-fn slot) 'std-pick-obj-html-fn))))
	`(html:html ((:a :href "" :id ,(name obj-link) ,@attrs)) 
	  ,@(when (action-link obj-link)
		  `((:when (modifiable-p ,slot) ;"&nbsp; "
		      #+nil((:a :id ,(action-link obj-link)
			   :href ,(format nil "javascript:open1('/asp/pick-val.html', '250px', '500px', '~a')"
					  (name obj-link))) (:translate '(:en "Change" :fr "Changer")))
                      ((:a :id ,(action-link obj-link)
			   :href ,(format nil "javascript:open1('/asp/pick-val.html', '250px', '500px', '~a')"
					  (name obj-link)))
                       ((:img :border "0" :src "/ch.png" :width "16" :height "16" :align "middle" :title "Change")))))))))))

(html:add-func-tag :slot-obj-link 'slot-obj-link-tag)

(defun std-pick-obj-html-fn (dispatcher)
  (let* ((item (item dispatcher))
	 (item-name (name item))
	 (object (object dispatcher))
	 (slot (slot dispatcher)))
    (html:html
     (:head
      (:title (:translate (meta::get-value-title slot) :default '(:en "Choose an object" :fr "Choisissez un objet" :sp "Elija un objeto")))
      ((:link :rel "stylesheet" :type "text/css" :href "/cal.css")))
     (:body
      :br
      (:h1 (:translate (meta::get-value-title slot) :default '(:en "Choose an object" :fr "Choisissez un objet" :sp "Elija un objeto")))
      (:jscript "window.focus();function f42(d){window.opener.Fch('" item-name "',d);"
		"window.close();};")
      (:p (:translate (meta::get-value-text slot)))
      (when dispatcher
	(when (meta::null-allowed (slot dispatcher))
	  (html:html "&nbsp;&nbsp;"
		     ((:a :href "javascript:f42('nil');") (:translate '(:en "None of these choices" :fr "Aucun de ces choix"
                                                                            :sp "Ninguna de estas opciones"))) :br :br))
	(loop for object in (funcall (meta::get-object-func (slot dispatcher))(object dispatcher))
	      do (html:html "&nbsp;&nbsp;"
			    ((:a :fformat (:href "javascript:f42('~a');" (encode-object-id object)))
			     (html:esc (meta::short-description object))) :br)))
      ((:div :align "center")((:a :class "call" :href "javascript:window.close();")
			      (:translate '(:en "Close" :fr "Fermer" :sp "Cerrar"))))))))


;;; pick-color

(defclass html-pick-color (html-pick-val)
  ())

(defmethod make-set-value-javascript ((item html-pick-color) value slot)
  (setf value (if value (html:quote-javascript-string value) ""))
  (html:fast-format nil "x_.fgt('~a').style.backgroundColor='~a';x_.fgt('~a').value='~a';"
		    (name item) value (name item) value))

(defun slot-pick-color-tag (attributes form)
  (destructuring-bind (slot-name . attrs) attributes
    (let ((slot (find (symbol-name slot-name) (clos:class-slots *current-class*)
		      :test #'string= :key #'clos:slot-definition-name)))
      (unless slot (error (format nil "Slot inconnu : ~a" slot-name)))
      (let ((item (make-instance 'html-pick-color :tooltip (meta::tooltip slot) :slot slot
				 :choices-fn t
				 :html-fn (or (meta::get-object-func slot) 'std-pick-color-html-fn))))
	`(html:html ((:input :type "text" :id ,(name item) :readonly "true" ,@attrs))
	  (:when (modifiable-p ,slot)
	    ((:a :id ,(action-link item)
		 :href ,(format nil "javascript:open1('/asp/pick-val.html', '400px', '500px', '~a')"
				(name item))) 
             ((:img :border "0" :src "/ch.png" :width "16" :height "16" :align "baseline" :title "Change")))))))))

(html:add-func-tag :slot-pick-color 'slot-pick-color-tag)

(defun luminance (r g b)
  (+ (* 0.3 r)(* 0.59 g)(* 0.11 b)))

(defun rgb-to-hsv (r g b)
  (let* ((min (min r g b))
	 (max (max r g b))
	 (delta (- max min)))
    (values (if (zerop delta) -1
		(float (cond
			 ((= r max)(/ (- g b) delta))
			 ((= g max)(+ 2 (/ (- b r) delta)))
			 (t(+ 4 (/ (- r g) delta))))))
	    (if (zerop max) 0 (float (/ delta max)))
	    max)))

(defun std-pick-color-html-fn (dispatcher)
  (flet ((color-td (r g b)
	   (let ((color (format nil "#~2,'0x~2,'0x~2,'0x" r g b)))
	     (html:html ((:td :bgcolor color :fformat (:onclick "f42('~a');" color))"&nbsp;&nbsp;&nbsp;")))))
    (let* ((colors nil)
	   (item (interface::item dispatcher))
	   (item-name (interface::name item)))
      (dotimes (r 6)
	(dotimes (g 6)
	  (dotimes (b 6)
	    (push (list (* r 51) (* g 51) (* b 51)(luminance r g b)) colors))))
      (setf colors (sort colors #'> :key 'fourth))
      (html:html
       (:head
	(:title (:translate '(:en "Choose a color" :fr "Choisissez une couleur")))
	((:link :rel "stylesheet" :type "text/css" :href "/pcol.css")))
       (:body
	:br
	(:h1 (:translate '(:en "Choose a color" :fr "Choisissez une couleur")))
        (:jscript "window.focus();var shot;function f42(d){if (!shot) {opener.Fch('" item-name "',d);"
                  "window.setTimeout('window.close();', 600); shot = true;}};")
#+nil        (:jscript "window.focus();function f42(d){window.opener.Fch('" item-name "',d);"
		  "window.close();};")
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
		 (loop for (r g b l) in row
		       do (color-td r g b))))))
	:br
	((:div :align "center")((:a :class "call" :href "javascript:window.close();")
				(:translate '(:en "Close" :fr "Fermer")))))))))

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

(defun slot-pick-multi-val-tag (attributes form)
  (destructuring-bind (slot-name . attrs) attributes
    (let ((slot (find (symbol-name slot-name) (clos:class-slots *current-class*)
		      :test #'string= :key #'clos:slot-definition-name))
	  (vertical (getf attrs :vertical)))
      (unless slot (error (format nil "Slot inconnu : ~a" slot-name)))
      (let ((item (make-instance 'html-pick-multi-val :tooltip (meta::tooltip slot) :slot slot
				 :choices-fn (or (meta::get-object-func slot) 'std-get-mval-choices)
				 :action-fn 'html-pick-multi-val-action-fn
				 :html-fn (or (meta::get-value-html-fn slot) 'std-pick-multi-val-html-fn))))
      (setf attrs (copy-list attrs))
      (remf attrs :vertical)
	(if vertical
	    `(html:html
	      ((:table :width "100%")
	       (:tr
		(:td ((:span :id ,(name item) ,@attrs))))
	       (:when (modifiable-p ,slot)
		 (:tr
		  ((:td :align "right" :valign "top")
		   ((:a :id ,(action-link item)
			:href ,(format nil "javascript:open1('/asp/pick-val.html', '300px', '500px', '~a')"
				       (name item))) 
                    ((:img :border "0" :src "/ch.png" :width "16" :height "16" :align "top" :title "Change"))))))))
	    `(html:html
              ((:span :id ,(name item) ,@attrs))
              (:when (modifiable-p ,slot)
                ((:a :id ,(action-link item)
                     :href ,(format nil "javascript:open1('/asp/pick-val.html', '300px', '500px', '~a')"
                                    (name item)))
                 ((:img :border "0" :src "/ch.png" :width "16" :height "16" :align "top" :title "Change"))))))))))

(html:add-func-tag :slot-pick-mval 'slot-pick-multi-val-tag)

(defun std-get-mval-choices (object)
  (let* ((choices (meta::choices (interface::slot interface::*dispatcher*)))
	 )
    (loop for (value tr-string) in (meta::choices (interface::slot interface::*dispatcher*))
	  for text = (meta::translate tr-string)
	  collect (list text value))))

(defun std-pick-multi-val-html-fn (dispatcher)
  (let* ((item (interface::item dispatcher))
	 (item-name (interface::name item))
	 (object (interface::object dispatcher))
	 (slot (interface::slot dispatcher))
	 (slot-value (funcall (get-value-fn dispatcher) object))
	 (choices ()))
    (html:html
     (:head
      (:title (:translate (meta::get-value-title slot) :default '(:en "Choose values" :fr "Choisissez des valeurs")))
      ((:link :rel "stylesheet" :type "text/css" :href "/cal.css")))
     (:body
      :br
      (:h1 (:translate (meta::get-value-title slot) :default '(:en "Choose values" :fr "Choisissez des valeurs")))
      (:jscript "window.focus();function f42(i,st){if (st) window.opener.Fck('" item-name "',i);"
		"else window.opener.Fck('" item-name "',-i);"
		"};")
      (:p (:translate (meta::get-value-text slot)))
      (when dispatcher
	(loop for (text value) in (funcall (choices-fn dispatcher) object)
	      for i from 0
	      do (html:html "&nbsp;&nbsp;"
			    ((:input :type "checkbox" :fformat (:id "CB~d" i)
				     :fformat (:onclick "f42(~d,CB~a.checked);" (1+ i) i)
				     :insert-string (if (member value slot-value :test #'equal) "CHECKED" "")
                                     #+nil(if (and value (member (decode-object-id value) slot-value :test #'equal)) "CHECKED" "")
				     ))
			    (html:esc text) :br)
	      (push value choices))
	(setf (item-state dispatcher) (nreverse choices)))
      ((:div :align "center")((:a :class "call" :href "javascript:window.close();")
			      (:translate '(:en "Close" :fr "Fermer"))))))))

(defun std-mpick-treeview-html-fn (dispatcher)
  (let* ((item (interface::item dispatcher))
         (item-name (interface::name item))
         (object (interface::object dispatcher))
         (slot (interface::slot dispatcher))
         (slot-value (funcall (get-value-fn dispatcher) object))
         (choices ())
         (i 0))
    (flet ((draw-item2 (node)
             (let* ((name (first node))
		    (text (first name))
		    (value (second name)))
	       (if value
		   (html:html "&nbsp;&nbsp;"
			      ((:input :type "checkbox" :fformat (:id "CB~d" i)
				       :fformat (:onclick "f42(~d,CB~a.checked);" (1+ i) i)
				       :insert-string (if (member value slot-value :test #'equal) "CHECKED" "")))
			      (incf i)
			      (push value choices)
			      (html:esc text))
                   (html:html "&nbsp;&nbsp;"
                              (html:esc text))))))
      (html:html
       (:head
	(:title (:translate (meta::get-value-title slot) :default '(:en "Choose a value" :fr "Choisissez une valeur")))
	((:link :rel "stylesheet" :type "text/css" :href "/cal.css")))
       (:style "
.d1  {overflow:visible; height:16px; font-family: Arial, Helvetica, sans-serif; font-size: 10pt;}
.d2  {overflow:visible; height:16px; font-family: Arial, Helvetica, sans-serif; font-size: 10pt;}
.ic  {height:16px; width:16px; border:0;}
.sp  {position:relative;}
")
       (:jscript
	"function fs(name)
{
  var item;
  item=document.getElementById(name);
  if (item)
     item.style.display = 'inline';
}
    
function fh(name)
{
  var item;
  item=document.getElementById(name);
  if (item)
     item.style.display = 'none';
}
")
       (:body
	:br
	(:h1 (:translate (meta::get-value-title slot) :default '(:en "Choose a value" :fr "Choisissez une valeur")))
        (:if nil ;(typep dispatcher 'html-slot-list-dispatcher)
             (:jscript "window.focus();var shot;function f42(d){if (!shot) {opener.Fad('" item-name "',d);"
                       "window.setTimeout('window.close();', 600); shot = true;}};")
             (:jscript "window.focus();function f42(i,st){if (st) window.opener.Fck('" item-name "',i);"
		"else window.opener.Fck('" item-name "',-i);"
		"};"))
	(:p (:translate (meta::get-value-text slot)))
	(when dispatcher
	  (draw-simple-tree (funcall (meta::get-object-func (slot dispatcher)) object) 0 t t ()
			    :draw-node-fn #'draw-item2)
	  (setf (item-state dispatcher) (nreverse choices)))
	:br :br
	((:div :align "center")
#+nil
         ((:a :class "call" :href "javascript:window.close();")
          (:translate '(:en "Remove all" :fr "Tout enlever")))
         ((:a :class "call" :href "javascript:window.close();")
          (:translate '(:en "Close" :fr "Fermer"))))))))
#+nil
  (let* ((item (interface::item dispatcher))
	 (item-name (interface::name item))
	 (object (interface::object dispatcher))
	 (slot (interface::slot dispatcher))
	 (slot-value (funcall (get-value-fn dispatcher) object))
	 (choices ())
	 (i 0))
    (flet ((draw-item2 (node)
	     (let* ((name (first node))
		    (text (first name))
		    (value (second name)))
	       (if value
		   (html:html "&nbsp;&nbsp;"
			      ((:input :type "checkbox" :fformat (:id "CB~d" i)
				       :fformat (:onclick "f42(~d,CB~a.checked);" (1+ i) i)
				       :insert-string (if (member value slot-value :test #'equal) "CHECKED" "")))
			      (incf i)
			      (push value choices)
			      (html:esc text)))
	       (html:html "&nbsp;&nbsp;"
			  (html:esc text)))))
      (html:html
       (:head
	(:title (:translate (meta::get-value-title slot) :default '(:en "Choose a value" :fr "Choisissez une valeur")))
	((:link :rel "stylesheet" :type "text/css" :href "/cal.css")))
       (:style "
.d1  {overflow:visible; height:16px; font-family: Arial, Helvetica, sans-serif; font-size: 10pt;}
.d2  {overflow:visible; height:16px; font-family: Arial, Helvetica, sans-serif; font-size: 10pt;}
.ic  {height:16px; width:16px; border:0;}
.sp  {position:absolute;}
")
       (:jscript
	"function fs(name)
{
  var item;
  item=document.getElementById(name);
  if (item)
     item.style.display = 'inline';
}
    
function fh(name)
{
  var item;
  item=document.getElementById(name);
  if (item)
     item.style.display = 'none';
}
")
       (:body
	:br
	(:h1 (:translate (meta::get-value-title slot) :default '(:en "Choose a value" :fr "Choisissez une valeur")))
	(:jscript "window.focus();function f42(i,st){if (st) window.opener.Fck('" item-name "',i);"
		"else window.opener.Fck('" item-name "',-i);"
		"};")
	(:p (:translate (meta::get-value-text slot)))
	(when dispatcher
	  (draw-simple-tree (funcall (choices-fn dispatcher) object) 0 t t ()
			    :draw-node-fn #'draw-item2))
	(setf (item-state dispatcher) (nreverse choices))
	:br :br
	((:div :align "center")((:a :class "call" :href "javascript:window.close();")
				(:translate '(:en "Close" :fr "Fermer")))))))))

(defun std-mpick-huge-treeview-html-fn (dispatcher)
  (let* ((item (interface::item dispatcher))
	 (item-name (interface::name item))
	 (object (interface::object dispatcher))
	 (slot (interface::slot dispatcher))
	 (slot-value (funcall (get-value-fn dispatcher) object))
	 (index (cdr (assoc "index" (posted-content *request*) :test 'string=)))
	 (level (cdr (assoc "level" (posted-content *request*) :test 'string=)))
	 (action (cdr (assoc "action" (posted-content *request*) :test 'string=)))
	 (path (item-state2 dispatcher))
	 (choices ())
	 (i 0))
    (setf index (ignore-errors (parse-integer index :junk-allowed t)))
    (setf level (ignore-errors (parse-integer level :junk-allowed t)))
    (when (and level index)
      (when (>= (length path) level)
	(setf path (subseq path 0 level)))
      (if (string= action "o")
	  (setf path (nconc path (list index)))))
    (setf path (or path '(0)))
    (setf (item-state2 dispatcher) path)
    (flet ((draw-item2 (node)
	     (let* ((name (first node))
		    (text (first name))
		    (value (second name)))
	       (if value
		   (html:html "&nbsp;&nbsp;"
			      ((:input :type "checkbox" :fformat (:id "CB~d" i)
				       :fformat (:onclick "f42(~d,CB~a.checked);" (1+ i) i)
				       :insert-string (if (member value slot-value :test #'equal) "CHECKED" "")))
			      (incf i)
			      (push value choices)
			      (html:esc text)))
	       (html:html "&nbsp;&nbsp;"
			  (html:esc text)))))
      (html:html
       (:head
	(:title (:translate (meta::get-value-title slot) :default '(:en "Choose a value" :fr "Choisissez une valeur")))
	((:link :rel "stylesheet" :type "text/css" :href "/cal.css")))
       (:style "
.d1  {overflow:visible; height:16px; font-family: Arial, Helvetica, sans-serif; font-size: 10pt;}
.ic  {height:16px; width:16px; border:0;}
")
       (:body
	:br
	(:h1 (:translate (meta::get-value-title slot) :default '(:en "Choose a value" :fr "Choisissez une valeur")))

	((:form :name "go" :method "post" :action "/asp/pick-val.html")
	 ((:input :name "item" :type "hidden" :value item-name))
	 ((:input :name "link" :type "hidden" :value (interface-id (interface dispatcher))))
	 ((:input :id "index" :name "index" :type "hidden"))
	 ((:input :id "level" :name "level" :type "hidden"))
	 ((:input :id "action" :name "action" :type "hidden"))
	 (:jscript "window.focus();function f42(i,st){if (st) window.opener.Fck('" item-name "',i);"
		   "else window.opener.Fck('" item-name "',-i);"
		   "};"
		   "function f43(l,i,a){fgt('index').value=i;fgt('level').value=l;fgt('action').value=a;"
		   "document.forms['go'].submit();};")
	 (:p (:translate (meta::get-value-text slot)))
	 (when dispatcher
	   (draw-huge-tree (funcall (choices-fn dispatcher) object) 0 0 t t () path #'draw-item2))
	(setf (item-state dispatcher) (nreverse choices))
	:br :br
	((:div :align "center")((:a :class "call" :href "javascript:window.close();")
				(:translate '(:en "Close" :fr "Fermer"))))))))))

(defun test-mpick (object)
  '(("LTD_DIRECT" "LTD_DIRECT")
    ("TEL_PABX" "TEL_PABX")
    ("SOGEDIN" "SOGEDIN")
    ("TEL_DIRECT" "TEL_DIRECT")
    ("SGLINK" "SGLINK")
    ("SGPAC" "SGPAC")
    ("TELEX" "TELEX")
    ("SOGETRADE" "SOGETRADE")
    ("INTERNET" "INTERNET")))

(defun make-tree (n base)
  (cons (list (format nil "~d" base) base)
	(when (>= n 1)
	  (loop for i from 0 to 9
		for val = (+ i base)
		collect (make-tree (1- n) (* val 10))))))

(defparameter *tree* (make-tree 2 0))

(defmethod leaf-node-p ((node list))
  (not (cdr node)))

(defmethod non-leaf-node-p ((node list))
  (cdr node))

(defmethod map-sub-nodes (fn (node list))
  (loop for (sub-node . rest) on (cdr node)
	for first = t then nil
	for i from 0
	do (funcall fn sub-node first (not rest) i)))

(defmethod draw-node ((node list))
  (html:html
   ((:input :type "checkbox" :style "height:16px;border:0px;"))
   (html::ffmt "~a" (caar node))))

(defun draw-prev-lines1 (previous-lasts)
  (loop for pl in previous-lasts do
	    (if pl
		(html:html ((:img :src "empty.gif" :class "ic" :align "top")))
		(html:html ((:img :src "line1.gif" :class "ic" :align "top"))))))

(defvar *div-id* 0)

(defun draw-simple-tree1 (node level path first last previous-lasts &optional (draw-node-fn #'draw-node))
  (let* ((non-leaf-node (non-leaf-node-p node))
	 (div-c (format nil "D~d" (incf *div-id*)))
	 (div-o (format nil "D~d" (incf *div-id*))))
    (setf first (and first (zerop level)))
    (if non-leaf-node
	(html:html
	 ((:div :class "d1" :id div-c :style "display:'';")
	  (draw-prev-lines1 previous-lasts)
	  ((:img :src
		 (if last
		     (if first "plus1.gif" "plus2.gif")
		     (if first "plus4.gif" "plus3.gif"))
		 :class "ic" :align "top" :fformat (:onclick "f825s('~a');f825h('~a');" div-o div-c)))
	  ((:img :src "folderClosed.gif" :class "ic" :align "top"))
	  "&nbsp;"
	  (funcall draw-node-fn node))
	 ((:div :class "d1" :id div-o :style "display:none;")
	   (draw-prev-lines1 previous-lasts)
	  ((:img :src
		 (if last
		     (if first "minus1.gif" "minus2.gif")
		     (if first "minus4.gif" "minus3.gif"))
		 :class "ic" :align "top" :fformat (:onclick "f825s('~a');f825h('~a');" div-c div-o)))
	  ((:img :src "folderOpen.gif" :class "ic" :align "top"))
	  "&nbsp;"
	  (funcall draw-node-fn node)
	  (when open-node
	    (incf level)
	    (setf previous-lasts (append previous-lasts (list last)))
	    (map-sub-nodes #'(lambda (node first last index)
			       (draw-simple-tree1 node level path first last previous-lasts))
			   node))))
	(html:html
	 ((:div :class "d1")
	  (draw-prev-lines1 previous-lasts)
	  ((:img :src
		 (if last
		     (if first "line1.gif" "line2.gif")
		     "line3.gif")
		 :class "ic" :align "top"))
	  ((:img :src "leaf.gif" :class "ic" :align "top"))
	  "&nbsp;"
	  (funcall draw-node-fn node))))))

(defun draw-prev-lines2 (previous-lasts)
  (loop for pl in previous-lasts
	for pos from 0 by 16 do
	(if pl
	    (html:html ((:img :src "/ey.gif" :class "ic" :align "top"
			      :fformat (:style "position:absolute;left:~dpx;" pos))))
	    (html:html ((:img :src "/l1.gif" :class "ic" :align "top"
			      :fformat (:style "position:absolute;left:~dpx;" pos)))))))

(defun draw-simple-tree (node level first last previous-lasts &key (draw-node-fn #'draw-node) opened-node)
  (let* ((non-leaf-node (non-leaf-node-p node))
	 (span-c (format nil "D~d" (incf *div-id*)))
	 (span-o (format nil "D~d" (incf *div-id*)))
	 (div-o (format nil "D~d" (incf *div-id*)))
	 (pos (* level 16))
	 (display (if opened-node "display:inline;" "display:none;")))
    (setf first (and first (zerop level)))
    (if non-leaf-node
	(html:html
	 ((:div :class "d1")
           (:nobr
	  (draw-prev-lines2 previous-lasts)
	  ((:span :id span-c :class "sp" :fformat (:style "left:~dpx;" pos))
	   ((:img :src
		  (if last
		      (if first "/p1.gif" "/p2.gif")
		      (if first "/p4.gif" "/p3.gif"))
		  :class "ic" :align "top" :fformat (:onclick "fs('~a');fs('~a');fh('~a');"
							      div-o span-o span-c)))
	   ((:img :src "/fc.gif" :class "ic" :align "top")))
	  ((:span :class "sp" :id span-o :fformat (:style "display:none;left:~dpx;" pos))
	   ((:img :src
		  (if last
		      (if first "/m1.gif" "/m2.gif")
		      (if first "/m4.gif" "/m3.gif"))
		  :class "ic" :align "top" :fformat (:onclick "fh('~a');fh('~a');fs('~a');"
							      div-o span-o span-c)))
	   ((:img :src "/fo.gif" :class "ic" :align "top")))
	  ((:span  :class "sp" :fformat (:style "left:~dpx;" pos))
            "&nbsp;"
            (funcall draw-node-fn node))))
	  ((:div :class "d1" :id div-o :style display)
	   (incf level)
	   (setf previous-lasts (append previous-lasts (list last)))
	   (map-sub-nodes #'(lambda (node first last index)
			      (draw-simple-tree node level first last previous-lasts :draw-node-fn draw-node-fn))
			  node)))
	(html:html
	 ((:div :class "d1")
	  (draw-prev-lines2 previous-lasts)
	  ((:span :class "sp" :fformat (:style "left:~dpx;" pos))
           (:nobr
            ((:img :src
                   (if last
                       (if first "/l1.gif" "/l2.gif")
                       "/l3.gif")
                   :class "ic" :align "top"))
            ((:img :src "/lf.gif" :class "ic" :align "top"))
            "&nbsp;"
            (funcall draw-node-fn node))))))))

(defun draw-prev-lines (previous-lasts)
  (loop for pl in previous-lasts do
	(if pl
	    (html:html ((:img :src "/ey.gif" :class "ic" :align "top")))
	    (html:html ((:img :src "/l1.gif" :class "ic" :align "top"))))))

(defun draw-huge-tree (node level index first last previous-lasts path &optional (draw-node-fn #'draw-node))
  (let* ((non-leaf-node (non-leaf-node-p node))
	 (opened (and non-leaf-node (eql index (pop path)))))
    (setf first (and first (zerop level)))
    (html:html
     ((:div :class "d1")
      (draw-prev-lines previous-lasts)
      ((:img :src
	     (if non-leaf-node
		 (if opened
		     (if last
			 (if first "/m1.gif" "/m2.gif")
			 (if first "/m4.gif" "/m3.gif"))
		     (if last
			 (if first "/p1.gif" "/p2.gif")
			 (if first "/p4.gif" "/p3.gif")))
		 (if last
		     (if first "/l1.gif" "/l2.gif")
		     "/l3.gif"))
	     :class "ic" :align "top" :fformat (:onclick "f43(~d,~d,'~a');"
							 level index (if opened "c" "o"))))
       ((:img :src (if non-leaf-node (if opened "/fo.gif" "/fc.gif") "/lf.gif")
	      :class "ic" :align "top"))
      "&nbsp;&nbsp;" (funcall draw-node-fn node))
     (incf level)
     (setf previous-lasts (append previous-lasts (list last)))
     (when opened
       (map-sub-nodes #'(lambda (node first last index)
			  (draw-huge-tree node level index first last previous-lasts path draw-node-fn))
		      node)))))
