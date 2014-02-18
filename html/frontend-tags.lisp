(in-package #:html)

(defparameter *jscript-lib-file* (merge-pathnames #P"fractal.js"
                                                  (asdf:system-source-directory :html)))

(defparameter *jscript-ws-lib-file* (merge-pathnames #P"fractal-ws.js"
                                                  (asdf:system-source-directory :html)))

(defun use-ui-tag (attributes form)
  (declare (ignore attributes form))
  #+nil`(html (:jscript (:insert-file ,*jscript-lib-file*))))

(defun use-ui-ws-tag (attributes form)
  (declare (ignore attributes form))
  #+nil`(html (:jscript (:insert-file ,*jscript-ws-lib-file*))))

(add-func-tag :use-ui 'use-ui-tag)
(add-func-tag :use-ui-ws 'use-ui-ws-tag)

(defvar *tab-unsel-class-name* nil)
(defvar *tab-sel-class-name* nil)
(defvar *tab-void-class-name* nil)
(defvar *tab-pane-class-name* nil)
(defvar *tab-tab-array-name* nil)
(defvar *tab-pane-array-name* nil)
(defvar *tab-items* nil)

(defmethod tab-tag ((frontend html) attributes tab-forms)
  (destructuring-bind (&key (name (string (gensym))) (class "tab0")
                            (remove-sibling-borders t) &allow-other-keys) attributes
    (let ((unsel-class-name (concatenate 'string class "UnSel"))
          (sel-class-name (concatenate 'string class "Sel"))
          (void-class-name (concatenate 'string class "Void"))
          (pane-class-name (concatenate 'string class "Pane"))
          (tab-array-name (concatenate 'string name "Tabs"))
          (pane-array-name (concatenate 'string name "Panes"))
          (table-name (concatenate 'string name "Table")))
      `(optimize-progn
        ,(html-gen
          `((:table :id ,table-name :class ,class :cellspacing "0")
            ((:tr :valign "middle")
             ,@(loop for (tab-text . nil) in tab-forms
                  as tab-class = sel-class-name then unsel-class-name
                  as i from 0
                  as item-name = (format nil "~a~d" tab-array-name i)
                  collect (html-gen `((:td :id ,item-name :class ,tab-class
                                           ;; :onmouseover ,(format nil "this.className='~aOver';" tab-class)
                                           ;; :onmouseout ,(format nil "this.className='~a';" tab-class)
                                           :onclick ,(format nil "f85425(~a, ~a, ~d, '~a', '~a', ~d);"
                                                             tab-array-name pane-array-name i unsel-class-name
                                                             sel-class-name (if remove-sibling-borders 1 0)))
                                      "&nbsp;" ,tab-text "&nbsp;")))
             ((:td "align" "center" "width" "100%" :class ,void-class-name) "&nbsp;"))))
        ;; (write-string ,(format nil "<SCRIPT>~a.style.display=\"\";</SCRIPT>" table-name) *html-stream*)
        ,@(loop for (nil . tab-form) in tab-forms
             as visibility = "" then "none"
             as i from 0
             as pane-name = (format nil "~a~d" pane-array-name i)
             collect (html-gen `((:div :id ,pane-name :class ,pane-class-name
                                       :style ,(format nil "display:~a;" visibility))
                                 ,@tab-form)))
        ,(html-gen `(:jscript
                     ,(format nil "var ~a;~a=new Array();var ~a;~a=new Array();"
                              pane-array-name pane-array-name
                              tab-array-name tab-array-name)
                     ,@(loop for i from 0 below (length tab-forms)
                          collect (format nil "~a[~d]=fgt('~a~d');~a[~d]=fgt('~a~d');"
                                          pane-array-name i pane-array-name i
                                          tab-array-name i tab-array-name i))))))))

(defmethod tab-tag ((frontend bootstrap) attributes tab-forms)
  (destructuring-bind (&key (name (string (gensym))) (bs-class "nav-tabs") &allow-other-keys) attributes
    `(optimize-progn
      ,(html-gen
        `((:div)
          ((:ul :id ,name :class ,(format nil "nav ~a" bs-class) :cellspacing "0")
           ,@(loop for (tab-text . nil) in tab-forms
                for i from 0
                for pane-name = (format nil "#~a~d" name i)
                collect (html-gen `(,(if (zerop i) '(:li :class "active") :li)
                                     ((:a :href ,pane-name :data-toggle "tab") ,tab-text)))))))
      ,(html-gen
        `((:div :class "tab-content")
          ,@(loop for (nil . tab-form) in tab-forms
               for i from 0
               for pane-name = (format nil "~a~d" name i)
               collect (html-gen `((:div :id ,pane-name :class
                                         ,(if (zerop i) "tab-pane active" "tab-pane"))
                                   ,@tab-form))))))))

;; :name :class :remove-sibling-borders are optional attributes
;; syntax
#+nil ((:tab :name "tt1" :class "tab0" :remove-sibling-borders t)
       ("tab-name1" <lhtml>)("tab-name2" <lhtml>))

(add-func-tag :tab 'tab-tag t)

;(defun combo-choice-tag (attributes choice-forms)
;  (destructuring-bind (&key (name (string (gensym)))(class "combo0")(align :left)
;			    (:offset 18)(:nb-col 1)) attributes
;    ;choice-form is (value . forms)
;    (let ((choice-class-name (concatenate 'string class "C"))
;	  (table-class-name (concatenate 'string class "T"))
;	  (table-name (concatenate 'string name "T")))
;    `(optimize-progn
;      ,(html-gen
;	`(((:span :style "position:relative;")
;	   ((:span :class ,class :id ,name) ,(first choice-forms))
;	   ((:div :style (format nil "background-color:#E0E0E0;position:absolute;top:~a;left:0;height:100;"
;				 offset))
;	    (:table :id ,table-name :class ,table-class-name :style "display:'hidden';")
;	    ,@(loop for (value . forms) in (rest choice-forms)
;		    collect (html-gen `(((:tr :valign "middle")
;					 (:td :class ,tab-class
;					  :onclick ,(format nil "f85425(~a, ~a, '~d', '~a', '~a', ~d);"
;							  tab-array-name pane-array-name i unsel-class-name
;							  sel-class-name (if remove-sibling-borders 1 0)))
;				       "&nbsp;" ,(quote-string tab-text) "&nbsp;")))
;	   ((:td "align" "center" "width" "100%" :class ,void-class-name) "&nbsp;"))))
;      (write-string ,(format nil "<SCRIPT>~a.style.display=\"block\";</SCRIPT>" table-name) *html-stream*)
;      ,@(loop for (tab-text . tab-form) in tab-forms
;	      as visibility = "block" then "none"
;	      collect (html-gen `((:div :id ,pane-array-name :class ,pane-class-name :style ,(format nil "display:'~a';" visibility))
;				  ,@tab-form)))))))

;(add-func-tag :combo-choice 'combo-choice-tag)

(defmethod tab-ex-tag ((frontend html) attributes forms)
  (destructuring-bind (&key (name (string (gensym)))(class "tab0") (remove-sibling-borders t)) attributes
    (declare (ignore remove-sibling-borders))
    (let ((*tab-unsel-class-name* (concatenate 'string class "UnSel"))
	  (*tab-sel-class-name* (concatenate 'string class "Sel"))
	  (*tab-tab-array-name* (concatenate 'string name "Tabs"))
	  (*tab-pane-array-name* (concatenate 'string name "Panes"))
	  (*tab-items* nil)
	  ;(table-name (concatenate 'string name "Table"))
          )
      `(optimize-progn
	,@(mapcar 'html-gen tab-forms)
	,(html-gen `(:jscript
		     ,(format nil "var ~a;~a=new Array();var ~a;~a=new Array();"
			      *tab-pane-array-name* *tab-pane-array-name*
			      *tab-tab-array-name* *tab-tab-array-name*)
		     ,@(loop for nil in *tab-items*
			   as i from 0
			   collect (format nil "~a[~d]=fgt('~a~d');~a[~d]=fgt('~a~d');"
					   *tab-pane-array-name* i *tab-pane-array-name* i
					   *tab-tab-array-name* i *tab-tab-array-name* i))))))))

(add-func-tag :tab-ex 'tab-ex-tag t)

(defmethod tab-item-tag ((frontend html) attributes forms)
  (destructuring-bind (item-number) attributes
    (let ((item-name (format nil "~a~d" *tab-tab-array-name* item-number)))
      (push (list item-number item-name) *tab-items*)
      (html-gen
       `((:span :id ,item-name
	  :onclick ,(format nil "f85425(~a, ~a, '~d', '~a', '~a', ~d);"
			    *tab-tab-array-name* *tab-pane-array-name* item-number *tab-unsel-class-name*
			    *tab-sel-class-name* 0))
	 ,@forms)))))

(add-func-tag :tab-item 'tab-item-tag t)

(defmethod tab-pane-tag ((frontend html) attributes forms)
  (destructuring-bind (item-number) attributes
    (let ((pane-name (format nil "~a~d" *tab-pane-array-name* item-number)))
      (html-gen
       `((:div :id ,pane-name :style ,(if (zerop item-number) "display:;" "display:none;"))
	 ,@forms)))))

(add-func-tag :tab-pane 'tab-pane-tag t)

(defmethod on-off-tag ((frontend html) attributes forms)
  (destructuring-bind (&key (name (string (gensym)))(class "onoff")(tag :span)) attributes
    (destructuring-bind (on-forms off-forms &rest pane-forms) forms
      (let ((on-name (concatenate 'string name "o"))
	    (off-name (concatenate 'string name "h"))
	    (div-name (concatenate 'string name "d")))
	`(optimize-progn
           ,(html-gen `((,tag :id ,on-name :class ,(concatenate 'string class "o") :style "display:none;"
                              :onclick ,(format nil "f825h('~a');f825s('~a');f825h('~a');" on-name off-name div-name)) ,on-forms))
           ,(html-gen `((,tag :id ,off-name :class ,(concatenate 'string class "h") ; :style "display:;"
                              :onclick ,(format nil "f825s('~a');f825h('~a');f825s('~a');" on-name off-name div-name)) ,off-forms))
           ,(when pane-forms (html-gen `((:div :id ,div-name :class ,(concatenate 'string class "d") :style "display:none;") ,@pane-forms))))))))

;example : (:on-off ("change password")("hide password")(:p "div to hide"))
(add-func-tag :on-off 'on-off-tag t)

