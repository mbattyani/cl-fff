(in-package interface)

(defmacro %html-string% (&body body)
  `(html:html-to-string ,body))
  
(defun write-item-id&style (item &optional add remove)
  (html:fast-format html:*html-stream* " id=~s" (name item))
  (when (tooltip item) (html:fast-format html:*html-stream* " TITLE=~s" (tooltip item)))
  (when (tab-id item) (html:fast-format html:*html-stream* " TABINDEX=~s" (tab-id item)))
  (when (html-class item)(html:fast-format html:*html-stream* " CLASS=~s" (html-class item)))
  (write-string " STYLE=\"" html:*html-stream*)
  (unless (stand-alone item)
    (if (floating-position item)
      (html:ffmt "width:~dpx;height:~dpx;position:relative;" (round (dx item))(round (dy item)))
      (html:ffmt "top:~dpx;left:~dpx;width:~dpx;height:~dpx;position:absolute;"
		 (round (y item))(round (x item))(round (dx item))(round (dy item)))))
  (write-css-style-string (style item) t)
  (write-css-style-string add t)
  (write-char #\" html:*html-stream*))

(defun write-named-css-style (style-name style-values)
  (html:fast-format html:*html-stream* ".~a {~%" style-name)
  (loop for (style value) in style-values
	do
	(html:fast-format html:*html-stream* "~a:~a;~%" style value))
  (write-string "} " html:*html-stream*))

(defun write-css-style-string (style-values &optional no-quotation full-string)
  (when full-string (write-string "style=" html:*html-stream*))
  (unless no-quotation (write-char #\" html:*html-stream*))
  (loop for (style value) in style-values
	do
	(html:fast-format html:*html-stream* "~a:~a;" style value))
  (unless no-quotation (write-char #\" html:*html-stream*)))

(defun input-type (ui-item)
  (typecase ui-item
    (radio-button "radio")
    (check-box-button "checkbox")
    (checked-button "checkbox")
    (def-push-button "button")
    (push-button "button")
    (multi-line-edit "text")
    (edit "text")))

(defmethod write-construction ((item pane) container language)
  (mapcar #'(lambda (sub-pane)
	      (write-construction sub-pane container language))
	  (sub-panes item)))

(defmethod write-construction ((item ui-item) container (language (eql :html)))
  (html:html-to-string
   ((:input type (input-type item) 
	    :exec (write-item-id&style item)
	    :exec (html:fast-format html:*html-stream* " onchange='fire_onchange(~s,~a.value);'" (name item)(name item))))))

(defmethod write-construction ((item button) container (language (eql :html)))
  (html:html-to-string
   ((:input type (input-type item) 
	    :exec (write-item-id&style item)
	    :exec (html:fast-format html:*html-stream* " onclick ='fire_onclick(~s,~a.value);'" (name item)(name item))))))

(defmethod write-construction ((item check-box-button) container (language (eql :html)))
  (html:html-to-string
   ((:input type (input-type item) 
	    :exec (write-item-id&style item)
	    :exec (html:fast-format html:*html-stream* " onclick ='fire_onchange(~s,~a.checked);'" (name item)(name item))))))

(defmethod write-construction ((item radio-button) container (language (eql :html)))
  (html:html-to-string
   ((:input :type (input-type item)
	    :name (name container)
	    :exec (write-item-id&style item (list (list :background-color "red")))
	    :exec (html:fast-format html:*html-stream* " onclick='fire_onchange(~s,~a);'"
				    (name container)(index item))))))

(defmethod write-construction ((item panel) container (language (eql :html)))
  `((:div :insert-string ,(html:html-to-string (write-item-id&style item)))
    ,(when (text item)
	   (html:html-to-string ((:div :exec (write-css-style-string
					      (append (list (list :width "100%")
							    (list :height (round (caption-height item))))(caption-style item)) nil t))
				 (html:write-string-quoting-specials (text item)))))
    ,@(html-source item)
    ,@(mapcar #'(lambda (sub-item)
		  (write-construction sub-item item :html))
	      (sub-items item))))

(defmethod write-construction ((item tabbed-pane) container (language (eql :html)))
  `((:tab :name ,(name item) :class ,(html-class item) :remove-sibling-borders t)
    ,@(loop for tab-item in (tab-items item)
	    collect (list (text tab-item) (write-construction (item tab-item) item :html)))))

(defmethod write-construction ((item label) container (language (eql :html)))
  (html:html-to-string ((:span :exec (write-item-id&style item ))
			(html:write-string-quoting-specials (text item)))))


;(defun write-style (item &key name back-color border visibility)
;  (format nil "id=~s~a~a style=\"top:~dpx;left:~dpx;width:~dpx;height:~dpx;position:absolute~a~a~a\""
;	  (name item)
;	  (if name (format nil " name=~a" name) "")
;	  (if (tooltip item) (format nil "TITLE=\"~a\"" (tooltip item)) "")
;	  (round (y item))(round (x item))(round (dx item))(round (dy item))
;	  (if back-color (format nil ";background-color:#~6,'0x" back-color) "")
;	  (if border (format nil ";border:groove;border-width thin" ) "")
;	  (if visibility (format nil ";visibility:~a" visibility) "")
;	  ))

(defmethod write-construction ((item combo-box) container (language (eql :html)))
  (html:html-to-string
   ((:select :exec (write-item-id&style item)
	     :exec (html:fast-format html:*html-stream* " onchange='fire_onchange(~s,~a.value);'" (name item)(name item)))(defun translate-tag (attributes form)
  (if (and (consp (first form))(eq (caar form) 'quote)(= (length form) 1))
      `(write-string
	(meta::translate ',(mapcar #'(lambda(x)(if (stringp x)(html::quote-string x) x))
				   (cadar form))) html:*html-stream*)
      `(html:esc (meta::translate ,@form))))

(html::add-func-tag :translate 'translate-tag)

    (loop for combo-item in (combo-items item)
	  for i from 0
	  do (format html:*html-stream* "<option value = ~d>~a~%" i combo-item)))))

(defmethod make-set-value-javascript ((item combo-box) value slot)
  (let ((position (position value (meta::choices slot) :key #'first)))
    (unless position (setf position -1))
    (html:fast-format nil "parent.document.all.~a.selectedIndex='~a';" (name item) position)))

(defmethod make-set-value-javascript ((item check-box-button) value slot)
  (html:fast-format nil "parent.document.all.~a.checked=~a;" (name item) (if value "true" "false")))

(defmethod write-construction ((item image) container (language (eql :html)))
  (when (click-map item) )
  (html:html ((:img :exec (write-item-id&style item '(("cursor" "hand")))
		    :src  (filename item)
		    :exec (when (click-id item)
			    (html:ffmt " onclick='fire_onclick(~s, ~s);'" (name item)(click-id item)))))))

;  (format html:*html-stream* "<OBJECT CLASSID=\"clsid:CB4C9FD7-C14F-11D3-A943-00C095ED76C8\" HEIGHT=~d WIDTH=~d ~a></OBJECT>~%" (round (dx item))(round (dy item))(write-style item))
; (with-script (dolist (combo-item (combo-items item))
;		 (format html:*html-stream* "document.~a.Add(~s, \"\");" (name item) combo-item)))

;<BODY  onload=\"RegisterW()\" onunload=\"UnRegisterW()\">
;<OBJECT ID=\"pn\" CLASSID=\"CLSID:DF3AFEB0-F464-11D3-A9D9-0C095ED76C09\"></OBJECT>

(defmethod make-set-value-javascript ((item html-item) value slot)
  (when (meta::choices slot)
    (setf value (meta::translate (second (assoc value (meta::choices slot))))))
  (let ((j-value (html:quote-javascript-string
		  (if (stringp value) value (write-to-string value)))))
    (concatenate 'string "parent.f826svi('" (name item) "', '" j-value "');")))

(defmethod make-set-status-javascript ((item html-item) status slot)
  (when (or (not slot) (modifiable-p *dispatcher*))
    (if status
	(concatenate 'string "parent.document.all." (name item) ".disabled=true;")
	(concatenate 'string "parent.document.all." (name item) ".disabled=false;"))))

(defmethod make-show-error-javascript ((item ui-item) error-text slot)
  (format nil "parent.document.all.~a.style.backgroundColor='red';" (name item)))

(defmethod write-interface (item (language (eql :html)))
  (html:html "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\"> "
	     (:html (:head ((:meta "name" "save" "content" "history")))
		    ((:body) ; "onload" "RW()" "onunload" "URW()")
;		     ((:applet "ID" "Applet1" "name" "Applet1" "code" "Applet1.class" "codebase" "http://www.expertgestion.com" "width" "1" "height" "1" "MAYSCRIPT" nil))
		     ((:applet "ID" "Applet1" "name" "Applet1" "code" "Applet1.class" "codebase" "http://127.0.0.1" "width" "1" "height" "1" "MAYSCRIPT" nil))
		     ((:script "language" "JavaScript")
"
function f85425(Tabs, Panes, nTab, BaseClassName, SelectedClassName, remove)
      {
      if (!nTab)
         nTab = 0;
      else
         nTab = parseInt(nTab);
      event.cancelBubble = true;
      var Tab;
      for (var i = 0; i < Tabs.length; i++)
      {
      Tab = Tabs[i];
      Tab.style.borderLeftStyle = \"\";
      Tab.style.borderRightStyle = \"\";
      Tab.className = BaseClassName;
      Panes[i].style.visibility = \"hidden\";
      }
      Tabs[nTab].className = SelectedClassName;
      Panes[nTab].style.visibility = \"inherit\";
      if (remove)
      {
      Tab = Tabs[nTab+1];
      if (Tab) Tab.style.borderLeftStyle = \"none\";
      Tab = Tabs[nTab-1];
      if (Tab) Tab.style.borderRightStyle = \"none\";
      }
      event.returnValue = false;
      }
      
function f8532(Tabs, table, SelClass) {
      var iTabSelected = 0;
      var iLength = Tabs.length;
      for (var i = 0; i < iLength; i++) 
         if (Tabs[i].className == SelClass) iTabSelected = i;
      table.setAttribute(\"s\", iTabSelected);
      }

")
			   ((:style "TYPE" "text/css") (%map-all-items 'write-item-styles item nil :html))
			   (write-construction item nil :html)))))

(defun translate-tag (attributes form)
  (if (and (consp (first form))(eq (caar form) 'quote)(= (length form) 1))
      `(write-string
	(meta::translate ',(mapcar #'(lambda(x)(if (stringp x)(html::quote-string x) x))
				   (cadar form))) html:*html-stream*)
      `(html:esc (meta::translate ,@form))))

(html::add-func-tag :translate 'translate-tag)
