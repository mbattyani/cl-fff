(in-package interface)

(defvar *current-pane* nil)
(defvar *current-window* nil)

(defun split-panes-args (list)
  (loop for rest on list
	as item = (first rest)
	until (keywordp item)
	collect item into panes
	finally return (values panes rest)))

(defun get-translated-string (strings &optional (language *country-language*))
  (cond
    ((not strings) "")
    ((stringp strings) strings)
    (t (let ((string (getf strings language "error no translation")))
	 (if (stringp string) string (first string))))))

(defun make-vertical-layout (&rest arguments)
  (multiple-value-bind (panes args) (split-panes-args arguments)
     (apply #'make-instance 'vertical-layout-pane :pane-list (make-layout-list panes) args)))

(defun make-horizontal-layout (&rest arguments)
  (multiple-value-bind (panes args) (split-panes-args arguments)
     (apply #'make-instance 'horizontal-layout-pane :pane-list (make-layout-list panes) args)))

(defun make-stack-layout (&rest arguments)
  (multiple-value-bind (panes args) (split-panes-args arguments)
     (apply #'make-instance 'pane :layout :same-place :sub-panes (make-layout-list panes) args)))

(defun make-group-layout (title pane &rest args)
  (let ((*top-level-item* (apply #'make-instance 'panel :parent *top-level-item*
				 :text (get-translated-string title) args)))
    (setf (sub-panes *top-level-item*)(list (make-layout-pane pane)))
    (setf (need-to-recompute *top-level-item*) t)
    (recompute-elastics *top-level-item*)
    *top-level-item*))

(defun make-panel-layout (pane &rest args)
  (let ((*top-level-item* (apply #'make-instance 'panel :parent *top-level-item* args)))
    (setf (sub-panes *top-level-item*)(list (make-layout-pane pane)))
    (setf (need-to-recompute *top-level-item*) t)
    (recompute-elastics *top-level-item*)
    *top-level-item*))

(defun make-tabbed-layout (tab-items &rest args)
  (let ((*top-level-item* (apply #'make-instance 'tabbed-pane :parent *top-level-item* args)))
    (setf (tab-items *top-level-item*)
	  (loop for (text pane . args) in tab-items
		collect (apply #'make-instance 'tab-item :text (get-translated-string text)
			       :item (make-panel-layout pane  :left-border 3 :top-border 3 :right-border 3
							:bottom-border 20) args)))
    (setf (sub-panes *top-level-item*)(mapcar #'item (tab-items *top-level-item*)))
    (setf (need-to-recompute *top-level-item*) t)
    (recompute-elastics *top-level-item*)
    *top-level-item*))

(defun make-html-layout (html-source &rest args)
  (let ((*top-level-item* (apply #'make-instance 'panel :html-source html-source :parent *top-level-item* args)))
    (setf (need-to-recompute *top-level-item*) t)
    (recompute-elastics *top-level-item*)
    *top-level-item*))

(defun make-object-view-layout (view-args &rest args)
  (let ((*top-level-item* (apply #'make-instance 'panel
				 :html-source (list (cons :object-view view-args))
				 :parent *top-level-item* args)))
    (setf (style *top-level-item*) '((:background-color "#C0C0C0")))
    (setf (need-to-recompute *top-level-item*) t)
    (recompute-elastics *top-level-item*)
    *top-level-item*))

(defun get-sub-object (object sub-object-path)
  (when sub-object-path
    (loop for slot-name in sub-object-path
	  while object
	  do (setf object (slot-value object slot-name))))
  object)

(defun get-sub-object-class (class sub-object-path)
  (when sub-object-path
    (loop for slot-name in sub-object-path
	  while class
	  as slot = (meta::find-slot-by-name class slot-name)
	  do (setf class (find-class (meta::value-type slot)))
	  ))
  class)

(defun make-sub-object-layout (sub-object title pane &rest args)
  (let ((*top-level-item* (apply #'make-instance 'panel :parent *top-level-item*
				 :text (get-translated-string title) args))
	(*current-sub-object-path* (append *current-sub-object-path* (list sub-object))))
    (setf (sub-panes *top-level-item*)(list (make-layout-pane pane)))
    (setf (need-to-recompute *top-level-item*) t)
    (recompute-elastics *top-level-item*)
    *top-level-item*))

(defun make-slot-layout (slot-name &rest args)
  (let ((slot (meta::find-slot-by-name (get-sub-object-class *current-class* *current-sub-object-path*) slot-name)))
    (apply #'make-effective-slot-layout slot (meta::list-of-values slot) (meta::choices slot) (meta::value-type slot)
	   *top-level-item* args)))

(defun reset-tab-order ()
  (setf *tab-id* 1))

(defun compute-tab-order (root-item)
  (map-all-panes #'(lambda (pane)
		     (when (is-tab-stop pane)
			 (setf (tab-id pane)(incf *tab-id*))))
		 root-item))

(defmethod is-tab-stop (pane)
  nil)

(defmethod is-tab-stop ((item edit))
  t)

(defmethod is-tab-stop ((item button))
  t)

(defmethod is-tab-stop ((item combo-box))
  t)

