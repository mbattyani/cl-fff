(in-package interface)

(defparameter *layouts* `(:html make-html-layout :view make-object-view-layout
			  :h make-horizontal-layout :v make-vertical-layout :g make-group-layout
			  :t make-tabbed-layout :vt make-vtab-buttons :s make-slot-layout
			  :stack make-stack-layout :sub-obj make-sub-object-layout
			  :push-button make-push-button :radio-button make-radio-button :check-box make-check-box
			  :submit make-submit-button
			  :panel make-panel-layout :list-box make-list-box
			  :edit make-edit :combo make-combo :label make-label-value :combo make-combo
			  :space make-space
			  :image make-image))

(defun map-names-in-view (view func)
  (cond
   ((null view)())
   ((listp view)(mapcar #'(lambda(x) (map-names-in-view x func)) view))
   ((keywordp view) view)
   ((symbolp view)(funcall func view))
   (t view)))

(defun apply-layout (view)
  (let ((layout-func (getf *layouts* (first view))))
    (when layout-func (apply layout-func (rest view)))))

(defun make-layout-pane (item)
  (if (listp item)
      (apply-layout item)
    (make-pane-for-slot item)))

(defun make-layout-list (list)
  (mapcar #'make-layout-pane list))

(defun make-pane-for-slot (slot-name &rest args)
  (apply 'make-slot-layout slot-name args))

(defun %map-all-items (function items container language &optional after)
  (cond
   ((null items) nil)
   ((listp items) (dolist (i items) (%map-all-items function i container language after)))
   (t (unless after (funcall function items container language))
      (%map-all-items function (sub-items items) items language after)
      (when after (funcall function items container language)))))

(defun make-view-layout (object-view x y dx dy country-language)
  (let ((view (find-if #'(lambda(v)
			   (and (= x (x v))(= y (y v))(= dx (dx v))(= dy (dy v))
				(eq country-language (country-language v))))
		       (view-layouts object-view))))
    (unless view
      (setf view (make-instance 'view-layout :object-view object-view :country-language country-language :x x :y y :dx dx :dy dy))
      (push view (view-layouts object-view)))
    (let ((*country-language* country-language)
	  (*root-item* (make-instance 'page-root))
	  (*top-level-item* (make-instance 'ui-root :border :etched :text "Démo Fractal Concept" :view-layout view)))
      (setf (page-root view) *root-item*)
      (let ((root *top-level-item*))
	(setf (html-func *root-item*) #'(lambda (socket) (let ((html::*html-stream* socket))
							   (write-interface root :html)))))
      (setf (sub-panes *top-level-item*)(apply-layout (description object-view)))
      (optimize-layouts *top-level-item*)
      (compute-layout *top-level-item* x y dx dy)
      (compute-tab-order *top-level-item*))
    view))

(defun find-item-for-slot (items slot)
  (catch 'found (%find-item-for-slot items slot)))

(defun %find-item-for-slot (items slot)
  (cond
   ((null items) nil)
   ((listp items) (dolist (i items) (%find-item-for-slot i slot)))
   (t (when (eq slot (slot items))(throw 'found items))
      (%find-item-for-slot (sub-items items) slot))))

