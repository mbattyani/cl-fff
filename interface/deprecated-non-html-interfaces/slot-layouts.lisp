(in-package #:interface)

(defun get-user-name (slot &optional (language *country-language*))
  (meta:translate (meta::user-name slot) language))

(defun get-tooltip (slot &optional tooltips (language *country-language*))
  (when slot
    (meta:translate (if tooltips tooltips (meta::tooltip slot)) language)))

(defmethod make-effective-slot-layout (slot listp (choices (eql nil)) type-name container &rest args &key stand-alone tooltip)
  (let ((edit (make-instance 'edit :slot slot :stand-alone stand-alone
			     :x-min 30 :x-value 100 :x-elasticity 10.0 :x-compressibility 2.0
			     :y-min 18 :y-max 25 :y-value 22 :y-elasticity 2.0 :y-compressibility 4.0
			     :parent container :tooltip (get-tooltip slot tooltip)))
	(label (make-label (get-user-name slot) :container container)))
    (make-instance 'hlabel-value-pane :label-pane label :value-pane edit)))

(defmethod make-effective-slot-layout (slot listp choices type-name container &rest args &key stand-alone tooltip)
  (let ((edit (make-instance 'combo-box :slot slot :stand-alone stand-alone
			     :combo-items (loop for (value string) in choices collect string)
			     :x-min 30 :x-value 100 :x-elasticity 10.0 :x-compressibility 2.0
			     :y-min 18 :y-max 25 :y-value 22 :y-elasticity 2.0 :y-compressibility 4.0
			     :parent container :tooltip (get-tooltip slot tooltip)))
	(label (make-label (get-user-name slot) :container container)))
    (make-instance 'hlabel-value-pane :label-pane label :value-pane edit)))

(defmethod make-effective-slot-layout (slot listp (choices (eql nil)) (type-name number) container &rest args &key stand-alone tooltip)
  (let ((edit (make-instance 'edit :slot slot  :stand-alone stand-alone
			     :x-min 30 :x-value 100 :x-elasticity 10.0 :x-compressibility 2.0
			     :y-min 18 :y-max 25 :y-value 22 :y-elasticity 2.0 :y-compressibility 4.0
			     :parent container :tooltip (get-tooltip slot tooltip)))
	(label (make-label (get-user-name slot) :container container)))
    (make-instance 'hlabel-value-pane :label-pane label :value-pane edit)))

(defmethod make-effective-slot-layout (slot listp (choices (eql nil)) (type-name (eql 'boolean)) container &rest args &key stand-alone tooltip)
  (let ((check (make-instance 'check-box-button :slot slot  :stand-alone stand-alone
			      :x-min 10 :x-value 15 :x-elasticity 0.0 :x-compressibility 2.0
			      :y-min 18 :y-max 25 :y-value 22 :y-elasticity 2.0 :y-compressibility 4.0
			      :parent container :tooltip (get-tooltip slot tooltip)))
	(label (make-label (get-user-name slot) :container container))
	(h-pane (make-instance 'pane :layout :horizontal)))
    (setf (sub-panes h-pane) (list check label))
    h-pane))

(defmethod make-effective-slot-layout (slot listp (choices list) type-name container &rest args &key stand-alone tooltip)
  (let ((buttons ())
	(items ())
	(group (make-instance 'button-group :slot slot :text (get-user-name slot) :parent container))
	(button-type (if listp 'check-box-button 'radio-button)))
    (setf (choices group) (meta::choices slot))
    (dolist (choice (choices group))
      (let ((button (make-instance button-type :value (first choice) :x-min 12 :x-max 20 :x-value 15
				   :x-elasticity 1.0 :x-compressibility 1.0 :parent group
				   :y-min 18 :y-max 25 :y-value 22 :y-elasticity 2.0 :y-compressibility 4.0))
	    (label (make-label (second choice) :container group)))
	(push button items)
	(push label items)
	(push (make-instance 'horizontal-layout-pane :pane-list (list button (basic-space) label)
			     :no-spaces t) buttons)))
    (setf (buttons group) (nreverse buttons))
    (setf (sub-items group) (nreverse items))
    (loop for button in (sub-items group)
	  for i from 0
	  do (when (typep button 'checked-button) (setf (index button) i)))
    (multiple-value-bind (matrix nx ny) (make-matrix-layout-pane (buttons group) :x-layout-first t)
      (setf (sub-panes group) matrix (nx group) nx (ny group) ny))
    (setf (need-to-recompute group) t)
    (recompute-elastics group)
    group))

(defmethod make-effective-slot-layout (slot listp (choices list) type-name container &rest args &key stand-alone tooltip)
  (let ((edit (make-instance 'combo-box :slot slot :stand-alone stand-alone
			     :combo-items (loop for (value string) in choices collect string)
			     :x-min 30 :x-value 100 :x-elasticity 10.0 :x-compressibility 2.0
			     :y-min 18 :y-max 25 :y-value 22 :y-elasticity 2.0 :y-compressibility 4.0
			     :parent container :tooltip (get-tooltip slot tooltip)))
	(label (make-label (get-user-name slot) :container container)))
    (make-instance 'hlabel-value-pane :label-pane label :value-pane edit)))

#|
(defclass bis-object-embedded-pane (slot-interface wll::pane)
  ()
  (:default-initargs :layout :horizontal))

(defmethod initialize-instance :after ((p bis-object-embedded-pane) &rest init-options &key view &allow-other-keys)
  (declare (ignore init-options))
  (let ((group-box (make-instance 'controls::group-box :x-align :left :text (string-capitalize (bis::user-name (slot p)) :end 1) :parent (wll::window p) :parent-pane p))
	(location (c2mop:slot-definition-location (slot p))))
    (setf (wll::sub-panes group-box) (list (create-pane-for-object view (bis::get-stored-location-value (object p) location) group-box)))
    (setf (wll::sub-panes p) group-box)
    (setf (wll::need-to-recompute p) t)
    (wll::recompute-elastics p)))
|#

(defmethod make-effective-slot-layout (slot listp (choices (eql nil))(type-name (eql 'meta::translated-string)) container &rest args &key stand-alone tooltip)
    (let* ((slot-name (get-user-name slot))
	   (translated-string-class (find-class 'meta::translated-string))
	   (french-slot-name (meta::user-name (find 'meta::french (c2mop:class-slots translated-string-class) :key 'c2mop:slot-definition-name)))
	   (english-slot-name (meta::user-name (find 'meta::english (c2mop:class-slots translated-string-class) :key 'c2mop:slot-definition-name)))
	   (*top-level-item* (make-instance 'panel :parent *top-level-item* :text slot-name))
	   (edit1 (make-instance 'edit :slot slot  :stand-alone stand-alone
				:x-min 30 :x-value 100 :x-elasticity 10.0 :x-compressibility 2.0
				:y-min 18 :y-max 25 :y-value 22 :y-elasticity 2.0 :y-compressibility 4.0
				:parent *top-level-item* :tooltip (get-tooltip slot tooltip)))
	   (label1 (make-label (concatenate 'string slot-name " (" (meta:translate english-slot-name *country-language*) ")")
			       :container *top-level-item*))
	   (edit2 (make-instance 'edit :slot slot  :stand-alone stand-alone
				:x-min 30 :x-value 100 :x-elasticity 10.0 :x-compressibility 2.0
				:y-min 18 :y-max 25 :y-value 22 :y-elasticity 2.0 :y-compressibility 4.0
				:parent *top-level-item* :tooltip (get-tooltip slot tooltip)))
	   (label2 (make-label (concatenate 'string slot-name " (" (meta:translate french-slot-name *country-language*) ")")
			       :container *top-level-item*)))
      (setf (sub-panes *top-level-item*)
	    (list (make-instance 'hlabel-value-pane :label-pane label1 :value-pane edit1)
		  (make-instance 'hlabel-value-pane :label-pane label2 :value-pane edit2)))
      (setf (need-to-recompute *top-level-item*) t)
      (recompute-elastics *top-level-item*)
      *top-level-item*))
