(in-package interface)

(defun make-label (text &rest args &key (container *top-level-item*) tooltip slot &allow-other-keys)
  (multiple-value-bind (dx dy)(text-size text)
    (apply 'make-instance `(label :text ,text :tooltip ,(get-tooltip slot tooltip) ,@args
			    :x-min ,dx :x-value ,(+ dx 4) :x-elasticity 1.0 :x-compressibility 4.0
			    :y-min ,dy :y-max ,(+ dy 10)  :y-value ,(+ dy 6) :y-elasticity 2.0 :y-compressibility 4.0
			    :parent ,container))))

(defun compute-button-width (text)
  (multiple-value-bind (dx dy)(text-size text)
    (+ 10.0 (* 10 (ceiling dx 10)))))

(defun make-vtab-buttons (tab-items &rest args &key (container *top-level-item*) tooltip slot &allow-other-keys)
  (let ((dx (loop for (text pane-name) in tab-items
		  maximize (compute-button-width text))))
    (apply 'make-instance `(vtab-buttons :tab-items ,tab-items ,@args :parent ,container :tooltip ,(get-tooltip slot tooltip)
			    :x-min ,(max dx 30) :x-value ,(+ dx 4) :x-max: ,(+ dx 20) :x-elasticity 2.0 :x-compressibility 4.0))))

(defun make-push-button (text &rest args &key (container *top-level-item*) tooltip slot &allow-other-keys)
  (multiple-value-bind (dx dy)(text-size text)
    (setf dx (+ 10.0 (* 10 (ceiling dx 10))))
    (apply 'make-instance `(push-button :text ,text  :tooltip ,(get-tooltip slot tooltip) ,@args
		   :x-min ,(max dx 30) :x-value ,(+ dx 4) :x-max: ,(+ dx 20) :x-elasticity 2.0 :x-compressibility 4.0
		   :y-min ,dy :y-max ,(+ dy 10)  :y-value ,(+ dy 6) :y-elasticity 2.0 :y-compressibility 4.0
		   :parent ,container))))

(defun make-radio-button (text &rest args &key (container *top-level-item*) tooltip slot &allow-other-keys)
  (multiple-value-bind (dx dy)(text-size text)
    (setf dx (+ 10.0 (* 10 (ceiling dx 10))))
    (apply 'make-instance `(radio-button :text ,text  :tooltip ,(get-tooltip slot tooltip) ,@args
			    :x-min ,(max dx 30) :x-value ,(+ dx 4) :x-max: ,(+ dx 20) :x-elasticity 2.0 :x-compressibility 4.0
			    :y-min ,dy :y-max ,(+ dy 10)  :y-value ,(+ dy 6) :y-elasticity 2.0 :y-compressibility 4.0
			    :parent ,container))))

(defun make-check-box (text &rest args &key (container *top-level-item*) tooltip slot &allow-other-keys)
  (multiple-value-bind (dx dy)(text-size text)
    (setf dx (+ 10.0 (* 10 (ceiling dx 10))))
    (apply 'make-instance `(check-box-button :text ,text  :tooltip ,(get-tooltip slot tooltip) ,@args
			    :x-min ,(max dx 30) :x-value ,(+ dx 4) :x-max: ,(+ dx 20) :x-elasticity 2.0 :x-compressibility 4.0
			    :y-min ,dy :y-max ,(+ dy 10)  :y-value ,(+ dy 6) :y-elasticity 2.0 :y-compressibility 4.0
			    :parent ,container))))

(defun make-edit (&rest args &key (container *top-level-item*) tooltip slot &allow-other-keys)
  (apply 'make-instance `(edit  :tooltip ,(get-tooltip slot tooltip) ,@args
			  :x-min 30 :x-value 100 :x-elasticity 10.0 :x-compressibility 2.0
			  :y-min 18 :y-max 25 :y-value 22 :y-elasticity 2.0 :y-compressibility 4.0
			  :parent ,container)))

(defun make-list-box (&rest args &key (container *top-level-item*) tooltip slot &allow-other-keys)
  (apply 'make-instance `(list-box  :tooltip ,(get-tooltip slot tooltip) ,@args
		 :x-min 30 :x-value 100 :x-elasticity 10.0 :x-compressibility 2.0
		 :y-min 40 :y-value 100 :y-elasticity 10.0 :y-compressibility 2.0
		 :parent ,container)))

(defun make-label-value (text &optional value &rest args)
  (if value
    (make-instance 'hlabel-value-pane :label-pane (apply 'make-label text args) :value-pane (apply-layout value))
    (apply 'make-label text args)))

(defun make-combo (combo-items &rest args &key (container *top-level-item*) tooltip slot &allow-other-keys)
  (apply 'make-instance `(combo-box  :tooltip ,(get-tooltip slot tooltip) ,@args
			  :combo-items ,combo-items
			  :x-min 30 :x-value 100 :x-elasticity 10.0 :x-compressibility 2.0
			  :y-min 18 :y-max 25 :y-value 22 :y-elasticity 2.0 :y-compressibility 4.0
			  :parent ,container)))

(defun make-space (&rest args &key (container *top-level-item*) &allow-other-keys)
  (apply 'make-instance `(pane ,@args
			  :x-min 2 :x-value 5 :x-max 7 :x-elasticity 10.0 :x-compressibility 2.0
			  :y-min 2 :y-value 5 :y-max 7 :y-elasticity 10.0 :y-compressibility 2.0
			  :parent ,container)))

(defun make-image (&rest args &key (container *top-level-item*) tooltip slot (dx 16) (dy 16) &allow-other-keys)
  (apply 'make-instance `(image :tooltip ,(get-tooltip slot tooltip) ,@args
			  :x-min ,dx :x-max ,dx :x-value ,dx :x-elasticity 0.0 :x-compressibility 0.0
			  :y-min ,dy :y-max ,dy :y-value ,dy :y-elasticity 0.0 :y-compressibility 0.0
			  :parent ,container)))