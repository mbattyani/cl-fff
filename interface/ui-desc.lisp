(in-package #:interface)

(defvar *next-id* 1000)
(defvar *current-sub-object-path* nil)

;(defclass page-root ()
;   ((name     :initform nil :accessor name :type string :initarg :name)
;    (id       :accessor id)
;    (all-items :initform (make-hash-table :test #'equal) :accessor all-items)
;    (static-page-p :initform t :accessor static-page-p)
;    (html-func :initform nil :accessor html-func)
;    (html-data :initform nil :accessor html-data)
;    (view-layout :initform nil :accessor view-layout :initarg :view-layout)
;   ))

;(defmethod write-html ((page page-root) socket)
;  (if (static-page-p page)
;    (progn
;      (unless (html-data page)
;	(setf (html-data page) (with-output-to-string (html:*html-stream*) (funcall (html-func page) socket))))
;      (write-string (html-data page) socket))
;    (funcall (html-func page) socket)))

;(defmethod initialize-instance :after ((page page-root) &rest init-options &key &allow-other-keys)
;  (setf (id page) (incf *next-id*))
;  (setf (name page) (format nil "P~a" (id page)))
;  (setf (gethash (name page) *all-page-roots*) page))

(defvar *default-disabled-p-fn* #'(lambda (obj slot)
                                    (declare (ignore obj slot)) nil))

(defclass html-item ()
  ((name     :initform nil :accessor name :type string)
   (id       :accessor id)
   (tab-id   :initform nil :accessor tab-id)
   (tooltip  :initform nil :accessor tooltip :initarg :tooltip)
   (read-only  :initform nil :accessor read-only :initarg :read-only)
   (force-visible  :initform nil :accessor force-visible :initarg :force-visible)
   (sub-items :initform nil :accessor sub-items :initarg :sub-items)
   (slot     :initform nil :accessor slot :initarg :slot)
   (disabled-p-fn :initform *default-disabled-p-fn* :accessor disabled-p-fn :initarg :disabled-p-fn)
   ))

(defclass ui-item (html-item pane)
  ((name     :initform nil :accessor name :type string :initarg :name)
   (id       :accessor id :initarg :id)
   (tab-id   :initform nil :accessor tab-id)
   (floating-position   :initform nil :accessor floating-position :initarg :floating-position)
   (x        :initform 0.0 :accessor x  :type float :initarg :x)
   (y        :initform 0.0 :accessor y  :type float :initarg :y)
   (dx       :initform 0.0 :accessor dx :type float :initarg :dx)
   (dy       :initform 0.0 :accessor dy :type float :initarg :dy)
   (tooltip  :initform nil :accessor tooltip :initarg :tooltip)
   (read-only  :initform nil :accessor read-only :initarg :read-only)
   (parent   :initform nil :accessor parent :initarg :parent)
   (sub-items :initform nil :accessor sub-items :initarg :sub-items)
   (action-func  :initform nil :accessor action-func :initarg :action-func)
   (slot     :initform nil :accessor slot :initarg :slot)
   (sub-object-path :initform *current-sub-object-path* :accessor sub-object-path)
   (real-control :initform nil :accessor real-control :initarg :real-control)
   (style    :initform '() :accessor style :type string :initarg :style)
   (html-class    :initform nil :accessor html-class :type string :initarg :html-class)
   (stand-alone :initform nil :accessor stand-alone :initarg :stand-alone)
   ))

(defmethod initialize-instance :after ((item ui-item) &rest init-options &key parent &allow-other-keys)
  (unless (name item)
    (setf (name item) (format nil "G~a" *next-id*)))
  (setf (id item) *next-id*)
  (incf *next-id*)
  (when parent (push item (sub-items parent)))
  (when *root-item* (setf (gethash (name item) (all-items *root-item*)) item))
  )

(defclass ui-container (ui-item)
  (
   ))

(defmethod write-interface (item language))

(defmethod write-interface :around (item language)
  (let ((*language* language))
    (call-next-method)))

(defmethod write-declaration (ui-item container language))

(defmethod write-construction (ui-item container language))

(defmethod write-destruction (ui-item container language))

(defclass panel (ui-container)
  ((text    :initform nil :accessor text   :initarg :text)
   (style   :initform '((:border "2px groove")(:background-color "#C0C0C0")) :accessor style :initarg :style)
   (caption-style    :initform '((:background-color "#d0d0C0")) :accessor caption-style :initarg :caption-style)
   (caption-height   :initform 0 :accessor caption-height :initarg :caption-height)
   (border  :initform nil :accessor border :initarg :border)
   (html-source :initform nil :accessor html-source  :type string :initarg :html-source))
  (:default-initargs :relative-coords t))

(defclass ui-root (panel)
  ())

(defclass button-group (panel)
  ((choices :accessor choices)
   (nx :accessor nx)
   (ny :accessor ny)
   (buttons :initform () :accessor buttons))
  (:default-initargs :relative-coords t))

(defclass tab-item ()
  ((text        :initform "text"   :accessor text    :type string  :initarg :text)
   (icon        :initform nil      :accessor icon    :type string  :initarg :icon)
   (item        :initform nil      :accessor item    :type ui-item :initarg :item)
   (tooltip     :initform nil      :accessor tooltip :type string  :initarg :tooltip)
   ))

(defclass tabbed-pane (ui-container)
  ((tab-items          :initform nil   :accessor tab-items :initarg :tab-items)
   (remove-sibling-borders :initform t :accessor remove-sibling-borders :initarg :remove-sibling-borders)
   (html-class    :initform "tab1" :accessor html-class :type string :initarg :html-class)
   (tab-style          :initform nil :accessor tab-style :initarg :tab-style)
   (tab-selected-style :initform nil :accessor tab-selected-style :initarg :tab-selected-style)
   (notab-style :initform nil :accessor notab-style :initarg :notab-style)
   (pane-style :initform '((:border-bottom "2px outset")(:border-left "1px solid white")(:border-right "2px outset"))
	       :accessor pane-style :initarg :pane-style))
  (:default-initargs :relative-coords t :layout :same-place))

;   (tab-style          :initform '((:cursor "hand")(:border-top "2px groove")(:border-bottom "2px groove")(:border-left "2px groove")(:border-right "2px groove")
;				   (:background-color "#c0c0c0"))
;		       :accessor tab-style :initarg :tab-style)
;   (tab-selected-style :initform '((:cursor "hand")(:color "red")(:border-top "1px outset")(:border-left "1px outset")(:border-right "1px outset")
;				   (:background-color "#c0c0c0"))
;		       :accessor tab-selected-style :initarg :tab-selected-style)
;   (notab-style :initform '((:border-bottom "1px inset")(:background-color "#c0c0c0"))
;		:accessor notab-style :initarg :notab-style)
;   (pane-style :initform '((:border-bottom "2px outset")(:border-left "1px solid white")(:border-right "2px outset"))
;	       :accessor pane-style :initarg :pane-style))


(defclass vtab-buttons (ui-item)
  ((tab-items :initform nil :accessor tab-items :initarg :tab-items)
   )
  (:default-initargs))

(defmethod initialize-instance :after ((item tabbed-pane) &rest init-options &key &allow-other-keys)
;  (when (eq *language* :html)
    (setf (top-border item) 26
	  (bottom-border item)0
	  (left-border item) 0
	  (right-border item) 0))

(defmethod initialize-instance :after ((item panel) &rest init-options &key &allow-other-keys)
  (when (text item)
    (when (not (border item))(setf (border item) :etched))
    (when (zerop (caption-height item))(setf (caption-height item) 18)))
  (when (border item)
	(when (= (top-border item) 0) (setf (top-border item) (+ (caption-height item) 6)))
	(when (= (bottom-border item) 0) (setf (bottom-border item) 6))
	(when (= (left-border item) 0) (setf (left-border item) 6))
	(when (= (right-border item) 0) (setf (right-border item) 6))))

(defclass label (ui-item)
  ((text        :initform "text" :accessor text  :type string :initarg :text)
   (icon        :initform nil      :accessor icon  :type string :initarg :icon)
   ))

(defclass button (label)
  ())

(defclass push-button (button)
  (
   ))

(defclass def-push-button (push-button)
  (
   ))

(defclass checked-button (button)
  ((value :accessor value :initarg :value)
   (index :accessor index :initarg :index)
   (push-like :accessor push-like :initarg :push-like :initform nil)
   ))

(defclass radio-button (checked-button)
  (
   ))

(defclass check-box-button (checked-button)
  (
   ))

(defclass color-button (button)
  ((color :accessor color :initarg :color)
   ))

;; List-box
(defclass list-box (ui-item)
  (
   ))

;; Std Edit classes

(defclass edit (ui-item)
  ((password :accessor password :initarg :password :initform nil)
   ))

(defclass multi-line-edit (edit)
  (
   ))

;; Std ListBox class
;; already defined
#+nil(defclass list-box (ui-item) 
  (
   ))

;; Std ComboBox class

(defclass combo-box (ui-item)
  ((combo-items :initform nil :accessor combo-items :initarg :combo-items)
   ))

;; Std Static classes

(defclass static-control (ui-item)
  (
   ))

;; Date Time edits
(defclass date-time-edit (ui-item)
  (
   ))

(defclass date-edit (date-time-edit)
  (
   ))

(defclass time-edit (date-time-edit)
  (
   ))

(defclass group-box (ui-container)
  (
   ))

(defclass image (ui-item)
  ((filename :initform "none" :accessor filename :initarg :filename)
   (click-map :initform nil :accessor click-map :initarg :click-map)
   (click-id :initform nil :accessor click-id :initarg :click-id)
   )
  (:default-initargs))

#+nil
(defclass simple-list (ui-item)
  ((html :accessor html :initform nil :initarg :html)
   (click-id :initform nil :accessor click-id :initarg :click-id)
   )
  (:default-initargs))

