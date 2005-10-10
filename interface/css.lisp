(in-package interface)

(defclass object-view ()
  ((font-family :accessor font-family :initarg :font-family font-family :user-name "font-family")
   (font-size :accessor font-size :initarg :font-size font-size :user-name "font-size")
   (font-style :accessor font-style :initarg :font-style font-style :user-name "font-style"
	       :choices '((:normal "normal")(:italic "italic")))
   (font-weight :accessor font-weight :initarg :font-weight font-weight :user-name "font-weight"
		:choices '((:normal "normal")(:bold "bold")(:bolder "bolder")(:lighter "lighter")))
   (text-align :accessor text-align :initarg :text-align text-align :user-name "text-align"
	       	:choices '((:left "left")(:right "right")(:center "center")(:justified "justified")))
   (text-decoration :accessor text-decoration :initarg :text-decoration text-decoration :user-name "text-decoration"
		    :choices '((:none "none")(:underline "underline")(:overline "overline")(:line-through "line-through")))
   (font-family :accessor font-family :initarg :font-family font-family :user-name "font-family")
   (font-family :accessor font-family :initarg :font-family font-family :user-name "font-family")
   (font-family :accessor font-family :initarg :font-family font-family :user-name "font-family")
   (font-family :accessor font-family :initarg :font-family font-family :user-name "font-family")
   (name   :initform ""  :accessor name :initarg :name :user-name "view name")
   (user-name   :initform ""  :accessor user-name :initarg :user-name :user-name "user name")
   (description :list-of-values t :initarg :description :initform () :accessor description :user-name "description")
   (comment :type string :initarg :comment :initform "" :accessor comment :user-name "commentaire")
   (views  :accessor views :initform () :visible nil :stored nil)
   )
  (:guid #x0EEAC0902E8911d4AA1400C095ED76C8
   :user-name "CSS Style"))

