(in-package interface)

(defclass clipboard
                 nil
                 ((operation :value-type t :user-name (make-instance 'meta-level:translated-string :en "" :fr "operation" :de "" :sp "" :it "") :description "" :object-help (make-instance 'meta-level::object-help :en "" :fr "" :de "" :sp "" :it "" :en-h "" :fr-h "" :de-h "" :sp-h "" :it-h "") :choices (list) :visible nil :visible-groups 'nil :modifiable nil :modifiable-groups 'nil :stored nil :in-proxy nil :indexed nil :unique nil :null-allowed t :list-of-values nil :new-objects-first nil :linked-value nil :modifiable nil :duplicate-value t :make-copy-string nil :get-value-title (make-instance 'meta-level:translated-string :en "" :fr "" :de "" :sp "" :it "") :get-value-text (make-instance 'meta-level:translated-string :en "" :fr "" :de "" :sp "" :it "") :sql-length 0 :value-to-string-fn nil :nb-decimals 0 :void-link-text (make-instance 'meta-level:translated-string :en "" :fr "" :de "" :sp "" :it "") :can-create-new-object nil :create-new-object nil :process-new-object-fn 'nil :get-value-sql "" :html-tag-attributes 'nil :dont-display-null-value nil :view-type :default :slot-view-name 'nil)
                  (source-object :value-type clipboard :user-name (make-instance 'meta-level:translated-string :en "" :fr "source-object" :de "" :sp "" :it "") :description "" :object-help (make-instance 'meta-level::object-help :en "" :fr "" :de "" :sp "" :it "" :en-h "" :fr-h "" :de-h "" :sp-h "" :it-h "") :choices (list) :visible t :visible-groups 'nil :modifiable nil :modifiable-groups 'nil :stored nil :in-proxy nil :indexed nil :unique nil :null-allowed t :list-of-values nil :new-objects-first nil :linked-value nil :modifiable nil :duplicate-value t :make-copy-string nil :get-value-title (make-instance 'meta-level:translated-string :en "" :fr "" :de "" :sp "" :it "") :get-value-text (make-instance 'meta-level:translated-string :en "" :fr "" :de "" :sp "" :it "") :sql-length 0 :value-to-string-fn nil :nb-decimals 0 :void-link-text (make-instance 'meta-level:translated-string :en "" :fr "" :de "" :sp "" :it "") :can-create-new-object nil :create-new-object nil :process-new-object-fn 'nil :get-value-sql "" :html-tag-attributes 'nil :dont-display-null-value nil :view-type :default :slot-view-name 'nil)
                  (source-slot :value-type clipboard :user-name (make-instance 'meta-level:translated-string :en "" :fr "source-slot" :de "" :sp "" :it "") :description "" :object-help (make-instance 'meta-level::object-help :en "" :fr "" :de "" :sp "" :it "" :en-h "" :fr-h "" :de-h "" :sp-h "" :it-h "") :choices (list) :visible t :visible-groups 'nil :modifiable nil :modifiable-groups 'nil :stored nil :in-proxy nil :indexed nil :unique nil :null-allowed t :list-of-values nil :new-objects-first nil :linked-value nil :modifiable nil :duplicate-value t :make-copy-string nil :get-value-title (make-instance 'meta-level:translated-string :en "" :fr "" :de "" :sp "" :it "") :get-value-text (make-instance 'meta-level:translated-string :en "" :fr "" :de "" :sp "" :it "") :sql-length 0 :value-to-string-fn nil :nb-decimals 0 :void-link-text (make-instance 'meta-level:translated-string :en "" :fr "" :de "" :sp "" :it "") :can-create-new-object nil :create-new-object nil :process-new-object-fn 'nil :get-value-sql "" :html-tag-attributes 'nil :dont-display-null-value nil :view-type :default :slot-view-name 'nil)
                  (objects :value-type clipboard :user-name (make-instance 'meta-level:translated-string :en "Copied objects" :fr "Objets copiés" :de "" :sp "" :it "") :description "" :object-help (make-instance 'meta-level::object-help :en "" :fr "" :de "" :sp "" :it "" :en-h "" :fr-h "" :de-h "" :sp-h "" :it-h "") :choices (list) :visible t :visible-groups 'nil :modifiable nil :modifiable-groups 'nil :stored nil :in-proxy nil :indexed nil :unique nil :null-allowed t :list-of-values t :new-objects-first nil :linked-value t :modifiable nil :duplicate-value t :make-copy-string nil :get-value-title (make-instance 'meta-level:translated-string :en "" :fr "" :de "" :sp "" :it "") :get-value-text (make-instance 'meta-level:translated-string :en "" :fr "" :de "" :sp "" :it "") :sql-length 0 :value-to-string-fn nil :nb-decimals 0 :void-link-text (make-instance 'meta-level:translated-string :en "" :fr "" :de "" :sp "" :it "") :can-create-new-object nil :create-new-object nil :process-new-object-fn 'nil :get-value-sql "" :html-tag-attributes 'nil :dont-display-null-value nil :view-type :default :slot-view-name 'nil))
                 (:user-name
                  (make-instance 'meta-level:translated-string :en "Clipboard" :fr "Presse-papier" :de "" :sp "" :it "")
                  :guid
                  44565462735855467339436917756
                  :object-help
                  (make-instance 'meta-level::object-help :en "" :fr "" :de "" :sp "" :it "" :en-h "" :fr-h "" :de-h "" :sp-h "" :it-h "")
                  :functions
                  (list (make-instance 'meta-level::fc-function :name 'paste-from-user-clipboard :user-name (make-instance 'meta-level:translated-string :en "" :fr "paste-from-user-clipboard" :de "" :sp "" :it "") :visible nil :visible-groups 'nil :get-value-title (make-instance 'meta-level:translated-string :en "" :fr "" :de "" :sp "" :it "") :get-value-text (make-instance 'meta-level:translated-string :en "" :fr "" :de "" :sp "" :it "") :get-value-sql "" :html-tag-attributes 'nil :disable-predicate 'nil :object-help (make-instance 'meta-level::object-help :en "" :fr "" :de "" :sp "" :it "" :en-h "" :fr-h "" :de-h "" :sp-h "" :it-h "")) (make-instance 'meta-level::fc-function :name 'cut-to-user-clipboard :user-name (make-instance 'meta-level:translated-string :en "" :fr "cut-to-user-clipboard" :de "" :sp "" :it "") :visible nil :visible-groups 'nil :get-value-title (make-instance 'meta-level:translated-string :en "" :fr "" :de "" :sp "" :it "") :get-value-text (make-instance 'meta-level:translated-string :en "" :fr "" :de "" :sp "" :it "") :get-value-sql "" :html-tag-attributes 'nil :disable-predicate 'nil :object-help (make-instance 'meta-level::object-help :en "" :fr "" :de "" :sp "" :it "" :en-h "" :fr-h "" :de-h "" :sp-h "" :it-h "")) (make-instance 'meta-level::fc-function :name 'clear-user-clipboard :user-name (make-instance 'meta-level:translated-string :en "Empty the clipboard" :fr "Vider le presse-papier" :de "" :sp "" :it "") :visible t :visible-groups 'nil :get-value-title (make-instance 'meta-level:translated-string :en "" :fr "" :de "" :sp "" :it "") :get-value-text (make-instance 'meta-level:translated-string :en "" :fr "" :de "" :sp "" :it "") :get-value-sql "" :html-tag-attributes 'nil :disable-predicate 'nil :object-help (make-instance 'meta-level::object-help :en "" :fr "" :de "" :sp "" :it "" :en-h "" :fr-h "" :de-h "" :sp-h "" :it-h "")))
                  :visible
                  t
                  :visible-groups
                  'nil
                  :instanciable
                  t))

(defvar *clipboard-dest-context-item* nil)

(defun clear-clipboard (clipboard)
  (setf (operation clipboard) nil
	(objects clipboard) nil
	(source-object clipboard) nil
	(source-slot clipboard) nil))

(defun copy-to-clipboard (clipboard objects source-object source-slot)
  (setf (operation clipboard) :copy
        (objects clipboard) objects
        (source-object clipboard) source-object
        (source-slot clipboard) source-slot))

(defun cut-to-clipboard (clipboard objects source-object source-slot)
  (setf (operation clipboard) :cut
        (objects clipboard) objects
        (source-object clipboard) source-object
        (source-slot clipboard) source-slot))

(defmethod check-clipboard-compatibility (clipboard dest-object dest-slot)
  (and (objects clipboard) (source-slot clipboard) dest-slot
       (subtypep (meta::value-type (source-slot clipboard))(meta::value-type dest-slot))))

(defun remove-clipboard-cut-objects ()
  )

#+nil
(defun insert-clipboard-objects (dest-object dest-slot)
  (let ((objects (objects clipboard)))
    (funcall (fdefinition (list 'setf (meta::accessor dest-slot)))
	     (append (funcall (fdefinition (meta::accessor dest-slot)) dest-object)
		     (objects clipboard))
	     dest-object)))

(defun paste-clipboard (clipboard dest-object dest-slot)
  (when (check-clipboard-compatibility clipboard dest-object dest-slot)
    (let ((objects (objects clipboard)))
      (setf (objects clipboard) nil)
      (setf objects 
            (if (meta::linked-value dest-slot)
                (set-difference objects (funcall (fdefinition (meta::accessor dest-slot)) dest-object))
                (mapcar #'(lambda (obj)
                            (meta::duplicate-object-for-paste obj :parent dest-object)) objects)))
      (funcall (fdefinition (list 'setf (meta::accessor dest-slot)))
               (if (meta::new-objects-first dest-slot)
                   (append objects (funcall (fdefinition (meta::accessor dest-slot)) dest-object))
                   (append (funcall (fdefinition (meta::accessor dest-slot)) dest-object) objects))
               dest-object))
    (clear-clipboard clipboard)))

(defun clear-user-clipboard (clipboard)
  (clear-clipboard clipboard)
  (reload-interface))

(defclass clipboard-item (interface::html-item)
  ((container :accessor container :initform "cont" :initarg :container)
   #+nil(source-dispatcher :accessor source-dispatcher :initform nil)
   (display-nb-only :accessor display-nb-only :initform nil :initarg :display-nb-only)))

(defmethod interface::make-set-value-javascript ((item clipboard-item) value slot)
  (let* ((source-dispatcher (or (item-state *dispatcher*)
                                (gethash (and (container item)(name (container item)))
                                         (dispatchers *http-link*))))
         (source-item-name (when source-dispatcher (name (item source-dispatcher))))
         (nb-objects (length (funcall (get-value-fn *dispatcher*) (object *dispatcher*)))))
    (setf (item-state *dispatcher*) source-dispatcher)
    (html:fast-format nil "x_.fgt('~a').innerHTML='~a';" (interface::name item)
      (html:quote-javascript-string
       (html:html-to-string 
        (:if (display-nb-only item)
             (cond
               ((= (length (objects *object*)) 1)
                (html:html (:translate '(:fr ": 1 objet" :en ": 1 object"))))
               ((objects *object*)
                (html:html nb-objects (:translate '(:fr " objets" :en " objects"))))
               (t (html:html (:translate '(:fr ": Vide" :en ": Empty")))))
             (:when (and source-dispatcher
                         (check-clipboard-compatibility *object*
                                                        (object source-dispatcher) (slot source-dispatcher)))
               ((:a :format (:href "javascript:Fck('~a', f854('~a',-10));" 
                                   source-item-name source-item-name))
                ((:img :border "0" :src "/p.jpg" :width "22" :height "18" :alt "Paste")) :br
                ((:a :href (encode-object-url *object*))" " nb-objects
                 (:if (> nb-objects 1)
                      (:translate '(:fr " objets copiés" :en "copied objects"))
                      (:translate '(:fr " objet copié" :en "copied object"))))))))))))

(defmethod interface::make-set-status-javascript ((item clipboard-item) status slot)
  )

(defun clipboard-item-tag (attributes form)
  (destructuring-bind (slot-name . attrs) attributes
    (let ((slot (find (symbol-name slot-name) (clos:class-slots interface::*current-class*)
		      :test #'string= :key #'clos:slot-definition-name)))
      (unless slot (error (format nil "Slot inconnu : ~a" slot-name)))
      (let* ((item (make-instance 'clipboard-item :tooltip (meta::tooltip slot) :slot slot
                                  :container *root-item* :display-nb-only (getf attrs :display-nb-only))))
	`(html:html ((:span :id ,(interface::name item))) 
                    (setf (container ,item) *clipboard-dest-context-item*))))))

(html:add-func-tag :clipboard-item 'clipboard-item-tag)

;;; clipboard views

(make-instance 'interface::object-view :object-class 'clipboard :special-view t
	       :country-languages '(:fr :en) :name "clipbd" :source-code
 `(((:clipboard-item objects))))

(make-instance 'interface::object-view :object-class 'clipboard :special-view nil
	       :country-languages '(:fr :en) :name "showclip" :source-code
 `((:h1 (:translate '(:fr "Contenu du presse-papier" :en "clipboard content")))
   (if (objects *object*)
       (html:html
        (:p (:i "Ces objets ont été copiés à partir de la liste \""
                (:b (:esc (meta::translate (meta::user-name (source-slot *object*))))) "\" de l'objet :"
                ((:a :href (encode-object-url (source-object *object*)))
                 (:esc (meta::short-description (source-object *object*))))))
        (:slot-table objects)
        (:obj-fn-table clear-user-clipboard))
       (html:html
        (:p "Le presse-papier est vide")))))

;;; cliboard link

(make-instance 'interface::object-view :object-class 'clipboard :special-view t
	       :country-languages '(:fr :en) :name "cliplnk" :source-code
`(((:clipboard-item objects :display-nb-only t))))

(defun clipboard-link ()
  (html:html
   (:when *user*
     ((:a :href (encode-object-url (clipboard *user*)))
      (:translate '(:fr "Presse-papier" :en "Clipboard"))
      " " (:object-view :object (clipboard *user*) :name "cliplnk")))))

