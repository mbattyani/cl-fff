(in-package #:meta-web)

(defvar *page-number* nil)
(defvar *header* nil)
(defvar *footer* nil)
(defvar *margins* nil)
(defvar *index* nil)
(defparameter *color1* '(0.2 0.2 0.8))

(defun trans-string (&rest args)
  (tt::put-string (funcall 'meta::translate args)))

(defun draw-class-graph-legend (box x y)
  (tt::draw-block
   (tt::compile-text ()
     (tt::paragraph (:h-align :center :font "Helvetica"
			      :font-size 10 :color '(0 0 0))
		    (tt::colored-box :dx 9.0 :dy 9.0 :color '(0.7 1.0 0.7) :border-width 0.5)
		    " class, "
		    (tt::colored-box :dx 9.0 :dy 9.0 :color '(1.0 0.7 0.7) :border-width 0.5)
		    " super-classes, "
		    (tt::colored-box :dx 9.0 :dy 9.0 :color '(1.0 1.0 0.7) :border-width 0.5)
		     (trans-string :en " sub-classes, " :fr " sous-classes, ")
		    (tt::colored-box :dx 9.0 :dy 9.0 :color '(0.7 0.7 1.0) :border-width 0.5)
		    (trans-string :en " use the class, " :fr " utilise la classe, ")
		    (tt::colored-box :dx 9.0 :dy 9.0 :color '(0.7 1.0 1.0) :border-width 0.5)
		    (trans-string :en " used by the class." :fr " utilisé par la classe.")))
   x y (tt::dx box) (tt::dy box) :border 0.1 :v-align :center))

(defun edge-color (color)
  (list
   (min (max (+ (first color)  (* (random 2000) 0.0001) -0.1) 0) 1.0)
   (min (max (+ (second color)  (* (random 2000) 0.0001) -0.1) 0) 1.0)
   (min (max (+ (third color)  (* (random 2000) 0.0001) -0.1) 0) 1.0)))

(defun make-class-node-box (graph class-info class-type)
  (make-instance 'tt::graph-node :graph graph :dx 150 :data
		 (tt::make-filled-vbox
		  (tt::compile-text ()
  		     (tt::paragraph (:h-align :center :font "Helvetica-Oblique"
					      :font-size 13 :color '(0 0 0))
				    (tt::put-string (name class-info))
				    :eol
				    (tt::with-style (:font "Times-Italic" :font-size 10)
				      (tt::put-string (french (user-name class-info))))))
		  150 tt::+huge-number+)
		 :background-color (case class-type
				     (:start-class '(0.7 1.0 0.7))
				     (:to-class '(0.7 0.7 1.0))
				     (:from-class '(0.7 1.0 1.0))
				     (:super-class '(1.0 0.7 0.7))
				     (:sub-class '(1.0 1.0 0.7))
				     (t '(1.0 1.0 1.0)))))

(defun gen-class-graph-layout (project classes depth &optional (max-dx 440) (max-dy 550))
  (let* ((g (make-instance 'tt::graph
			   :dot-attributes '(("rankdir" "LR")("nodesep" "0.2")("ranksep" "0.5"))
			   :max-dx max-dx :max-dy max-dy :border-width nil))
	 (nodes (make-hash-table))
	 (all-classes (get-project-classes project))
	 (new-classes classes)
	 (next-new-classes ()))
    (flet ((add-class-node (class type)
	     (unless (gethash class nodes)
	       (setf (gethash class nodes)(make-class-node-box g class type)))))
      (dolist (class classes)
	(add-class-node class :start-class))
      (loop while new-classes
	repeat depth do
	(loop for class in all-classes
	      unless (member class classes) do
	      (loop for super in (direct-superclasses class) do
		    (when (member super new-classes)
		      (add-class-node class :sub-class)
		      (pushnew class next-new-classes)
		      (pushnew class classes)))
	      (loop for slot in (direct-slots class)
		    for object-type = (object-type slot) do
		    (when (and (eq (value-type slot) :object)
			       (member object-type new-classes))
		      (add-class-node class :to-class)
		      (pushnew class next-new-classes)
		      (pushnew class classes))))
	(loop for class in new-classes do
	      (loop for super in (direct-superclasses class) do
		    (unless (member super classes)
		      (add-class-node super :super-class)
		      (pushnew super next-new-classes)
		      (pushnew super classes)))
	      (loop for slot in (direct-slots class)
		    for object-type = (object-type slot) do
		    (when (and (eq (value-type slot) :object)
			       (not (member object-type classes)))
		      (add-class-node object-type :from-class)
		      (pushnew object-type next-new-classes)
		      (pushnew object-type classes))))
	(setf new-classes next-new-classes
	      next-new-classes ()))
;	    (old-classes ()))
	(dolist (class classes)
;	  (push class old-classes)
	  (let ((members-by-value-class (make-hash-table)))
	    (loop for super in (direct-superclasses class) do
		  (when (member super classes)
		    (make-instance 'tt::graph-edge  :graph g
				   :head (gethash super nodes) :tail (gethash class nodes)
				   :label "sub-class" :label-color '(0.0 0.0 0.6)
				   :color (edge-color '(0.7 0.3 0.3)) :width 2)))
	    (loop for slot in (direct-slots class)
		  for object-type = (object-type slot) do
		  (when (and (eq (value-type slot) :object)(member object-type classes))
		    (push slot (gethash object-type members-by-value-class))))
	    (maphash #'(lambda(val-class slots)
			 (make-instance 'tt::graph-edge  :graph g
					:head (gethash class nodes) :tail (gethash val-class nodes)
					:label (if (< (length slots) 3)
						   (format nil "~{~a~^\\n~}" (mapcar 'name slots))
						   (format nil "~{~a~^\\n~}..." (mapcar 'name (subseq slots 0 2))))
					:label-color '(0.0 0.0 0.0)
					:color (edge-color '(0.3 0.3 0.7)) :width 2))
		     members-by-value-class)))
	(when (> (hash-table-count nodes) 8)
	  (setf (tt::max-dx g) 660
		(tt::max-dy g) 440
		(tt::landscape-layout g) t))
	(tt::compute-graph-layout g)
	g)))

(defun draw-file-graph-legend (box x y)
  (tt::draw-block
   (tt::compile-text ()
     (tt::paragraph (:h-align :center :font "Helvetica"
			      :font-size 10 :color '(0 0 0))
		    (tt::colored-box :dx 9.0 :dy 9.0 :color '(0.7 1.0 0.7) :border-width 0.5)
		    (trans-string :en " User file, " :fr " Fichier utilisateur, ")
		    (tt::colored-box :dx 9.0 :dy 9.0 :color '(1.0 0.7 0.7) :border-width 0.5)
		    (trans-string :en " Generated file" :fr " Fichier généré, ")))
   x y (tt::dx box) (tt::dy box) :border 0.1 :v-align :center))

(defun make-file-node-box (graph source-file)
  (make-instance 'tt::graph-node :graph graph :dx 200 :data
		 (tt::make-filled-vbox
		  (tt::compile-text ()
		    (tt::paragraph (:h-align :center :font "Helvetica-Oblique"
					     :font-size 16 :color '(0 0 0))
				   (tt::put-string (name source-file))
				   :eol
				   (tt::with-style (:font "Times-Italic" :font-size 12)
				     (tt::put-string (description source-file)))
				   (tt::vspace 5)))
		  200 tt::+huge-number+)
		 :background-color (if (generated source-file)
				       (edge-color '(1.0 0.7 0.7))
				       (edge-color '(0.7 1.0 0.7)))))

(defun gen-file-graph-layout (project &optional (max-dx 660) (max-dy 450))
  (let* ((g (make-instance 'tt::graph
			   :dot-attributes '(("rankdir" "LR")("nodesep" "0.2")("ranksep" "0.9"))
			   :max-dx max-dx :max-dy max-dy :border-width 0
			   :background-color '(0.8 0.8 0.8) :landscape-layout t))
	 (nodes (make-hash-table)))
    (flet ((get-file-node (file)
	     (if (gethash file nodes)
		 (gethash file nodes)
		 (let ((node (make-file-node-box g file)))
		   (setf (gethash file nodes) node)
		   node))))
      (loop for file in (files project)
	    for node = (get-file-node file) do
	    (loop for dep-file in (dependances file)
		  for dep-node = (get-file-node dep-file) do
		  (make-instance 'tt::graph-edge :graph g
				 :head dep-node :tail node
				 :color (edge-color '(0.3 0.3 0.7)) :width 2)))
      (tt::compute-graph-layout g)
      g)))

(defun draw-doc-wavelet-rule (box x0 y0)
  (let ((dx/2 (* (tt::dx box) 0.5))
	(dy/2 (* (tt::dy box) 0.5)))
    (pdf:with-saved-state
      (pdf:translate (+ x0 dx/2) (- y0 dy/2))
      (pdf:set-line-width 1)
      (pdf:set-color-stroke (tt::color box))
      (pdf:move-to (- dx/2) 0)
      (loop for x from (- dx/2) by 0.2
	    for y = (* dy/2 (cos (* x 0.6)) (exp (* (expt (sin (* x 0.1)) 2) -0.04))(exp (* x x -0.00004)))
	    while (< x dx/2)
	    do (pdf:line-to x y))
      (pdf:stroke))))


(defun put-name-val (name value)
  (tt:row ()
   (tt:cell () (tt::with-style (:font "Helvetica" :font-size 10.9)
		 (tt::put-string name)))
   (tt:cell () (tt::with-style (:font "Helvetica" :font-size 10.9)
		 (tt::put-string (if value
				     (format nil "~a" value)
				     ""))))))

(defun name-value-table (title list)
  (tt:table (:col-widths '(130 320) :border 0.1  :splittable-p t)
	    (tt::header-row ()
		    (tt:cell (:col-span 2 :background-color '(0.6 0.6 0.9))
			     (tt::with-style (:font "Helvetica-Bold" :font-size 14)
			       (tt::put-string title))))
	    (loop for (name value) on list by 'cddr
		  do (put-name-val name value))))

(defun yes-no (bool)
  (if bool "oui" "non"))

(defun visible-string (obj)
  (if (visible obj) "tous"
      (format nil "~{~a~^, ~}"
	      (mapcar 'name (visible-groups obj)))))

(defun modifiable-string (obj)
  (if (modifiable obj) "tous"
      (format nil "~{~a~^, ~}"
	      (mapcar 'name (modifiable-groups obj)))))

(defmethod gen-doc-content ((fn function-info))
  (push (list (format nil "~a (fonction de ~a)" (string-downcase (name fn))
		      (name (meta::parent fn))) fn) *index*)
  (let* ((tt::*pp-font-size* 12)
	(content
	 (tt::compile-text ()
   	    (tt::mark-ref-point fn)
	    (tt:paragraph (:font "Helvetica-Bold" :font-size 16 :top-margin 20)
			  "Fonction : " (tt::put-string (name fn)))
	    (tt:hrule :dy 2)
	    (tt::vspace 10)
	    (tt:paragraph (:font "Helvetica-Bold" :font-size 16 :top-margin 20)
		(name-value-table (format nil "Caractéristiques générales de la fonction ~a" (name fn))
				  (append
				   (list
				    "Nom" (name fn)
				    "Nom français" (french (user-name fn))
				    "Nom anglais" (english (user-name fn))
				    "Description" (description fn)
				    "Commentaire" (comment fn)
				    "Visible par" (visible-string fn)))))
	    (when (disable-predicate fn)
	      (tt:paragraph (:font "Helvetica-Bold" :font-size 12 :top-margin 10 :bottom-margin 5)
			    "Prédicat pour désactivation :")
	      (tt::process-lisp-code (safe-read-from-string (disable-predicate fn))))
	    :fresh-page)))
    (tt::draw-pages content :margins *margins* :header *header* :footer *footer*)))

;DESCRIPTION DETAILLEE D'UN ATTRIBUT (SLOT) :
(defmethod gen-doc-content ((slot slot-info))
  (push (list (format nil "~a (slot de ~a)" (string-downcase (name slot))
		      (name (meta::parent slot))) slot) *index*)
  (let* (*(tt::*pp-font-size* 12)
	(content
	 (tt::compile-text ()
	 (tt::mark-ref-point slot)
	 (tt:paragraph (:font "Helvetica-Bold" :font-size 16 :top-margin 0)
	     "Slot : " (tt::put-string (name slot)))
	 (tt:hrule :dy 2)
	 (tt:paragraph (:font "Helvetica-Bold" :font-size 16 :top-margin 10)
	     (name-value-table (format nil "Caractéristiques générales du slot ~a" (name slot))
	       (append
		(list
		 "Nom de l'attribut" (name slot)
		 "Nom français" (french (user-name slot))
		 "Nom anglais" (english (user-name slot))
		 "Nom dans la table SQL" (meta::convert-name-to-sql-name (or (sql-name slot) (name slot)))
		 "Description" (description slot)
		 "Commentaire" (comment slot)
		 "Type" (meta::translated-choice-value 'value-type slot))
	       (when (eq (value-type slot) :object)
		 (list "Type d'objet" (name (object-type slot))
		       "Texte FR si vide" (french (void-link-text slot))
		       "Texte EN si vide" (english (void-link-text slot))
		       "Peut créer nouvel obj." (yes-no (can-create-new-object slot))
		       "Créer objet valeur" (yes-no (create-new-object slot))
		       "Fn pour obtenir obj." (get-object-fn slot)
		       "Fn à appeler sur new obj." (process-new-object-fn slot)
		       "Titre FR pour dialog box de sélection" (french (get-value-title slot))
		       "Titre EN pour dialog box de sélection" (english (get-value-title slot))
		       "Texte FR pour dialog box de sélection" (french (get-value-text slot))
		       "Texte EN pour dialog box de sélection" (english (get-value-text slot))
		       "Fn de génération HTML" (get-value-html-fn slot)
		       ))
	       (when (eq (value-type slot) :other)
		 (list "Autre type" (other-type slot)))
	       (list
		 "Enregistré dans base" (yes-no (stored slot))
		 "Dans proxy" (yes-no (in-proxy slot))
		 "Indexé" (yes-no (indexed slot))
		 "Unique" (yes-no (unique slot))
		 "Ne pas afficher nuls" (yes-no (dont-display-null-value slot))
		 "Valeur par défaut" (initform slot)
		 "Unité" (unit slot)
		 "Visible par" (visible-string slot)
		 "Modifiable par" (modifiable-string slot)
		 "Liste d'objets" (yes-no (list-of-values slot))
		 )
	       (when (list-of-values slot)
		 (list "Nouveaux objets en haut" (yes-no (new-objects-first slot))))
	       (list
		 "Type de vue" (meta::translated-choice-value 'value-type slot)
		 "Attribut HTML" (html-tag-attributes slot)
		 "Dupl. valeur si copie" (yes-no (duplicate-value slot))
		 "Ajouter \"Copie de\"" (yes-no (duplicate-value slot))
		 "Fn pour dupliquer valeur" (duplicate-value-fn slot)
		 )
		)))
	 (when (choices slot)
	   (tt::vspace 20)
	   (tt::table (:col-widths '(100 175 175) :splittable-p t)
		     (tt::header-row ()
			 (tt::cell (:col-span 3 :background-color '(0.6 0.6 0.9))
				   (tt::paragraph (:font "Helvetica-Bold" :font-size 14)
						  "Liste des valeurs possibles")))
		     (tt::header-row (:background-color '(0.7 1.0 0.7))
			 (tt::cell ()(tt::paragraph () "Valeur"))
			 (tt::cell ()(tt::paragraph () "Nom FR"))
			 (tt::cell ()(tt::paragraph () "Nom EN")))
		     (dolist (choice (choices slot))
		       (tt::row ()
			   (tt::cell ()(tt::paragraph (:font "Helvetica" :font-size 11)
						      (tt::put-string (format nil "~a" (choice-value choice)))))
			   (tt::cell ()(tt::paragraph (:font "Helvetica" :font-size 11)
						      (tt::put-string (french (name choice)))))
			   (tt::cell ()(tt::paragraph (:font "Helvetica" :font-size 11)
						      (tt::put-string (english (name choice)))))))))
	 (when (disable-predicate slot)
	   (tt:paragraph (:font "Helvetica-Bold" :font-size 12 :top-margin 10)
			 "Prédicat pour désactivation :")
	   (tt::process-lisp-code (safe-read-from-string (disable-predicate slot))))
	 (when (value-constraint slot)
	   (tt:paragraph (:font "Helvetica-Bold" :font-size 12 :top-margin 10)
			 "Contrainte pour la valeur du slot :")
	   (tt::process-lisp-code (safe-read-from-string (value-constraint slot))))
	 :fresh-page)))
    (tt::draw-pages content :margins *margins* :header *header* :footer *footer*)))

;DESCRIPTION DETAILLEE D'UNE CLASSE :
(defmethod gen-doc-content ((class class-info))
  (push (list (format nil "~a (classe)" (string-downcase (name class))) class) *index*)
  (let* ((real-class (gethash class *class-info-to-class*))
	 (*class-doc* (format nil ", classe ~a" (name class)))
	 (content
	 (tt::compile-text ()
          (tt:set-contextual-variable :classe (format nil " (classe ~a)" (string-downcase (name class))))
	  (tt::mark-ref-point class)
	  (tt:paragraph (:font "Helvetica-Bold" :font-size 16 :top-margin 20)
			"Class : " (tt::put-string (name class)))
	  (tt:hrule :dy 2)
	  (tt::vspace 10)
	  (tt:paragraph (:font "Helvetica-Bold" :font-size 16 :top-margin 20)
		(name-value-table (format nil "Caractéristiques générales de la classe ~a" (name class))
			(list
			 "Nom français" (french (user-name class))
			 "Nom anglais" (english (user-name class))
			 "Nom de la table SQL" (meta::sql-name real-class)
			 "Description" (description class)
			 "Commentaire" (comment class)
			 "class GUID" (guid class)
			 "Version" (version class)
			 "Instanciable" (yes-no (instanciable class))
			 "Stockée en RAM" (yes-no (use-memory-store class))
			 "Hérite des classes" (format nil "~{~a~^, ~}"
						      (mapcar 'name (direct-superclasses class)))
			 "Visible par" (visible-string class)
			 "Description courte" (if (short-description class)
						  (short-description class)
						  "voir sources et super-classes")
			 )))
	  (tt::vspace 3)
	  :hfill (tt::graph-box (gen-class-graph-layout (project class) (list class) 1)) :hfill
	  (tt::vspace 10)
	  (tt::user-drawn-box :dx 400 :dy 12 :stroke-fn 'draw-class-graph-legend) :eol
	  (tt::paragraph (:h-align :center :font "Times-Italic" :font-size 14 :top-margin 6)
			 (trans-string :en "Graph of neighbor classes" :fr "Graphe des classes voisines."))
	  (when (direct-slots class)
	    (tt::vspace 20)
	    (tt::table (:col-widths '(100 120 30 170 30) :splittable-p t)
		     (tt::header-row ()
				     (tt::cell (:col-span 5 :background-color '(0.7 1.0 0.7))
					       (tt::paragraph (:font "Helvetica-Bold" :font-size 14)
							      "Liste des attributs (slots) directs")))
		     (tt::header-row (:background-color '(0.7 1.0 0.7))
			 (tt::cell ()(tt::paragraph () "Nom"))
			 (tt::cell ()(tt::paragraph () "Type"))
			 (tt::cell ()(tt::paragraph () "Liste"))
			 (tt::cell ()(tt::paragraph () "Description"))
			 (tt::cell ()(tt::paragraph () "Page")))
		     (dolist (slot (direct-slots class))
		       (tt::row ()
				(tt::cell ()(tt::paragraph (:font "Helvetica" :font-size 11)
							   (tt::put-string (name slot))))
				(tt::cell ()(tt::paragraph (:font "Helvetica" :font-size 11)
						(tt::put-string (meta::translated-choice-value 'value-type slot))
						(when (eq (value-type slot) :object)
						  (tt::put-string (format nil " (~a)" (name (object-type slot)))))))
				(tt::cell ()(tt::paragraph (:font "Helvetica" :font-size 11)
						(tt::put-string (yes-no (list-of-values slot)))))
				(tt::cell ()(tt::paragraph (:font "Helvetica-Oblique" :font-size 10)
						(tt::put-string (description slot))))
				(tt::cell ()(tt::paragraph (:font "Helvetica-Oblique" :font-size 10)
						(tt::format-string "~d" (tt::find-ref-point-page-number slot))))))))
	  (dolist (super (direct-superclasses class))
	    (when (direct-slots super)
	      (tt::vspace 20)
	      (tt::table (:col-widths '(100 120 30 170 30) :splittable-p t)
		     (tt::header-row ()
			     (tt::cell (:col-span 5 :background-color '(1.0 0.7 0.7))
				       (tt::paragraph (:font "Helvetica-Bold" :font-size 14)
					 (tt::put-string (format nil "Liste des attributs hérités de ~a"
								 (name super))))))
		     (tt::header-row (:background-color '(1.0 0.7 0.7))
			 (tt::cell ()(tt::paragraph () "Nom"))
			 (tt::cell ()(tt::paragraph () "Type"))
			 (tt::cell ()(tt::paragraph () "Liste"))
			 (tt::cell ()(tt::paragraph () "Description"))
			 (tt::cell ()(tt::paragraph () "Page")))
		     (dolist (slot (direct-slots super))
		       (tt::row ()
				(tt::cell ()(tt::paragraph (:font "Helvetica" :font-size 11)
							   (tt::put-string (name slot))))
				(tt::cell ()(tt::paragraph (:font "Helvetica" :font-size 11)
						(tt::put-string (meta::translated-choice-value 'value-type slot))
						(when (eq (value-type slot) :object)
						  (tt::put-string (format nil " (~a)" (name (object-type slot)))))))
				(tt::cell ()(tt::paragraph (:font "Helvetica" :font-size 11)
							   (tt::put-string (yes-no (list-of-values slot)))))
				(tt::cell ()(tt::paragraph (:font "Helvetica-Oblique" :font-size 10)
							   (tt::put-string (description slot))))
				(tt::cell ()(tt::paragraph (:font "Helvetica-Oblique" :font-size 10)
						(tt::format-string "~d" (tt::find-ref-point-page-number slot)))))))))
	  (when (direct-functions class)
	    (tt::vspace 20)
	    (tt::table (:col-widths '(100 150 170 30) :splittable-p t)
		     (tt::header-row ()
			 (tt::cell (:col-span 4 :background-color '(0.7 1.0 0.7))
				   (tt::paragraph (:font "Helvetica-Bold" :font-size 14)
						  "Liste des fonctions directes")))
		     (tt::header-row (:background-color '(0.7 1.0 0.7))
			 (tt::cell ()(tt::paragraph () "Nom"))
			 (tt::cell ()(tt::paragraph () "Nom FR"))
			 (tt::cell ()(tt::paragraph () "Visible par"))
			 (tt::cell ()(tt::paragraph () "Page")))
		     (dolist (fonc (direct-functions class))
		       (tt::row ()
			   (tt::cell ()(tt::paragraph (:font "Helvetica" :font-size 11)
						      (tt::put-string (name fonc))))
			   (tt::cell ()(tt::paragraph (:font "Helvetica" :font-size 11)
						      (tt::put-string (french (user-name fonc)))))
			   (tt::cell ()(tt::paragraph (:font "Helvetica" :font-size 11)
						      (tt::put-string (visible-string fonc))))
			   (tt::cell ()(tt::paragraph (:font "Helvetica-Oblique" :font-size 10)
						(tt::format-string "~d" (tt::find-ref-point-page-number fonc))))))))
	  (dolist (super (direct-superclasses class))
	    (when (direct-functions super)
	      (tt::vspace 20)
	      (tt::table (:col-widths '(100 150 170 30) :splittable-p t)
		     (tt::header-row ()
			 (tt::cell (:col-span 4 :background-color '(1.0 0.7 0.7))
				   (tt::paragraph (:font "Helvetica-Bold" :font-size 14)
					(tt::put-string (format nil "Liste des fonctions héritées de ~a"
								(name super))))))
		     (tt::header-row (:background-color '(1.0 0.7 0.7))
			 (tt::cell ()(tt::paragraph () "Nom"))
			 (tt::cell ()(tt::paragraph () "Nom FR"))
			 (tt::cell ()(tt::paragraph () "Visible par"))
			 (tt::cell ()(tt::paragraph () "Page")))
		     (dolist (fonc (direct-functions super))
		       (tt::row ()
			   (tt::cell ()(tt::paragraph (:font "Helvetica" :font-size 11)
						      (tt::put-string (name fonc))))
			   (tt::cell ()(tt::paragraph (:font "Helvetica" :font-size 11)
						      (tt::put-string (french (user-name fonc)))))
			   (tt::cell ()(tt::paragraph (:font "Helvetica" :font-size 11)
						      (tt::put-string (visible-string fonc))))
			   (tt::cell ()(tt::paragraph (:font "Helvetica-Oblique" :font-size 10)
						(tt::format-string "~d" (tt::find-ref-point-page-number fonc)))))))))
	  (tt::vspace 10)
	  (tt:paragraph (:font "Helvetica-Bold" :font-size 14 :top-margin 20)
			"Description des tables SQL pour la classe "
			(tt::put-string (name class)))
	  (tt::vspace 10)
	  (tt::paragraph (:h-align :left :top-margin 15
				   :left-margin 5 :right-margin 5 :font "courier" :font-size 11)
			 (tt::verbatim
			  (meta::gen-sql-create-table real-class))
			 (loop for slot in (c2mop:class-slots real-class)
			       when (meta::stored slot) do
			       (tt::vspace 10)
			       (tt::verbatim
				(meta::gen-slot-aux-tables-sql real-class slot))))
	  :fresh-page)))
    (tt::draw-pages content :margins *margins* :header *header* :footer *footer*)
    (dolist (slot (direct-slots class))
      (gen-doc-content slot))
    (dolist (fn (direct-functions class))
      (gen-doc-content fn))))


;DESCRIPTION DETAILLEE D'UN GROUPE DE CLASSE :
(defmethod gen-doc-content ((group class-group))
  (let ((content
	 (tt::compile-text ()
	  (tt::mark-ref-point group)
	  (tt:paragraph (:font "Helvetica-Bold" :font-size 16 :top-margin 20)
			"Groupe de classes: " (tt::put-string (name group)))
	  (tt:hrule :dy 2)
	  (tt::vspace 10)
	  (tt:paragraph (:font "Helvetica-Bold" :font-size 16 :top-margin 20)
			(name-value-table "Caractéristiques générales"
			  (list
			   "Nom" (name group)
			   "Nom FR" (french (user-name group))
			   "Description" (description group)
			   "Commentaire" (comment group)
			   "Nombre de classes" (length (classes group))
			   )))
	  (tt::vspace 10)
	  (tt::table (:col-widths '(100 315 35) :splittable-p t)
		 (tt::header-row ()
		     (tt::cell (:col-span 3 :background-color '(0.6 0.6 0.9))
			 (tt::paragraph () "Liste des classes")))
		 (tt::header-row (:background-color '(0.6 0.6 0.9))
		     (tt::cell ()(tt::paragraph () "Nom"))
		     (tt::cell ()(tt::paragraph () "Description"))
		     (tt::cell ()(tt::paragraph () "Page")))
		 (dolist (class (classes group))
		   (tt::row ()
			(tt::cell ()(tt::paragraph (:font-size 11) (tt::put-string (name class))))
			(tt::cell ()(tt::paragraph (:font-size 11) (tt::put-string (description class))))
			(tt::cell ()(tt::paragraph (:font-size 11)
			       (tt::format-string "~d" (tt::find-ref-point-page-number class)))))))
	  (tt::vspace 20)
	  :fresh-page)))
    (tt::draw-pages content :margins *margins* :header *header* :footer *footer*)

    (dolist (class (classes group))
      (gen-doc-content class))))

(defmethod gen-doc-content ((file source-file))
  (push (list (format nil "~a (classe)" (name file)) file) *index*)
  (let ((content
	 (tt::compile-text ()
	  (tt::mark-ref-point file)
	  (tt:paragraph (:font "Helvetica-Bold" :font-size 16 :top-margin 20)
			"Fichier source: " (tt::put-string (name file)))
	  (tt:hrule :dy 2)
	  (tt:paragraph (:font "Helvetica-Bold" :font-size 12 :top-margin 20)
			"Description:")
	  (tt:paragraph (:font "helvetica-oblique" :font-size 12)
			(tt::put-string (description  file)))
	  (tt:paragraph (:font "Helvetica-Bold" :font-size 12
			       :top-margin 30 :bottom-margin 10)
			"Source:")
	  (typeset::process-lisp-code
	   (pathname (concatenate 'string
				  (sources-directory (meta::parent file))
				  (name file))))
	  :fresh-page)))
    (tt::draw-pages content :margins *margins* :header *header* :footer *footer*)))

;DESCRIPTION GENERALE DU PROJET :
(defmethod gen-doc-content ((project project))
  (let ((content
	 (tt::compile-text
	  ()
	  (tt::paragraph (:h-align :center)
	     (tt::with-style (:font "Helvetica" :font-size 70 :color *color1*)
	       :vfill
	       (tt::put-string (meta::translate (name project))) :eol
	       (tt::vspace 10)
	       (tt::hrule :dy 30 :stroke-fn 'draw-doc-wavelet-rule :color *color1*)
	       (tt::vspace 5))
	     (tt::with-style (:font "Times-Italic" :font-size 40 :color *color1*)
	       (trans-string :en "Technical Documentation" :fr "Documentation technique") :vfill))
	     :eop
	     (tt:paragraph (:font "Helvetica-Bold" :font-size 16 :top-margin 20)
			   (trans-string :en "Table of Contents" :fr "Table des matières"))
	     (tt:hrule :dy 2)
	     (tt::vspace 20)
	     (tt::paragraph (:h-align :fill :font "Helvetica" :font-size 16 :color '(0.0 0 0.4)
				  :left-margin 25 :right-margin 25)
			(trans-string :fr "Description de la documentation"
				      :en "Description of the documentation") (tt::dotted-hfill)
			(tt::format-string "~d" (tt::find-ref-point-page-number :help)))
	     (tt::vspace 10)
	     (tt::paragraph (:h-align :fill :font "Helvetica" :font-size 16 :color '(0.0 0 0.4)
				  :left-margin 25 :right-margin 25)
			"Paramètres généraux" (tt::dotted-hfill)
			(tt::format-string "~d" (tt::find-ref-point-page-number project)))
	     (tt::vspace 10)
	     (tt::paragraph (:h-align :left :font "Helvetica" :font-size 16 :color '(0.0 0 0.4)
				  :left-margin 25 :right-margin 25)
			"Goupes de classes")
	     (tt::vspace 10)
	     (dolist (group (class-groups project))
	       (tt::paragraph (:h-align :fill :font "Helvetica" :font-size 14 :color '(0.0 0 0.4)
				    :left-margin 25 :right-margin 25)
			  (tt::format-string "Groupes de classes ~a" (name group)) (tt::dotted-hfill)
			  (tt::format-string "~d" (tt::find-ref-point-page-number group)))
	       (dolist (class (classes group))
		 (tt::paragraph (:h-align :fill :font "Helvetica" :font-size 12 :color '(0.0 0 0.4)
				      :left-margin 45 :right-margin 25)
			    (tt::format-string "Classe ~a" (name class)) (tt::dotted-hfill)
			    (tt::format-string "~d" (tt::find-ref-point-page-number class)))
		 (tt::vspace 5)
		 (dolist (slot (direct-slots class))
		   (tt::paragraph (:h-align :fill :font "Helvetica" :font-size 10 :color '(0.0 0 0.4)
					:left-margin 65 :right-margin 25)
			      (tt::format-string "Slot ~a" (name slot)) (tt::dotted-hfill)
			      (tt::format-string "~d" (tt::find-ref-point-page-number slot))))
		 (tt::vspace 5)
		 (dolist (fn (direct-functions class))
		   (tt::paragraph (:h-align :fill :font "Helvetica" :font-size 10 :color '(0.0 0 0.4)
					:left-margin 65 :right-margin 25)
			      (tt::format-string "Fonction ~a" (name fn)) (tt::dotted-hfill)
			      (tt::format-string "~d" (tt::find-ref-point-page-number fn))))
		 (tt::vspace 10))
	       (tt::vspace 10))
	     (tt::paragraph (:h-align :fill :font "Helvetica" :font-size 16 :color '(0.0 0 0.4)
				  :left-margin 25 :right-margin 25)
			    "Fichiers sources" )
	     (tt::vspace 10)
	     (dolist (file (files project))
	       (tt::paragraph (:h-align :fill :font "Helvetica" :font-size 12 :color '(0.0 0 0.4)
				    :left-margin 65 :right-margin 25)
			  (tt::format-string "Fichier ~a" (name file)) (tt::dotted-hfill)
			  (tt::format-string "~d" (tt::find-ref-point-page-number file))))
	     (tt::vspace 10)
	     (tt::paragraph (:h-align :fill :font "Helvetica" :font-size 14 :color '(0.0 0 0.4)
				      :left-margin 45 :right-margin 25)
			    "Graphe de dépendance des fichiers sources" (tt::dotted-hfill)
			    (tt::format-string "~d" (tt::find-ref-point-page-number :file-graph)))
	     (tt::vspace 20)
	     (tt::paragraph (:h-align :fill :font "Helvetica" :font-size 16 :color '(0.0 0 0.4)
				      :left-margin 25 :right-margin 25)
			    "Index" (tt::dotted-hfill)
			    (tt::format-string "~d" (tt::find-ref-point-page-number :index)))
	     (tt::vspace 20)
	     (tt::paragraph (:h-align :fill :font "Helvetica" :font-size 16 :color '(0.0 0 0.4)
				      :left-margin 25 :right-margin 25)
			    "Fin" (tt::dotted-hfill)
			    (tt::format-string "~d" (tt::find-ref-point-page-number :the-end)))
	     (when (other-documents project)
	       (tt::vspace 20)
	       (tt::paragraph (:h-align :fill :font "Helvetica" :font-size 16 :color '(0.0 0 0.4)
					:left-margin 25 :right-margin 25)
			      "Annexes")
	       (tt::vspace 10)
	       (dolist (doc (other-documents project))
		 (tt::paragraph (:h-align :fill :font "Helvetica" :font-size 12 :color '(0.0 0 0.4)
					  :left-margin 35 :right-margin 25)
				(tt::put-string (title doc))
				(when (page-number doc)
				  (tt::dotted-hfill)
				  (tt::put-string (page-number doc))))))

	     :eop
             :hfill (tt::graph-box (gen-class-graph-layout project (mapcan #'(lambda (x) (copy-list (classes x))) (class-groups project)) 1)) :hfill
	     (if (eq meta::*country-language* :fr)
		 (french-doc-help project)
		 (english-doc-help project))
	     (tt::vspace 10)
	     (tt::mark-ref-point project)
	     (tt:paragraph (:h-align :left :font "Helvetica-Bold" :font-size 16 :top-margin 20 :color *color1*)
			   "Projet : " (tt::put-string (name project)))
	     (tt::vspace 3)
	     (tt:hrule :dy 2 :color *color1*)
	     (tt:paragraph (:font "Helvetica-Bold" :font-size 16 :top-margin 20)
			   (name-value-table "Caractéristiques générales"
					     (list
					      "Nom" (name project)
					      "Version" (format nil "~d" (version project))
					      "Description" (description project)
					      "Package" (project-package project)
					      "Nombre de classes" (format nil "~d"
									(reduce '+ (class-groups project)
									  :key #'(lambda (g) (length (classes g)))))
					      "Nombre de groupes d'utilisateurs" (format nil "~d" (length (user-groups project)))
					      )))
	     (tt::vspace 20)
;********************
	     (tt::table (:col-widths '(100 315 35) :splittable-p t)
			(tt::header-row ()
		     (tt::cell (:col-span 3 :background-color '(0.6 0.6 0.9))
			 (tt::paragraph () "Liste des groupes de classes")))
		 (tt::header-row (:background-color '(0.6 0.6 0.9))
		     (tt::cell ()(tt::paragraph () "Nom"))
;		     (tt::cell ()(tt::paragraph () "Nom FR"))
		     (tt::cell ()(tt::paragraph () "Description"))
		     (tt::cell ()(tt::paragraph () "Page")))
		 (dolist (class-group (class-groups project))
		   (when (print-in-doc class-group)
		     (tt::row ()
		      (tt::cell ()(tt::paragraph (:font-size 11)
					 (tt::put-string (name class-group))))
;			(tt::cell ()(tt::paragraph () (tt::put-string (french (user-name class-group)) )))
		      (tt::cell ()(tt::paragraph (:font "Helvetica-Oblique" :font-size 11)
				   (tt::put-string (description class-group))))
			(tt::cell ()(tt::paragraph (:font-size 11)
			       (tt::format-string "~d" (tt::find-ref-point-page-number class-group))))))))
	     (tt::vspace 20)
;********************
	  (tt::table (:col-widths '(100 350) :splittable-p t)
	     (tt::header-row ()
		     (tt::cell (:col-span 2 :background-color '(0.6 0.6 0.9))
		       (tt::paragraph () "Liste des groupes d'utilisateurs")))
	     (tt::header-row (:background-color '(0.6 0.6 0.9))
		     (tt::cell ()(tt::paragraph () "Nom"))
		     (tt::cell ()(tt::paragraph () "Description")))
	    (dolist (group (user-groups project))
	      (tt::row ()
 	         (tt::cell ()(tt::paragraph (:font-size 11) (tt::put-string (name group))))
		 (tt::cell ()(tt::paragraph (:font "Helvetica-Oblique" :font-size 11)
					    (tt::put-string (description group))))
		 )))
	  (tt::vspace 20)
;********************
	  (tt::table (:col-widths '(100 315 35) :splittable-p t)
	     (tt::header-row ()
		     (tt::cell (:col-span 3 :background-color '(0.6 0.6 0.9))
			       (tt::paragraph () "Liste des fichiers sources")))
	     (tt::header-row (:background-color '(0.6 0.6 0.9))
		     (tt::cell ()(tt::paragraph () "Nom"))
		     (tt::cell ()(tt::paragraph () "Description"))
		     (tt::cell ()(tt::paragraph () "Page")))
	    (dolist (file (files project))
	      (tt::row ()
 	         (tt::cell ()(tt::paragraph (:font-size 11) (tt::put-string (name file))))
		 (tt::cell ()(tt::paragraph (:font "Helvetica-Oblique" :font-size 11)
					    (tt::put-string (description file))))
		 (tt::cell ()(tt::paragraph (:font-size 11)
			       (tt::format-string "~d" (tt::find-ref-point-page-number file)))))))
	  :eop
	  (tt::mark-ref-point :file-graph)
	  :hfill (tt::graph-box (gen-file-graph-layout project)) :hfill
	  (tt::vspace 10)
	  (tt::user-drawn-box :dx 200 :dy 12 :stroke-fn 'draw-file-graph-legend) :eol
	  (tt::paragraph (:h-align :center :font "Times-Italic" :font-size 16 :top-margin 6)
			 (trans-string :en "File dependency graph." :fr "Graphe des dépendances de fichier."))
;********************
	  :eop)))
    (tt::draw-pages content :margins *margins* :header *header* :footer *footer*))

  (dolist (group (class-groups project))
    (when (print-in-doc group)
      (gen-doc-content group)))

  (when (print-source-files project)
    (dolist (file (files project))
      (when (print-in-doc file)
	(gen-doc-content file))))

  (when pdf:*page* (tt::finalize-page pdf:*page*))
  (setf *index* (remove-duplicates (sort *index* #'string<= :key #'first) :test #'equal))
  (let ((content
	 (tt::compile-text ()
	   (tt::mark-ref-point :index)
	   (tt:paragraph (:font "Times-Italic" :font-size 100 :top-margin 20 :color '(0.0 0 0.4))
			 "Index")
	   (tt:vspace 20)
	   (loop with letter = nil
		 for (name obj) in *index* do
		 (unless (eql (aref name 0) letter)
		   (setf letter (aref name 0))
		   (tt::paragraph (:h-align :left :font "Times-Italic" :font-size 80
					    :color '(0.0 0 0.4)
					    :left-margin 5 :right-margin 5
					    :top-margin 20 :bottom-margin 10)
				  (tt::format-string "~c" (char-upcase letter))))
		 (tt::paragraph (:h-align :fill :font "Helvetica" :font-size 10 :color '(0.0 0 0.4)
					  :left-margin 5 :right-margin 5)
				(tt::put-string name) (tt::dotted-hfill)
				(tt::format-string "~d" (tt::find-ref-point-page-number obj)))))))
    (loop while (tt::boxes content) do
	  (pdf:with-page ()
	    (tt::draw-block content 20 800 257.5 700)
	    (when (tt::boxes content)
	      (tt::draw-block content (+ 40 257.5) 800 257.5 700)))))
    (let ((content
	   (tt::compile-text ()
	       (tt::mark-ref-point :the-end)
	       (tt::paragraph (:h-align :center)
		   (tt::with-style (:font "Times-Italic" :font-size 80 :color *color1*)
		     :vfill
		     (tt::put-string "Fin") :eol
		     (tt::vspace 10)
		     (tt::hrule :dy 30 :stroke-fn 'draw-doc-wavelet-rule :color *color1*)
		     (tt::vspace 5)))
	       :vfill
	       :eop)))
      (tt::draw-pages content :margins *margins* :header *header* :footer *footer*))
    (when pdf:*page* (tt::finalize-page pdf:*page*)))



(defun gen-doc (project)
  (create-project-classes project)
  (let ((file (format nil "~a~a-documentation.pdf"
		      (or (and (> (length (sources-directory project)) 0)
			       (sources-directory project))
			  "/tmp/")
		      (name project)))
	(*margins* '(72 72 72 50))
	(pdf::*max-number-of-pages* 2000)
	(*index* nil)
	(*package* (ensure-package (project-package project)))
        (meta::*country-language* :fr))
    (tt::with-document ()
      (let* ((print-stamp (multiple-value-bind (second minute hour date month year)
			      (decode-universal-time (version-date project))
			    (format nil #T(:en "Version ~a of ~2,'0D-~2,'0D-~4D ~2,'0D:~2,'0D:~2,'0D"
					   :fr "Version ~a du ~2,'0D-~2,'0D-~4D ~2,'0D:~2,'0D:~2,'0D")
				    (project-version project)
				    date month year hour minute second)))
	     (*header* #'(lambda(pdf:*page*)
			   (tt::compile-text ()
			      (tt:paragraph (:h-align :center :color '(0 0 0)
						      :font "Helvetica-BoldOblique" :font-size 12)
					    (tt::put-string (meta::translate (name project)))
					    (trans-string :fr ": Documentation technique"
							  :en ": Technical Documentation")
					    (tt::put-string (tt:get-contextual-variable :classe)))
			      (tt:hrule :dy 0.5))))
	     (*footer* (lambda (pdf:*page*)
		       (tt::compile-text
			(:font "Helvetica" :font-size 10 :color '(0 0 0))
			(tt:hrule :dy 1/2)
			(tt:hbox (:align :center :adjustable-p t)
			      (tt:verbatim print-stamp)
					;(hspace 100)
			      :hfill
			      (tt:verbatim
			       (format nil "Page ~d/~d"
				       pdf:*page-number*
				       (tt::find-ref-point-page-number :the-end))))))))
	(gen-doc-content project)
	(when (tt::final-pass-p)
	  (pdf:write-document file))))))

(defun french-doc-help (project)
  (tt::with-text-compilation
    (tt:paragraph (:h-align :justified :font "Helvetica-Bold" :font-size 16 :top-margin 20)
		  "Description de la documentation")
    (tt::mark-ref-point :help)
    (tt:hrule :dy 2)
    (tt:paragraph (:h-align :justified :font "Helvetica-Bold" :font-size 14 :top-margin 20)
		  "Préambule")
    (tt:paragraph (:h-align :justified :font "Helvetica-Oblique" :font-size 12 :top-margin 15)
		  "Le projet " (tt::put-string (name project)) " est basé sur le modèle classique des serveurs web applicatifs (modèle des trois tiers). Il se compose d'une base de données SQL (PostgreSQL), d'un framework applicatif (Plateforme applicative de Fractal Concept) et d'un serveur web (Apache 1.3). Le logiciel est utilisé par des postes clients légers (Internet Explorer). Cette documentation ne contient que les éléments spécifiques à l'application. Elle ne contient pas les documentations de la base de données, de la plateforme applicative et du serveur web.")
    (tt:paragraph (:h-align :justified :font "Helvetica-Bold" :font-size 14 :top-margin 20)
	 	  "Description")
    (tt:paragraph (:h-align :justified :font "Helvetica" :font-size 12 :top-margin 15)
		  "Cette documentation contient la description de l'intégralité des éléments applicatifs constituant le projet "
		  (tt::put-string (name project))"." :eol
		  (tt::vspace 10)
		  "Ces éléments sont:")
    (tt:paragraph (:h-align :justified :font "Helvetica" :font-size 12 :left-margin 10 :top-margin 10)
		  "Les paramètres généraux du projet: Nom, description, version, date, groupes d'utilisateurs, groupes de classes, liste des fichiers sources avec leur description et le graphe des dépendance des fichiers entre eux, etc."
		  )
    (tt:paragraph (:h-align :justified :font "Helvetica" :font-size 12 :left-margin 10 :top-margin 10)
		  "Les groupes de classes d'objets avec leur listes de classes."
		  )
    (tt:paragraph (:h-align :justified :font "Helvetica" :font-size 12 :left-margin 10 :top-margin 10)
		  "Les classes d'objets avec, pour chacune des classes, leurs propriétés, le graphes des classes ayant un rapport avec cette classe, (super-classes, sous-classes, classes utilisées par et classes utilisant cette classe) et la description des slots (attributs) directs et hérités pour cette classe."
		  )
    (tt:paragraph (:h-align :justified :font "Helvetica" :font-size 12 :left-margin 10 :top-margin 10)
		  "Les définitions SQL des tables utilisées dans la base de données PostgresQL pour le stockage des données."
		  )
    (tt:paragraph (:h-align :justified :font "Helvetica" :font-size 12 :left-margin 10 :top-margin 10)
		  "Les descriptions détailleés des slots des objets avec toutes leurs propriétés, les contraintes sur les valeurs, les prédicats de désactivation, etc."
		  )
    (tt:paragraph (:h-align :justified :font "Helvetica" :font-size 12 :left-margin 10 :top-margin 10)
		  "Les fichiers sources sont inclus dans leur intégralité avec une colorisation syntaxique (keywords en rouge, commentaires en italique bleu, symboles standard en bleu foncé, etc.)"
		  )
    (tt:paragraph (:h-align :justified :font "Helvetica" :font-size 12 :left-margin 10 :top-margin 10)
		  "Un index permet de retrouver tous les noms des classes, des slots (avec leur classe) et des fonctions (aussi avec leur classe)."
		  )
    (tt:paragraph (:h-align :justified :font "Helvetica" :font-size 12 :left-margin 10 :top-margin 10)
		  "Les autres documents tels que les cahiers des charges et les descriptions fonctionnelles sont aussi inclus à la fin de cette documentation ou joints en tant que documents séparés."
		  )

    (tt:paragraph (:h-align :justified :font "Helvetica-Bold" :font-size 14 :left-margin 0 :top-margin 20)
		  "Définition des propriétés des slots.")
    (tt:paragraph (:h-align :justified :font "Helvetica" :font-size 12 :left-margin 0 :top-margin 10)
		  "Les slots sont décrits dans une table de propriété du type de celle ci-dessous. Dans cette table les valeurs sont remplacées par une description de ces propriétés."
		  )

    (tt:paragraph (:font "Helvetica-Bold" :font-size 16 :top-margin 10)
		  (name-value-table (format nil "Caractéristiques générales d'un slot ")
	    (append
	     (list
	      "Nom de l'attribut" "Nom informatique du slot (fonction accessor)"
	      "Nom français" "Nom français présenté à l'utilisateur"
	      "Nom anglais" "Nom anglais présenté à l'utilisateur"
	      "Nom dans la table SQL" "Le nom de la colone SQL dans la table associée à l'objet"
	      "Description" "La description du slot"
	      "Commentaire" "Les commentaires sur ce slot"
	      "Type" "Le type Lisp du slot"
	      "Type d'objet" "La classe du l'objet (si le type du slot est objet)"
	      "Texte FR si vide" "Le texte français à mettre en l'abscence d'objet (si le type du slot est objet)"
	      "Texte EN si vide" "Le texte anglais à mettre en l'abscence d'objet (si le type du slot est objet)"
	      "Peut créer nouvel obj." "l'utilisateur habilité peut créer un nouvel objet dans ce slot (si le type du slot est objet)"
	      "Créer objet valeur" "Le système cré automatiquement l'objet contenu dans ce slot (si le type du slot est objet)"
	      "Fn pour obtenir obj." "La fonction à appeller pour obtenir l'objet à mettre dans ce slot (si le type du slot est objet)"
	      "Fn à appeler sur new obj." "La fonction à appeller (si elle est précisée) pour traiter l'objet retourné par la fonction précédente. La valeur retournée par cette fonction est mise dans le slot. (si le type du slot est objet)"
	      "Titre FR pour dialog box de sélection" "Le titre français à mettre sur la dialog box de choix de l'objet à mettre dans le slot (si le type du slot est objet)"
	      "Titre EN pour dialog box de sélection" "Le titre anglais à mettre sur la dialog box de choix de l'objet à mettre dans le slot (si le type du slot est objet)"
	      "Texte FR pour dialog box de sélection" "Le texte français à mettre dans la dialog box de choix de l'objet à mettre dans le slot (si le type du slot est objet)"
	      "Texte EN pour dialog box de sélection" "Le texte anglais à mettre dans la dialog box de choix de l'objet à mettre dans le slot (si le type du slot est objet)"
	      "Fn de génération HTML" "La fontion à appeler pour générer l'interface HTML de choix de l'objet à mettre dans le slot (si le type du slot est objet)"
	      "Autre type" "Le nom du type Lisp de la valeur à mettre dans le slot (si le type du slot est autre type"
	      "Enregistré dans base" "La valeur du slot doit être stockée dans la base de données"
	      "Dans proxy" "La valeur du slot doit être stockée dans le proxy d'interface entre l'objet et sa représentation dans la base de données."
	      "Indexé" "La colonne représentant le slot dans la base de donnée SQL doit être indexée"
	      "Unique" "La colonne représentant le slot dans la base de donnée SQL doit être unique"
	      "Ne pas afficher nuls" "Les valeurs nulles ne seront pas affichées (le champ sera vide dans l'interface)"
	      "Valeur par défaut" "La valeur par défaut du slot au moment de la création d'un objet"
	      "Unité" "L'unité de la valeur contenue dans le slot"
	      "Visible par" "La liste des groupes d'utilisateurs habilités à voir ce slot"
	      "Modifiable par" "La liste des groupes d'utilisateurs habilités à voir ce slot"
	      "Liste d'objets" "Le slot contient une liste de valeurs"
	      "Nouveaux objets en haut" "Les nouvelles valeurs sont rajoutées en début de liste (si le slot est une liste)"
	      "Type de vue" "Le type de vue HTML utilisée pour représenter le slot dans l'interface"
	      "Attribut HTML" "Les attributs HTML à utiliser pour la représentation HTML de l'objet"
	      "Dupl. valeur si copie" "En cas de copie de l'objet, la valeur du slot doit être dupliquée"
	      "Ajouter \"Copie de\"" "En cas de copie de l'objet rajouter \"Copie de\" devant la valeur contenue dans le slot (valable pour les strings seulement)"
	      "Fn pour dupliquer valeur" "La fonction à utiliser pour dupliquer la valeur contenue dans le slot"))))

    (tt:paragraph (:h-align :justified :font "Helvetica" :font-size 12 :left-margin 0 :top-margin 10)
		  "La description du slot comporte aussi éventuellement la liste des valeurs possibles ainsi que le prédicat pour désactivation (code source Lisp) et la contrainte sur la valeur du slot (code source Lisp).")
    :fresh-page))

(defun english-doc-help (project)
  (french-doc-help project))

(defun make-mcd-entity-node (class-info class-type)
  (list class-info class-type))

(defun make-mcd-relation-node (name class1 class2 nb1 nb2)
  (list name class1 class2 nb1 nb2))

(defvar %super-classes% ())

(defun collect-super-classes (class)
  (let ((%super-classes% nil))
    (%collect-super-classes% class)
    %super-classes%))

(defun %collect-super-classes% (class)
  (loop for super in (direct-superclasses class) do
        (unless (member super %super-classes%)
          (push super %super-classes%)
          (%collect-super-classes% super))))

(defun collect-all-slots (class)
  (let ((super-classes (collect-super-classes class))
        (slots ()))
    (pushnew class super-classes)
    (loop for class in super-classes do
          (loop for slot in (direct-slots class) do
                (pushnew slot slots :key 'name :test 'equal)))
    slots))

(defun gen-mcd-graph (project classes depth &optional (max-dx 440) (max-dy 550))
  (setf classes (remove-if-not 'instanciable classes))
  (let* ((entities (make-hash-table))
	 (relations ())
	 (all-classes (get-project-classes project))
	 (new-classes classes)
	 (next-new-classes ()))
    (flet ((add-entity-node (class type)
	     (unless (gethash class entities)
	       (setf (gethash class entities)(list class type (symbol-name (gensym "N"))))))
           (add-relation-node (slot class1 class2 nb1 nb2)
	       (push (list slot class1 class2 nb1 nb2 (symbol-name (gensym "N"))(symbol-name (gensym "N"))) relations)))
      (dolist (class classes)
	(add-entity-node class :start-class))
      (loop while new-classes
            repeat depth do
            (loop for class in new-classes do
                  (loop for slot in (collect-all-slots class)
                        for object-type = (object-type slot) do
                        (when (eq (value-type slot) :object)
                          (when (not (member object-type classes))
                            (add-entity-node object-type :from-class)
                            (push object-type next-new-classes)
                            (push object-type classes))
                          (add-relation-node slot class object-type 0 0))))
            (setf new-classes next-new-classes
                  next-new-classes ()))
      (append (list 's-dot::graph '((s-dot::label "MCD")(s-dot::ratio "1.0")(s-dot::rankdir "LR")(s-dot::fontname "Arial")))
              (loop for class in classes
                    collect (list 's-dot::node
                                  (list (list 's-dot::id (third (gethash class entities)))
                                        (list 's-dot::label (name class))
                                        '(s-dot::height "1.0")
                                        '(s-dot::fontname "Arial")
                                        '(s-dot::shape "box")
                                        '(s-dot::style "filled"))))
              (loop for (slot class1 class2 nb1 nb2 id id1) in relations
                    collect (list 's-dot::node
                                  (list (list 's-dot::id id)
                                        (list 's-dot::label (name slot))
                                        '(s-dot::fontname "Arial")
                                        '(s-dot::shape "diamond")
                                        '(s-dot::style "filled"))))
              (loop for (slot class1 class2 nb1 nb2 id id1) in relations
                    collect (list 's-dot::edge
                                  (list (list 's-dot::from (third (gethash class1 entities)))
                                        (list 's-dot::to id)
                                        '(s-dot::labeldistance "3.0")
                                        '(s-dot::headport "w")
;                                        '(s-dot::tailport "e")
                                        '(s-dot::arrowhead "none")
                                        (if (null-allowed slot)
                                            '(s-dot::arrowtail "odottee")
                                          '(s-dot::arrowtail "tee"))
                                        #+nil
                                        (if (null-allowed slot)
                                            '(s-dot::taillabel "0..1")
                                          '(s-dot::taillabel "1"))))
                    collect (list 's-dot::edge (list (list 's-dot::from id)
                                              (list 's-dot::to (third (gethash class2 entities)))
                                              '(s-dot::labeldistance "3.0")
;                                              '(s-dot::headport "w")
                                              '(s-dot::tailport "e")
                                              (if (list-of-values slot)
                                                  '(s-dot::arrowhead "crowodot")
                                                '(s-dot::arrowhead "tee"))
                                              #+nil
                                              (if (list-of-values slot)
                                                  '(s-dot::headlabel "0..N")
                                                '(s-dot::headlabel "1"))
                                              ))))
#+nil
      (append (list 's-dot::graph '((s-dot::label "MCD Physique")(s-dot::ratio "1.0")(s-dot::rankdir "LR")
                                    (s-dot::fontname "Arial")))
              (loop for class in classes collect
                    (list* 's-dot::record '((s-dot::fontname "Arial"))
                           (list 's-dot::node
                                 (list (list 's-dot::id (symbol-name (gensym "N")))
                                       (list 's-dot::label (meta::convert-name-to-sql-name (name class)))
                                       '(s-dot::shape "box")
                                       '(s-dot::style "filled")))
                           (list 's-dot::node
                                 (list (list 's-dot::id (third (gethash class entities)))
                                       (list 's-dot::label "id")
                                       '(s-dot::shape "box")
                                       '(s-dot::style "filled")))
                           (loop for (slot class1 class2 nb1 nb2 id id1) in relations
                                 when (and (eql class1 class) (not (list-of-values slot))) collect
                                 (list 's-dot::node
                                       (list (list 's-dot::id id1)
                                             (list 's-dot::label (meta::convert-name-to-sql-name (name slot)))
                                             '(s-dot::shape "box")
                                             '(s-dot::style "filled"))))))
              (loop for (slot class1 class2 nb1 nb2 id id1) in relations
                    when (list-of-values slot) collect
                    (list 's-dot::record '((s-dot::fontname "Arial"))
                          (list 's-dot::node
                                (list (list 's-dot::id id)
                                      (list 's-dot::label (meta::convert-name-to-sql-name
                                                           (format nil "~a_~a" (name class1)(name slot))))
                                      '(s-dot::shape "box")
                                      '(s-dot::style "filled")))
                          (list 's-dot::node
                                (list (list 's-dot::id (format nil "~a1" id))
                                      (list 's-dot::label "parentid")
                                      '(s-dot::shape "box")
                                      '(s-dot::style "filled")))
                          (list 's-dot::node
                                (list (list 's-dot::id (format nil "~a2" id))
                                      (list 's-dot::label "id")
                                      '(s-dot::shape "box")
                                      '(s-dot::style "filled")))))
              (loop for (slot class1 class2 nb1 nb2 id id1) in relations
                    when (not (list-of-values slot)) collect
                    (list 's-dot::edge
                          (list (list 's-dot::from (format nil (if (eql class1 class2) "~a:w" "~a:e") id1))
                                (list 's-dot::to (format nil "~a:w" (third (gethash class2 entities))))
                                '(s-dot::labeldistance "3.0")
                                '(s-dot::headport "w")
                                (if (eql class1 class2)
                                    '(s-dot::tailport "w")
                                    '(s-dot::tailport "e"))
                                '(s-dot::arrowhead "normal")
                                '(s-dot::arrowtail "none")
                                #+nil
                                (if (null-allowed slot)
                                    '(s-dot::arrowtail "odottee")
                                  '(s-dot::arrowtail "tee"))
                                #+nil
                                (if (list-of-values slot)
                                    '(s-dot::arrowhead "crowodot")
                                  '(s-dot::arrowhead "tee"))
                                #+nil
                                (if (null-allowed slot)
                                    '(s-dot::taillabel "0..1")
                                  '(s-dot::taillabel "1"))))
                    when (list-of-values slot) collect
                    (list 's-dot::edge
                          (list (list 's-dot::from (format nil "~a:e" (third (gethash class1 entities))))
                                (list 's-dot::to (format nil "~a1:w" id))
                                '(s-dot::labeldistance "3.0")
                                '(s-dot::headport "w")
                                '(s-dot::tailport "e")
                                '(s-dot::arrowhead "none")
                                '(s-dot::arrowtail "normal")
                                #+nil
                                (if (null-allowed slot)
                                    '(s-dot::arrowtail "odottee")
                                  '(s-dot::arrowtail "tee"))
                                #+nil
                                (if (null-allowed slot)
                                    '(s-dot::taillabel "0..1")
                                  '(s-dot::taillabel "1"))))
                    when (list-of-values slot) collect
                    (list 's-dot::edge
                          (list (list 's-dot::from (format nil (if (eql class1 class2) "~a2:w" "~a2:e") id))
                                (list 's-dot::to (format nil (if (eql class1 class2) "~a:e" "~a:w")
                                                         (third (gethash class2 entities))))
                                '(s-dot::labeldistance "3.0")
                                '(s-dot::tailport "e")
                                '(s-dot::headport "w")
                                #+nil
                                (if (list-of-values slot)
                                    '(s-dot::arrowhead "crowodot")
                                  '(s-dot::arrowhead "tee"))
                                #+nil
                                (if (list-of-values slot)
                                    '(s-dot::headlabel "0..N")
                                  '(s-dot::headlabel "1"))
                                )))))))

(defclass mcd-entity ()
  (
   (node-id :accessor node-id :initform (symbol-name (gensym "N")))
   (class-info :accessor class-info :initform nil)
;   (parent-class :accessor parent-class :initform nil)
;   (parent-slot :accessor parent-slot :initform nil)
   (out-relations :accessor out-relations :initform nil)
   (in-relations :accessor in-relations :initform nil)
   ))

(defclass mcd-relations ()
  (
   (node-id :accessor node-id :initform (symbol-name (gensym "N")))
   (slot-info :accessor slot-info :initform nil)
   (from-entity :accessor out-slots :initform nil)
   (to-entity :accessor in-slots :initform nil)
   ))

(defun gen-mcd-graph-detail (project classes depth &optional (max-dx 440) (max-dy 550))
  (setf classes (remove-if-not 'instanciable classes))
  (let* ((entities (make-hash-table))
	 (relations ())
	 (all-classes (get-project-classes project))
	 (new-classes classes)
	 (next-new-classes ()))
    (loop for class in all-classes
          for entity1 = (make-instance 'mcd-entity)
          do
          (setf (gethash class entities) entity1
                )
          (loop for slot in (collect-all-slots class)
                for object-type = (object-type slot) do
                (when (eq (value-type slot) :object)
                  (when (not (member object-type classes))
                    (add-entity-node object-type :from-class)
                    (push object-type next-new-classes)
                    (push object-type classes))
                  (add-relation-node slot class object-type 0 0))))

    (flet ((add-entity-node (class type)
	     (unless (gethash class entities)
	       (setf (gethash class entities)(list class type (symbol-name (gensym "N"))))))
           (add-relation-node (slot class1 class2 nb1 nb2)
	       (push (list slot class1 class2 nb1 nb2 (symbol-name (gensym "N"))) relations)))
      (dolist (class classes)
	(add-entity-node class :start-class))
      (loop while new-classes
            repeat depth do
            (loop for class in new-classes do
                  (loop for slot in (collect-all-slots class)
                        for object-type = (object-type slot) do
                        (when (eq (value-type slot) :object)
                          (when (not (member object-type classes))
                            (add-entity-node object-type :from-class)
                            (push object-type next-new-classes)
                            (push object-type classes))
                          (add-relation-node slot class object-type 0 0))))
            (setf new-classes next-new-classes
                  next-new-classes ()))
      (append (list 's-dot::graph '((s-dot::label "MCD")(s-dot::ratio "1.0")(s-dot::rankdir "LR")))
              (loop for class in classes
                    collect (list 's-dot::node
                                  (list (list 's-dot::id (third (gethash class entities)))
                                        (list 's-dot::label (name class))
                                        '(s-dot::shape "box")
                                        '(s-dot::style "filled"))))
              (loop for (slot class1 class2 nb1 nb2 id) in relations
                    collect (list 's-dot::node
                                  (list (list 's-dot::id id)
                                        (list 's-dot::label (name slot))
                                        '(s-dot::shape "diamond")
                                        '(s-dot::style "filled"))))
              (loop for (slot class1 class2 nb1 nb2 id) in relations
                    collect (list 's-dot::edge
                                  (list (list 's-dot::from (third (gethash class1 entities)))
                                        (list 's-dot::to id)
                                        '(s-dot::labeldistance "3.0")
                                        '(s-dot::headport ":w")
                                        '(s-dot::arrowhead "none")
                                        (if (null-allowed slot)
                                            '(s-dot::arrowtail "odottee")
                                          '(s-dot::arrowtail "tee"))
                                        #+nil
                                        (if (null-allowed slot)
                                            '(s-dot::taillabel "0..1")
                                          '(s-dot::taillabel "1"))))
                    collect (list 's-dot::edge (list (list 's-dot::from id)
                                              (list 's-dot::to (third (gethash class2 entities)))
                                              '(s-dot::labeldistance "3.0")
                                              '(s-dot::tailport ":e")
                                              (if (list-of-values slot)
                                                  '(s-dot::arrowhead "crowodot")
                                                '(s-dot::arrowhead "tee"))
                                              #+nil
                                              (if (list-of-values slot)
                                                  '(s-dot::headlabel "0..N")
                                                '(s-dot::headlabel "1"))
                                              )))))))

(defun gen-mcd-graph-detail (project classes depth &optional (max-dx 440) (max-dy 550))
  (setf classes (remove-if-not 'instanciable classes))
  (let* ((entities (make-hash-table))
	 (relations ())
	 (all-classes (get-project-classes project))
	 (new-classes classes)
	 (next-new-classes ()))
    (loop for class in all-classes
          for entity1 = (make-instance 'mcd-entity)
          do
          (setf (gethash class entities) entity1
                )
          (loop for slot in (collect-all-slots class)
                for object-type = (object-type slot) do
                (when (eq (value-type slot) :object)
                  (when (not (member object-type classes))
                    (add-entity-node object-type :from-class)
                    (push object-type next-new-classes)
                    (push object-type classes))
                  (add-relation-node slot class object-type 0 0))))

    (flet ((add-entity-node (class type)
	     (unless (gethash class entities)
	       (setf (gethash class entities)(list class type (symbol-name (gensym "N"))))))
           (add-relation-node (slot class1 class2 nb1 nb2)
	       (push (list slot class1 class2 nb1 nb2 (symbol-name (gensym "N"))) relations)))
      (dolist (class classes)
	(add-entity-node class :start-class))
      (loop while new-classes
            repeat depth do
            (loop for class in new-classes do
                  (loop for slot in (collect-all-slots class)
                        for object-type = (object-type slot) do
                        (when (eq (value-type slot) :object)
                          (when (not (member object-type classes))
                            (add-entity-node object-type :from-class)
                            (push object-type next-new-classes)
                            (push object-type classes))
                          (add-relation-node slot class object-type 0 0))))
            (setf new-classes next-new-classes
                  next-new-classes ()))
      (append (list 's-dot::graph '((s-dot::label "MCD")(s-dot::ratio "1.0")(s-dot::rankdir "LR")))
              (loop for class in classes
                    collect (list 's-dot::node
                                  (list (list 's-dot::id (third (gethash class entities)))
                                        (list 's-dot::label (name class))
                                        '(s-dot::shape "box")
                                        '(s-dot::style "filled"))))
              (loop for (slot class1 class2 nb1 nb2 id) in relations
                    collect (list 's-dot::node
                                  (list (list 's-dot::id id)
                                        (list 's-dot::label (name slot))
                                        '(s-dot::shape "diamond")
                                        '(s-dot::style "filled"))))
              (loop for (slot class1 class2 nb1 nb2 id) in relations
                    collect (list 's-dot::edge
                                  (list (list 's-dot::from (third (gethash class1 entities)))
                                        (list 's-dot::to id)
                                        '(s-dot::labeldistance "3.0")
                                        '(s-dot::headport ":w")
                                        '(s-dot::arrowhead "none")
                                        (if (null-allowed slot)
                                            '(s-dot::arrowtail "odottee")
                                          '(s-dot::arrowtail "tee"))
                                        #+nil
                                        (if (null-allowed slot)
                                            '(s-dot::taillabel "0..1")
                                          '(s-dot::taillabel "1"))))
                    collect (list 's-dot::edge (list (list 's-dot::from id)
                                              (list 's-dot::to (third (gethash class2 entities)))
                                              '(s-dot::labeldistance "3.0")
                                              '(s-dot::tailport ":e")
                                              (if (list-of-values slot)
                                                  '(s-dot::arrowhead "crowodot")
                                                '(s-dot::arrowhead "tee"))
                                              #+nil
                                              (if (list-of-values slot)
                                                  '(s-dot::headlabel "0..N")
                                                '(s-dot::headlabel "1"))
                                              )))))))
