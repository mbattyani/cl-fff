(in-package meta-web)

(defvar *header* nil)
(defvar *footer* nil)
(defvar *margins* nil)
(defparameter *color1* '(0.2 0.2 0.8))


(defun edge-color (color)
  (list 
   (+ (first color) (* (random 2000) 0.0002) -0.0001)
   (+ (second color) (* (random 2000) 0.0002) -0.0001)
   (+ (third color) (* (random 2000) 0.0002) -0.0001)))

(defun make-class-node-box (graph class-info class-type)
  (make-instance 'tt::graph-node :graph graph :dx 70 :data
		 (tt::make-filled-vbox
		  (tt::compile-text ()
  		     (tt::paragraph (:h-align :center :font "Helvetica-Oblique"
					      :font-size 12 :color '(0 0 0))
				    (tt::put-string (name class-info))
				    :eol
				    (tt::with-style (:font "Times-Italic" :font-size 9)
				      (tt::put-string (description class-info)))))
		  70 tt::+huge-number+)
		 :background-color (case class-type
				     (:start-class '(0.7 1.0 0.7))
				     (:to-class '(0.7 0.7 1.0))
				     (:from-class '(0.7 1.0 1.0))
				     (:super-class '(1.0 0.7 0.7))
				     (:sub-class '(1.0 1.0 0.7))
				     (t '(1.0 1.0 1.0)))))

(defun gen-class-graph-layout (project classes depth)
  (let* ((g (make-instance 'tt::graph
			   :dot-attributes '(("rankdir" "LR")("nodesep" "0.3")("ranksep" "0.8"))
			   :max-dx 440 :max-dy 550 :border-width nil))
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
      (let ((members-by-value-class (make-hash-table))
	    (old-classes ()))
	(dolist (class classes)
	  (push class old-classes)
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


;DESCRIPTION DETAILLEE D'UN ATTRIBUT (SLOT) :
(defmethod gen-doc-content ((slot slot-info))
  (let ((content
	 (tt::compile-text
	 ()
	 (tt:paragraph (:font "Helvetica-Bold" :font-size 16 :top-margin 20)
	     "Slot : " (tt::put-string (name slot)))
	 (tt:hrule :dy 2)
	 (tt::vspace 10)
	 (tt:paragraph (:font "Helvetica-Bold" :font-size 16 :top-margin 20)
	     (name-value-table "Caractéristiques générales"
		(list
		 "Nom du champ" (french (user-name slot))
		 "Description" (description slot)
		 "Commentaire" (comment slot)
		 "Dans proxy " (if (in-proxy slot) "oui" "non")
		 "enregistré dans la base " (if (stored slot) "oui" "non")
		 "Visible par" (if (visible slot) "tous"
				   (dolist (user (visible-groups slot))
				     (tt::put-string (name user)) " "))
		 )))
	 (tt::vspace 10)
	 :eop)))
    (tt::draw-pages content :margins *margins* :header *header* :footer *footer*)
    ))

;DESCRIPTION DETAILLEE D'UNE CLASSE :
(defmethod gen-doc-content ((class class-info))
  (let* ((real-class (gethash class *class-info-to-class*))
	 (content
	 (tt::compile-text
	  ()
	  (tt:paragraph (:font "Helvetica-Bold" :font-size 16 :top-margin 20)
			"Class : " (tt::put-string (name class)))
	  (tt:hrule :dy 2)
	  (tt::vspace 10)
	  (tt:paragraph (:font "Helvetica-Bold" :font-size 16 :top-margin 20)
		(name-value-table "Caractéristiques générales"
			(list
			 "Nom français" (french (user-name class))
			 "Nom anglais" (english (user-name class))
			 "Nom de la table SQL" (meta::sql-name real-class)
			 "Description" (description class)
			 "Commentaire" (comment class)
			 "Hérite des classes" (dolist (info (direct-superclasses class))
						(tt::put-string (name info)) " ")
			 "Visible par" (if (visible class)
					   "tous"
					   (dolist (user (visible-groups class))
					     (tt::put-string (name user)) " "))
			 "Description courte" (if (short-description class)
						  (short-description class)
						  "voir sources et super-classes")
			 )))
	  (tt::vspace 20)
	  :hfill (tt::graph-box (gen-class-graph-layout (project class) (list class) 1)) :hfill
	  (tt::paragraph (:h-align :center :font "Times-Italic" :font-size 11)
			 "Graphe des classes voisines.")
	  (tt::vspace 20)
	  (tt::table (:col-widths '(100 150 200) :splittable-p t)
		     (tt::header-row ()
				     (tt::cell (:col-span 3 :background-color '(0.6 0.6 0.9))
					       (tt::paragraph () "Liste résumée des attributs directs (slots)")))
		     (tt::header-row (:background-color '(0.6 0.6 0.9))
			 (tt::cell ()(tt::paragraph () "Nom"))
			 (tt::cell ()(tt::paragraph () "Type"))
			 (tt::cell ()(tt::paragraph () "Description")))
		     (dolist (slot (direct-slots class))
		       (tt::row ()
				(tt::cell ()(tt::paragraph () (tt::put-string (name slot))))
				(tt::cell ()(tt::paragraph ()
						(tt::put-string (meta::translated-choice-value 'value-type slot))))
				(tt::cell ()(tt::paragraph () (tt::put-string (french (user-name slot))))))))
	  (tt::vspace 10)
	  (tt::table (:col-widths '(100 150 200) :splittable-p t)
		     (tt::header-row ()
			 (tt::cell (:col-span 3 :background-color '(0.6 0.6 0.9))
				   (tt::paragraph () "Liste résumée des fonctions")))
		     (tt::header-row (:background-color '(0.6 0.6 0.9))
			 (tt::cell ()(tt::paragraph () "Nom"))
			 
			 (tt::cell ()(tt::paragraph () "Description"))
			 (tt::cell ()(tt::paragraph () "Visible par")))
		     (dolist (fonc (direct-functions class))
		       (tt::row ()
			   (tt::cell ()(tt::paragraph () (tt::put-string (name fonc))))
			   (tt::cell ()(tt::paragraph () (tt::put-string (french (user-name fonc)))))
			   (tt::cell ()(tt::paragraph () (if (visible fonc) "tous"
							     (dolist (user (visible-groups fonc))
							       (tt::put-string (name user)) " ")))))))
	  (tt::vspace 10)
	  (tt:paragraph (:font "Helvetica-Bold" :font-size 14 :top-margin 20)
			"Description des tables SQL pour la classe :"
			(tt::put-string (name class)))
	  (tt::vspace 10)
	  (tt::paragraph (:h-align :left :top-margin 15
				   :left-margin 5 :right-margin 5 :font "courier" :font-size 10)
			 (tt::verbatim
			  (meta::gen-sql-create-table real-class))
			 (loop for slot in (clos:class-slots real-class)
			       when (meta::stored slot) do
			       (tt::vspace 10)
			       (tt::verbatim
				(meta::gen-slot-aux-tables-sql real-class slot))))
	  :eop)))
    (tt::draw-pages content :margins *margins* :header *header* :footer *footer*)
    (dolist (slot (direct-slots class))
      (gen-doc-content slot))))


;DESCRIPTION DETAILLEE D'UN GROUPE DE CLASSE :
(defmethod gen-doc-content ((group class-group))
  (let ((content
	 (tt::compile-text
	  ()
	  (tt:paragraph (:font "Helvetica-Bold" :font-size 16 :top-margin 20)
			"Groupe de classes: " (tt::put-string (name group)))
	  (tt:hrule :dy 2)
	  (tt::vspace 10)
	  (tt:paragraph (:font "Helvetica-Bold" :font-size 16 :top-margin 20)
			(name-value-table "Caractéristiques générales"
			  (list
			   "Nom du champ" (french (user-name group))
			   "Description" (description group)
			   "Commentaire" (comment group)
			   "Nombre de classes" 
			   )))
	  (tt::vspace 10)
	  (tt::table (:col-widths '(100 300) :splittable-p t)
		 (tt::header-row ()
		     (tt::cell (:col-span 2 :background-color '(0.6 0.6 0.9))
			 (tt::paragraph () "Liste résumée des classes")))
		 (tt::header-row (:background-color '(0.6 0.6 0.9))
		     (tt::cell ()(tt::paragraph () "Nom")) (tt::cell ()(tt::paragraph () "Description")))
		 (dolist (class (classes group))
		   (tt::row ()
			(tt::cell ()(tt::paragraph () (tt::put-string (name class))))
			(tt::cell ()(tt::paragraph () (tt::put-string (french (user-name class)) ))))))
	  (tt::vspace 20)
	  :eop)))
    (tt::draw-pages content :margins *margins* :header *header* :footer *footer*)
    
    (dolist (class (classes group))
      (gen-doc-content class))))


(defun put-name-val (name value)
  (tt:row ()
   (tt:cell () (tt::with-style (:font "Helvetica" :font-size 12)
		 (tt::put-string name)))
   (tt:cell () (tt::with-style (:font "Helvetica" :font-size 12)
		 (tt::put-string value)))))

(defun name-value-table (title list)
  (tt:table (:col-widths '(100 300) :border 0.1)
	    (tt:row ()
		    (tt:cell (:col-span 2 :background-color '(0.6 0.6 0.9))
			     (tt::with-style (:font "Helvetica-Bold" :font-size 14)
			       (tt::put-string title))))
	    (loop for (name value) on list by 'cddr
		  do (put-name-val name value))))



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
	       "Documentation technique" :vfill)
	     :eop
	  (tt::vspace 10)
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
	  (tt::table (:col-widths '(100 300) :splittable-p t)
		 (tt::header-row ()
		     (tt::cell (:col-span 2 :background-color '(0.6 0.6 0.9))
			 (tt::paragraph () "Liste résumée des groupes de classes")))
		 (tt::header-row (:background-color '(0.6 0.6 0.9))
		     (tt::cell ()(tt::paragraph () "Nom"))(tt::cell ()(tt::paragraph () "Description")))
		 (dolist (class (class-groups project))
		   (tt::row ()
			(tt::cell ()(tt::paragraph () (tt::put-string (name class))))
			(tt::cell ()(tt::paragraph () (tt::put-string (french (user-name class)) ))))))
	  (tt::vspace 20)
;********************
	  (tt::table (:col-widths '(100 300) :splittable-p t)
	     (tt::header-row ()
		     (tt::cell (:col-span 2 :background-color '(0.6 0.6 0.9))
		       (tt::paragraph () "Liste résumée des groupes d'utilisateurs")))
	     (tt::header-row (:background-color '(0.6 0.6 0.9))
		     (tt::cell ()(tt::paragraph () "Nom"))(tt::cell ()(tt::paragraph () "Description")))
	    (dolist (group (user-groups project))
	      (tt::row ()
 	         (tt::cell ()(tt::paragraph () (tt::put-string (name group))))
		 (tt::cell ()(tt::paragraph () (tt::put-string (description group)))))))
	  (tt::vspace 20)
;********************	  
	  (tt::table (:col-widths '(100 300) :splittable-p t)
	     (tt::header-row ()
		     (tt::cell (:col-span 2 :background-color '(0.6 0.6 0.9))
		       (tt::paragraph () "Liste résumée des listes SQL")))
	     (tt::header-row (:background-color '(0.6 0.6 0.9))
		     (tt::cell ()(tt::paragraph () "Nom"))(tt::cell ()(tt::paragraph () "Description")))
	    (dolist (sql (sql-lists project))
	      (tt::row ()
 	         (tt::cell ()(tt::paragraph () (tt::put-string (name sql))))
		 (tt::cell ()(tt::paragraph () (tt::put-string (french (user-name sql)) ))))))
	  (tt::vspace 20)
;********************	  
	  (tt::table (:col-widths '(100 300) :splittable-p t)
	     (tt::header-row ()
		     (tt::cell (:col-span 2 :background-color '(0.6 0.6 0.9))
		       (tt::paragraph () "Liste résumée des tables de valeurs")))
	     (tt::header-row (:background-color '(0.6 0.6 0.9))
		     (tt::cell ()(tt::paragraph () "Nom"))(tt::cell ()(tt::paragraph () "Description")))
	    (dolist (table (values-tables project))
	      (tt::row ()
 	         (tt::cell ()(tt::paragraph () (tt::put-string (name table))))
		 (tt::cell ()(tt::paragraph () (tt::put-string (french (description table)) ))))))
;********************	  
	  :eop))))
    (tt::draw-pages content :margins *margins* :header *header* :footer *footer*))
  ;;; 2 groupes pour les tests
  (dolist (group (subseq (class-groups project) 0 3))
    (gen-doc-content group)))
  
(defun gen-doc (project)
  (create-project-classes project)
  (let ((file (format nil "~a~a-documentation.pdf"
		      (or (and (> (length (sources-directory project)) 0)
			       (sources-directory project))
			  "/tmp/")
		      (name project)))
	(*margins* '(72 72 72 50))
	(meta::*country-language* :fr))
    (tt::with-document ()
      (let* ((print-stamp (multiple-value-bind (second minute hour date month year)
			      (get-decoded-time)
			    (format nil "Printed on ~4D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
				    year month date hour minute second)))
	     (*header* (tt::compile-text ()
				   (tt:paragraph (:h-align :center
							:font "Helvetica-BoldOblique" :font-size 12)
					      (tt::put-string (meta::translate (name project)))
					      ": Documentation technique")
				   (tt:hrule :dy 0.5)))
	     (*footer* (lambda (pdf:*page*)
		       (tt::compile-text
			(:font "Helvetica" :font-size 10)
			(tt:hrule :dy 1/2)
			(tt:hbox (:align :center :adjustable-p t)
			      (tt:verbatim print-stamp)
					;(hspace 100)
			      :hfill
			      (tt:verbatim
			       (format nil "Page ~d"
				       (1+ (position pdf:*page* (tt::pages pdf:*document*))))))))))
	(gen-doc-content project)
	(when pdf:*page* (tt::finalize-page pdf:*page*))
	(pdf:write-document file)))))
