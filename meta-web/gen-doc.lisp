(in-package meta-web)

(defvar *header* nil)
(defvar *footer* nil)
(defvar *margins* nil)
(defparameter *color1* '(0.2 0.2 0.8))

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

(defmethod gen-doc-content ((slot slot-info))
  (let ((content
	 (tt:compile-text
		 ()
		 (tt:paragraph (:font "Helvetica-Bold" :font-size 16 :top-margin 20)
			    "Slot : " (tt::put-string (name slot)))
		 (tt:hrule :dy 2)
		 :eop)))
    (tt:draw-pages content :margins *margins* :header *header* :footer *footer*)
    
    ))


(defmethod gen-doc-content ((class class-info))
  (let ((content
	 (tt:compile-text
		 ()
		 (tt:paragraph (:font "Helvetica-Bold" :font-size 16 :top-margin 20)
			    "Class : " (tt::put-string (name class)))
		 (tt:hrule :dy 2)
		 :eop)))
    (tt:draw-pages content :margins *margins* :header *header* :footer *footer*)
    
    (dolist (slot (direct-slots class))
      (gen-doc-content slot))))

(defmethod gen-doc-content ((group class-group))
  (let ((content
	 (tt:compile-text
	  ()
	  (tt:paragraph (:font "Helvetica-Bold" :font-size 16 :top-margin 20)
			"Groupe de classes: " (tt::put-string (name group)))
	  (tt:hrule :dy 2)
	  :eop)))
    (tt:draw-pages content :margins *margins* :header *header* :footer *footer*)
    
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

(defmethod gen-doc-content ((project project))
  (let ((content
	 (tt:compile-text
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
	  (tt::vspace 20)
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
	  :eop))))
    (tt:draw-pages content :margins *margins* :header *header* :footer *footer*))
  ;;; 2 groupes pour les tests
  (dolist (group (subseq (class-groups project) 0 2))
    (gen-doc-content group)))
  
(defun gen-doc (project)
  (let ((file (format nil "~a~a-documentation.pdf"
		      (or (and (> (length (sources-directory project)) 0)
			       (sources-directory project))
			  "/tmp/")
		      (name project)))
	(*margins* '(72 72 72 50))
	(meta::*country-language* :fr))
    (tt:with-document ()
      (let* ((print-stamp (multiple-value-bind (second minute hour date month year)
			      (get-decoded-time)
			    (format nil "Printed on ~4D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
				    year month date hour minute second)))
	     (*header* (tt:compile-text ()
				   (tt:paragraph (:h-align :center
							:font "Helvetica-BoldOblique" :font-size 12)
					      (tt::put-string (meta::translate (name project)))
					      ": Documentation technique")
				   (tt:hrule :dy 0.5)))
	     (*footer* (lambda (pdf:*page*)
		       (tt:compile-text
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
	(when pdf:*page* (tt:finalize-page pdf:*page*))
	(pdf:write-document file)))))
