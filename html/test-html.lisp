(in-package html)

(html-to-file #P"/temp/test-h1.html"
	      (:html
	       (:head (:title "Test html<1>")) :crlf
	       (:body
		(:h1 (esc "Test html<1>")) :crlf
		:br
		(:p "The multiplication table")
		((:table :bgcolor "mediumseagreen")
		 ((:tr :bgcolor "lemonchiffon")((:th :width "20" :align "lightgreen") "x")
		  (dotimes (a 11)
		    (html ((:th :width "20" :align "right") (ffmt "~a" a)))))
		 (dotimes (b 11)
		   (html (:tr ((:td :width "20" :align "right" :bgcolor "lightgreen") (ffmt "~a" b))
			      (dotimes (a 11)
				(html ((:td :width "20" :align "right" :bgcolor "mediumaquamarine") (ffmt "~a" (* a b))))))))))))

(pprint (macroexpand 
	 '(:html
	   (:head (:title "Test html<1>")) :crlf
	   (:body
	    (:h1 (esc "Test html<1>")) :crlf
	    :br
	    (:p "The multiplication table")
	    ((:table :bgcolor "mediumseagreen")
	     ((:tr :bgcolor "lemonchiffon")((:th :width "20" :align "lightgreen") "x")
	      (dotimes (a 11)
		(html ((:th :width "20" :align "right") (ffmt "~a" a)))))
	     (dotimes (b 11)
	       (html (:tr ((:td :width "20" :align "right" :bgcolor "lightgreen") (ffmt "~a" b))
			  (dotimes (a 11)
			    (html ((:td :width "20" :align "right" :bgcolor "mediumaquamarine") (ffmt "~a" (* a b)))))))))))))

(pprint (macroexpand '(html (:tr ((:td :width "20" :align "right" :bgcolor "green") (ffmt "~a" b))
			     (dotimes (a 11)
			       (html ((:td :width "20" :align "right") (ffmt "~a" (* a b)))))))))

(defun cv-title (title)
  (html ((:div :style "'border:none;border-bottom:solid windowtext 0.5pt'")
	 (:p (:b title)))))

(defmacro cv-item (date &rest text)
  `(html (:p (:b ,date) " "  ,@text)))

(html-to-file #P"/temp/cv-marc.html"
	      (:html
	       (:head (:title "CV Marc")) :crlf
	       (:body
		((:font :face "arial")
		 (:div
		  (:p "Marc Battyani" :br "3, Avenue Saint Marc" :br "77850 HERICY")
		  (:p ((:a :href "'mailto:Marc_Battyani@wanadoo.fr'") "Marc_Battyani@wanadoo.fr") :br
		      "Téléphone personnel: +33 (0)1 60 74 24 25" :br
		      "Téléphone professionnel: +33 (0)1 60 39 53 40")
		  :br ) :crlf
		 (cv-title "EXPERIENCE PROFESSIONNELLE")
		 (cv-item "Depuis 2000:"
			  "Directeur de la recherche et du développement et gérant de la société FRACTAL CONCEPT qui a repris les activités en électronique et informatique de la société CONTEXT FREE")
		 (cv-item "1989 - 2000:"
			  "Directeur de la recherche de CONTEXT FREE, gérant jusqu'en 1998. Conception, développement et mise au point de technologies en informatique et instrumentation." :br
			  
			  (:ul (:li "Pilotage de robots et traitement de signaux ultrasonores pour l'industrie aéronautique et spatiale.")
			       (:li "Compilateurs et Frameworks de génération de logiciels Win32. Utilisé par les sociétés du groupe Sage pour réaliser une dizaine de logiciels (Ciel Paie, Gestion Commerciale, Ciel et Sage états financiers...) commercialisés à plus de 150000 exemplaires.")
			       (:li "Cryptographie: Chiffrement des mesures de surveillance des centrales nucléaires par l'Agence Internationale de l'Energie Atomique")
			       (:li "Reconstitution d'images tomographiques médicales. Utilisé par les scanners Mécaserto et Siemens")
			       (:li "Moteurs de base de données documentaires et sémantiques. Utilisé par le CETIM, MCP,...")
			       (:li "Réalité virtuelle et visualisation 3D pour la DGA")
			       (:li "...")
			       )
			  "PDG (1996-1997) de la société Inédit SA spécialisée en génération automatique de catalogues.")
		 (cv-item "1988 - 1989:"
			  "Ingénieur consultant en nouvelles technologies dans la société COROM. Audit technique pour des sociétés de capital risque.")
		 (cv-item "1987 - 1988:"
			  "Ingénieur de recherche aux Laboratoires d'Electronique et de Physique appliquées, centre de recherche du groupe PHILIPS, dans la division architecture de systèmes. Les recherches portaient sur des architectures destinées au traitement d'images. Dépôt d'un brevet sur un arbitre de bus spécialisé pour CDi.")
		 (cv-title "FORMATION")
		 ((:p :align "center") (:i "Ingénieur Supélec promotion 1986 section Instrumentation" :br
					   "Post diplôme (DEA) en informatique théorique à l'université de Montréal"))
		 (cv-item "1986 - 1987:"
			  "Assistant, étudiant visiteur post-diplôme à l'université de MONTREAL (CANADA) en informatique théorique (cryptographie, théorie des langage, intelligence artificielle) dans le cadre des échanges inter-universitaire franco-québécois. Recherches et mise au point d'un compilateur de moteur d'inférences pour un système expert destiné aux architectes paysagistes")
 		 (cv-item "1983 - 1986:"
	 		  "Ecole Supérieure d'Electricité (SUPELEC). Spécialisation en instrumentation, métrologie. Entré 1<sup>er</sup> sur concours P'")
 		 (cv-item "1986:"
	 		  "Licence et maîtrise d'informatique théorique à l'université PARIS XI ORSAY en parallèle
avec la 3<sup>ième</sup> année à SUPELEC. Mentions TB")
		 (cv-title "DIVERS")
		 (:p "Anglais couramment lu, écrit et parlé.")
		 (:p "Lauréat du concours général 1981 en physique. Représentation de la FRANCE aux 12<sup>ièmes</sup> olympiades internationales de physique en 1981 à Varna (Bulgarie).")
		 (:p "Nationalité française, 37 ans. Marié, trois enfants.")
 		 ))))

