(html::html-file "/temp/t1.html"
		 (:head (:title "Salut Anne"))
		 (:body 
		  (:center (:h1 "Salut Anne"))
		  (:p "Quelques exemples de html/lisp. ")
		  (:i "en italique"
		      (:princ-safe "avec des caractères spéciaux : < > ç {}"))
		  :p
		  (:b "En Gras") :br :br
		  (:center (:h2 "Une table"))
		  ((:table border (+ 1 3)
			   bordercolor "red"
			   bgcolor "grey"
			   cellpadding "3")
		   (:tr ((:td bgcolor "blue") 
			 ((:font :color "white" :size "+1")
			  "Value"))
			((:td bgcolor "blue") 
			 ((:font :color "yellow" :size "+3")
			  "Square"))
			)
		   (dotimes (i 10)
		     (html::html (:tr (:td (:princ i))
				      ((:td bgcolor (format nil "#~x~x~x" 
							    (+ 100 (random 100))
							    (+ 100 (random 100))
							    (+ 100 (random 100))))
				       (:princ (* i i)))))))))

