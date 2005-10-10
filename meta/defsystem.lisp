(lw:defsystem "METALEVEL"
  (:default-pathname "")
  :members (
	    ("LISP-UTILITY" :type :system)
	    "defpackage"
	    "rules"
	    "meta-class"
	    "slot"
	    "object"
	    "store"
	    #-no-psql "psql-store"
	    "class-info"
	    #-no-psql "sql"
	    "utilities"
;	    "function"
	    )
  :rules
  ((:in-order-to :compile :all (:requires (:load :previous)))
   (:in-order-to :load :all (:requires (:load :previous)))
  ))
