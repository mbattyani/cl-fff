(lw:defsystem "HTML"
  (:default-pathname "")
  :members ("defpackage"
	    "html-utils"
	    "html-gen2"
	    "func-tags"
	    "base64"
	    "sha"
	    "franz-if"
	    "smtp"
	    )
  :rules
  ((:in-order-to :compile :all (:requires (:load :previous)))
   (:in-order-to :load :all (:requires (:load :previous))))
  )

(lw:defsystem "HTML-PARSE"
  (:default-pathname "")
  :members ("defpackage"
	    "franz-if"
	    "html-parse"
	    )
  :rules
  ((:in-order-to :compile :all (:requires (:load :previous)))
   (:in-order-to :load :all (:requires (:load :previous))))
  )