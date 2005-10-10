(lw:defsystem "INTERFACE"
  (:default-pathname "")
  :members (("HTML" :type :system)
	    ("METALEVEL" :type :system)
	    ("LISP-UTILITY" :type :system)
	    "defpackage"
	    "specials"
	    "font"
	    "font-tables"
	    "sql-lists"
	    "pane"
	    "pane-utilities"
	    "request"
	    "session"
	    "process-url"
	    "http-link"
	    "view"
	    "ui-desc"
;	    "ui-java"
	    "ui-html"
	    "ui-atl"
;	    "ui-win32"
	    "item-layouts"
	    "slot-layouts"
	    "layouts"
	    "view-layout"
	    "html-view"
	    "http"
	    "rpc"
	    "std-web"
	    "dispatcher"
	    "http-server"
	    "apache"
	    "jserv"
	    "predefined-urls"
	    )
  :rules
  ((:in-order-to :compile :all (:requires (:load :previous)))
   (:in-order-to :load :all (:requires (:load :previous))))
  )

