(in-package "USER")

(defsystem "LISP-UTILITY"
  (:default-pathname ""
   :optimize ((debug 3) (safety 3)))
   :members
  ("defpackage"
   "utility"
   "infix"
   "string"
   )
    :rules
  ((:in-order-to :compile :all (:requires (:load :previous)))
   (:in-order-to :load :all (:requires (:load :previous)))
   ))

(defsystem "FLI-UTILITY"
  (:default-pathname ""
   :optimize ((debug 3) (safety 3)))
   :members
  ("defpackage"
   "stdtypes"
   "parse-c-decls"
   "utility-fli"
   )
    :rules
  ((:in-order-to :compile :all (:requires (:load :previous)))
   (:in-order-to :load :all (:requires (:load :previous)))
   ))

(defsystem "UTILITY"
  (:default-pathname ""
   :optimize ((debug 3) (safety 3)))
   :members
;  (("FLI-UTILITY" :type :system)
   (("LISP-UTILITY" :type :system)
   )
    :rules
  ((:in-order-to :compile :all (:requires (:load :previous)))
   (:in-order-to :load :all (:requires (:load :previous)))
   ))


