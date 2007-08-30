
(defpackage meta-level
 (:use common-lisp clos)
 (:shadow cl:defclass)
 (:export translated-string translate)
 (:nicknames meta))

(defpackage interface-rules
 (:use common-lisp)
   (:nicknames irules))

