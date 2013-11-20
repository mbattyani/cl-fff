(defpackage #:interface
 (:use common-lisp iterate)
 (:shadowing-import-from meta-level defclass)
 (:import-from meta-level *country-language* *country*)
 (:export
  *server-name*
  *clws-address*
  *clws-port*
  *web-server*))
