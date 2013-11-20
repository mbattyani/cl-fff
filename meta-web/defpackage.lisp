(defpackage meta-web
  (:nicknames mw)
  (:use common-lisp iterate)
  (:shadowing-import-from meta-level defclass)
  (:import-from interface *user* *session* *object* *request* *country-language* *user-groups*)
  )
