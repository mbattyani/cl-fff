(defpackage meta-web
  (:nicknames mw)
  (:use common-lisp iterate webapp)
  (:shadowing-import-from meta-level defclass)
  (:import-from interface *user* *session* *object* *request* *country-language* *user-groups*)
  )
