(defpackage webapp
  (:nicknames wa)
  (:use common-lisp)
  (:shadowing-import-from meta-level defclass)
  (:import-from interface *user* *session* *object* *request* *country-language* *user-groups*)
  (:exports #:gen-breadcrumbs #:insert-page-title #:insert-html-meta #:insert-html-head-links
            #:write-page ))
