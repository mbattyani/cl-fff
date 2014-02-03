(defpackage webapp
  (:nicknames wa)
  (:use common-lisp)
  (:shadowing-import-from meta-level defclass)
  (:import-from interface *user* *session* *object* *request* *country-language* *user-groups*)
  (:export #:gen-breadcrumbs #:insert-page-title #:insert-page-header #:insert-page-footer
           #:insert-html-meta #:insert-html-head-links
           #:write-page #:page-desc #:user #:base-app #:*app*))
