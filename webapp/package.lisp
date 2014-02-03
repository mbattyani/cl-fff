(defpackage webapp
  (:nicknames wa)
  (:use common-lisp)
  (:shadowing-import-from meta-level defclass)
  (:import-from interface *user* *session* *object* *request* *country-language* *user-groups*)
  (:export #:gen-breadcrumbs #:insert-page-title #:insert-page-header #:insert-page-footer
           #:insert-html-meta #:insert-html-head-links
           #:write-page #:page-desc #:user #:base-app #:*app*
           #:find-user-by-user-name #:find-user-by-cookie #:link-user-cookie #:create-new-user
           #:check-authentification #:password #:auto-login #:last-access #:ensure-user #:switch-user
           #:insert-log-inout-button #:insert-login-dialog
           #:*authenticated* #:*auth-status* #:*auth-name*
           ))
