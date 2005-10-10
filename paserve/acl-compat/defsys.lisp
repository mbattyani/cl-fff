(in-package "CL-USER")

(defsystem "ACL-COMPAT"
  (:default-pathname "ACL-COMPAT:")
  :members
  ("acl-compat-common-lisp-lw"
   "nregex"
   "acl-excl-lw"
   "acl-mp-package"
   "acl-mp-lw"
   "gray-stream-package"
   "acl-socket-lw"
   "acl-sys-lw"
   "meta"
   "uri"
   "chunked-stream-mixin")
  
  :rules
  ((:in-order-to :compile "acl-excl-lw"
    (:caused-by (:compile "nregex"))
    (:requires (:load "nregex")))
   (:in-order-to :load "acl-excl-lw"
    (:requires (:load "nregex")))
   
   (:in-order-to :compile "acl-mp-lw"
    (:caused-by (:compile "acl-mp-package" "acl-socket-lw"))
    (:requires (:load "acl-mp-package" "acl-socket-lw")))
   (:in-order-to :load "acl-mp-lw"
    (:requires (:load "acl-mp-package" "acl-socket-lw")))
   
   (:in-order-to :compile "acl-socket-lw"
    (:caused-by (:compile "chunked-stream-mixin"))
    (:requires (:load "chunked-stream-mixin")))
   (:in-order-to :load "acl-socket-lw"
    (:requires (:load "chunked-stream-mixin")))
   
   (:in-order-to :compile "chunked-stream-mixin"
    (:caused-by (:compile "acl-excl-lw" "gray-stream-package"))
    (:requires (:load "acl-excl-lw" "gray-stream-package")))
   (:in-order-to :load "chunked-stream-mixin"
    (:requires (:load "acl-excl-lw" "gray-stream-package")))
   
   (:in-order-to :compile "uri"
    (:caused-by (:compile "meta"))
    (:requires (:load "meta")))
   (:in-order-to :load "uri"
    (:requires (:load "meta")))))

(eval-when (:load-toplevel :execute)
  (pushnew :acl-compat *features*))
