(in-package #:interface)

; (require "comm")
(defvar *user-name* :fr)
(defvar *password* :fr)
(defvar *session* nil)
(defvar *session-id* nil)
(defvar *country-language-id* nil)
(defvar *request* nil)
(defvar *request-counter* 0)
(defvar *request-id* "a")
(defvar *server-name* "")
(defvar *content-stream* nil) ; the stream to write the result content to
(defvar *url-prefix* "")

(defvar *top-level-item* nil)
(defvar *language* nil)

(defvar *object* nil)
(defvar *object-stack* nil)
(defvar *page-root* nil)
(defvar *user* nil)
(defvar *user-groups* nil)
(defvar *request-views* nil) ; the list of (view . object) in the page
(defvar *tab-id* 1)
(defvar *dispatcher* nil)
(defvar *fired-action-value* nil)
(defvar *xml-http* nil)
(defvar *web-server*) ;; :apache, :hunchentoot,

(defconstant +URL-encoding-table+
  "BADy7xuop5Pl8ct2MWORQSNeZ9$-CnzabEFHqrswTUVfghiYXjkGIv01LmKJd346"
  "The non standard set of characters used for URL BASE64 encoding")

(defvar +URL-decoding-table+ (base64:make-decode-table +URL-encoding-table+))

(defun encode-url-string (url)
  (let ((base64::*encode-table* +URL-encoding-table+))
    (base64:string-to-base64-string url)))

(defun decode-url-string (url)
  (let ((base64::*decode-table* +URL-decoding-table+))
    (base64:base64-string-to-string url)))
