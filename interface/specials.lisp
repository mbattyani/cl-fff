(in-package interface)

(require "comm")
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
(defvar *url-prefix* "/asp")

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

