(in-package #:meta-web)

(defmethod insert-page-title ((app meta-app) page)
  (let* ((object-id (getf (interface::session-params *request*) :object))
         (object (interface::decode-object-id object-id)))
    (webapp::ensure-user)
    (html:html "Web App Framework" (:when object " - " (:esc (meta::short-description object))))))

