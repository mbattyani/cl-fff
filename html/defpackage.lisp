
(defpackage html
  (:use common-lisp)
  (:export #:html #:html-to-string #:html-to-stream #:html-to-file #:*html-stream*
	   #:write-string-quoting-specials #:fast-format #:write-http-newline
	   #:force-style #:quote-javascript-string #:quote-string
	   #:html-to-stream #:html-to-string #:html-to-file #:html #:html-gen #:fmt
	   #:ffmt #:esc #:define-empty-tags #:add-func-attr #:add-func-tag
           #:merge-attributes
           #:parse-html
	   ))





