(in-package "META-WEB")

(defun convert-base-from-version-4-to-5 (store)
  (dolist (class 'nil)
     (meta::create-class-table store (find-class class)))


)
