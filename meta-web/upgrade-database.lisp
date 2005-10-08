(in-package "META-WEB")

(defun convert-base-from-version-4-to-5 (store)
  (dolist (class '(clipboard))
     (meta::create-class-table store (find-class class)))


)
