(in-package "META-WEB")

(defun update-class-project-1-to-2 (store pass)
  (let* ((*package* (find-package "META-WEB")) (class (find-class 'project)))
    (when (eq pass :modify-tables) (meta-level::add-slot-to-class-table store class 'files))
    (when (eq pass :modify-data)
      (clsql-sys:with-database (nil nil :pool *database-pool*)
        (clsql-sys:do-query ((object-id) "SELECT id  FROM project" :types :auto)
          (let ((object (meta-level:load-object object-id store)))
            (when object
              (let ((data-object (meta-level::load-object-data object)))
                (slot-makunbound data-object 'files)
                (meta-level::initialize-unbound-slots object)
                (meta-level::mark-object-as-modified object))))))))
  (meta-level::save-modified-objects store))


(defun convert-base-from-version-1-to-2 (store)
  (dolist (class '(test source-file))
     (meta::create-class-table store (find-class class)))
(update-class-project-1-to-2 store :modify-tables)


(update-class-project-1-to-2 store :modify-data)
)
