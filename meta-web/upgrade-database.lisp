(in-package "META-WEB")

(defun update-class-slot-info-3-to-4 (store pass)
  (let* ((*package* (find-package "META-WEB")) (class (find-class 'slot-info)))
    (when (eq pass :modify-tables) (meta-level::add-slot-to-class-table store class 'list-format))
    (when (eq pass :modify-data)
      (clsql-sys:with-database (nil nil :pool *database-pool*)
        (clsql-sys:do-query ((object-id) "SELECT id  FROM slot_info" :types :auto)
          (let ((object (meta-level:load-object object-id store)))
            (when object
              (let ((data-object (meta-level::load-object-data object)))
                (slot-makunbound data-object 'list-format)
                (meta-level::initialize-unbound-slots object)
                (meta-level::silent-mark-object-as-modified object))))))))
  (meta-level::save-modified-objects store))


(defun update-class-project-3-to-4 (store pass)
  (let* ((*package* (find-package "META-WEB")) (class (find-class 'project)))
    (when (eq pass :modify-tables)
      (meta-level::add-slot-to-class-table store class 'other-documents)
      (meta-level::add-slot-to-class-table store class 'project-version)
      (meta-level::add-slot-to-class-table store class 'version-date))
    (when (eq pass :modify-data)
      (clsql-sys:with-database (nil nil :pool *database-pool*)
        (clsql-sys:do-query ((object-id) "SELECT id  FROM project" :types :auto)
          (let ((object (meta-level:load-object object-id store)))
            (when object
              (let ((data-object (meta-level::load-object-data object)))
                (slot-makunbound data-object 'other-documents)
                (slot-makunbound data-object 'project-version)
                (slot-makunbound data-object 'version-date)
                (meta-level::initialize-unbound-slots object)
                (meta-level::silent-mark-object-as-modified object))))))))
  (meta-level::save-modified-objects store))


(defun convert-base-from-version-3-to-4 (store)
  (dolist (class '(other-document))
     (meta::create-class-table store (find-class class)))
(update-class-project-3-to-4 store :modify-tables)
(update-class-slot-info-3-to-4 store :modify-tables)


(update-class-project-3-to-4 store :modify-data)
(update-class-slot-info-3-to-4 store :modify-data)
)
