(in-package "META-WEB")

(defun update-class-slot-info-3-to-4 (store pass)
  (let* ((*package* (find-package "META-WEB")) (class (find-class 'slot-info)))
    (when (eq pass :modify-tables)
      (meta-level::add-slot-to-class-table store class 'slot-view-name)
      
      )
    (when (eq pass :modify-data)
      (clsql-sys:with-database (nil nil :pool *database-pool*)
        (clsql-sys:do-query ((object-id) "SELECT id  FROM slot_info" :types :auto)
          (let ((object (meta-level:load-object object-id store)))
            (when object
              (let ((data-object (meta-level::load-object-data object)))
                (slot-makunbound data-object 'slot-view-name)
                
                
                (meta-level::initialize-unbound-slots object)
                (meta-level::silent-mark-object-as-modified object))))))))
  (meta-level::save-modified-objects store))





(defun convert-base-from-version-3-to-4 (store)
(update-class-slot-info-3-to-4 store :modify-tables)


(update-class-slot-info-3-to-4 store :modify-data)
)
