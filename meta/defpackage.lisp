
(defpackage meta-level
 (:use common-lisp #+lispworks clos)
 (:shadow cl:defclass)
 (:shadowing-import-from :cl defmethod defgeneric standard-generic-function standard-method standard-class)
 (:export
  translated-string
  translate
  psql-create-db-pool
  psql-store
  ascii-store
  mongo-store)
 (:nicknames meta))

(defpackage interface-rules
 (:use common-lisp)
 (:nicknames irules))

  #+nil(:shadowing-import-from :clos
                          ;; needed
                          process-a-slot-option 


                          
                          defmethod defgeneric standard-generic-function standard-method standard-class
                          generic-function-method-combination
                          standard-slot-definition
                                                
                          direct-slot-definition-class
                          standard-direct-slot-definition
                          class-direct-default-initargs

                          atomic-exchange-slot-value
                          compute-applicable-methods method-combination-error
                          generic-function-argument-precedence-order

                          standard-effective-slot-definition
                          slot-definition-allocation method-to-definition-spec
                          class-direct-superclasses
                          initialize-instance
                          metaobject class-of generic-function-declarations
                          eql-specializer-p  standard-accessor-method update-dependent compressed-gf-p
                          standard-object remove-method specializer-direct-methods no-next-method map-instance-instance-values map-classes writer-method-class
                          invalid-method-error *print-error-on-unbound-slot*

                          generic-function slot-definition-type make-writer-method
                          standard-reader-method find-no-checking-parents compare-and-swap-slot-value

                          
                          find-method-internal make-instance add-class-finalizing-hook-for-subclasses ensure-generic-function structure-direct-slot-definition
                          compute-slots ensure-class update-instance-for-different-class
                          slot-value-using-class
                          
                          untrace-new-instances-on-access
                          add-class extract-specializer-names compute-default-initargs trap-instance-access class-direct-subclasses
                          ;; to check?
                         ; structure-class change-class method-qualifiers
                         ; optimize-setf-of-slot-value function-keywords remove-dependent finalize-inheritance
                         ; effective-slot-definition-class next-method-p find-method-combination map-dependents method-slot-name add-direct-method call-method validate-metaclass-change apply-method-function class-potential-initargs define-method-combination
                          ; standard-writer-method with-accessors accessor-method-slot-definition specializer-direct-generic-functions get-or-setup-slot-value-atomically slot-unbound trace-new-instances-on-access undefgeneric optimize-slot-value slot-exists-p break-new-instances-on-access add-named-method built-in-class class-name generic-function-name generic-function-method-class slot-boundp generic-function-methods ensure-class-property-atomically allocate-instance compute-discriminating-function make-load-form remove-class-redefinition-hook  class-slots method-lambda-list add-method print-object make-load-form-saving-slots ensure-slot-value-atomically generic-function-initial-methods ensure-generic-function-using-class
                         ; update-instance-for-redefined-class ensure-class-using-class class-default-initargs slot-definition-initargs slot-definition add-dependent
                         ; slot-makunbound
                         ; remove-direct-method class-precedence-list with-slots remove-named-method find-class validate-generic-function-class-change class-finalized-p structure-slot-definition
                          slot-definition-name
                          ;structure-effective-slot-definition method-function generic-function-lambda-list validate-superclass untrace-on-access find-method slot-definition-location
                          ;unbreak-on-access undefclass-function class-direct-slots symbol-macrolet effective-slot-definition call-next-method class-direct-methods class-prototype unbreak-new-instances-on-access untrap-instance-access slot-missing find-slot-definition make-method-lambda make-instances-obsolete set-funcallable-instance-function class-not-checking-p default-initargs add-direct-subclass slot-exists-p-using-class compute-class-potential-initargs set-make-instance-argument-checking class-extra-initargs remove-direct-subclass extract-lambda-list process-a-class-option slot-makunbound-using-class method slots-changed standard compute-class-precedence-list add-class-redefinition-hook reinitialize-instance compute-effective-method fdefinedp shared-initialize slot-definition-readers set-clos-initarg-checking method-generic-function
                          funcallable-standard-object
                          no-applicable-method slot-value
                          funcallable-standard-class class-instance-slots-number class compute-effective-slot-definition
                          class-effective-slots direct-slot-definition make-reader-method method-specializers gf-demand-load-get-module slot-boundp-using-class reader-method-class slot-definition-initform structure-object make-slotd forward-referenced-class slot-definition-writers break-on-access slot-definition-initfunction compute-discriminator-code method-combination trace-on-access specializer funcallable-instance-p
                          )
