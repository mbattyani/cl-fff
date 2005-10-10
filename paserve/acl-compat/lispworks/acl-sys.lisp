(in-package :sys)
(let ((*handle-warn-on-redefinition* :warn))
;      (*packages-for-warn-on-redefinition* nil))

  (defun command-line-arguments ()
    system:*line-arguments-list*)
  
  (defun command-line-argument (n)
    (nth n system:*line-arguments-list*))
  
  (defun reap-os-subprocess (&key (wait nil))
    (declare (ignore wait))
    nil)

  (export 'command-line-arguments)
  (export 'command-line-argument)
  (export 'reap-os-subprocess))

;; Franz uses the MSWINDOWS feature conditional in some of their code;
;; thus, under Windows, ACL-COMPAT should probably push MSWINDOWS
;; onto the *features* list when it detects the presence of WIN32
;; under Lispworks.
#+WIN32 (eval-when (:compile-toplevel :load-toplevel :execute)
          (pushnew :mswindows *features*))
