(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :cl-gendoc))

(defsystem :checkl-docs
  :depends-on (:checkl :cl-gendoc)

  :serial t
  :pathname "doc"

  :components
  ((:file "generate")))

(gendoc:define-gendoc-load-op :checkl-docs :checkl.docs 'generate)
