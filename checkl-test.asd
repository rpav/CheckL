(cl:eval-when (:load-toplevel :execute)
  (asdf:load-system :fiveam)
  (asdf:load-system :checkl))

(defsystem :checkl-test
  :description "CheckL: Testing testing testing"
  :version "1.0"
  :author "Ryan Pavlik <rpavlik@gmail.com>"
  :license "LLGPL, BSD"

  :depends-on (:fiveam :checkl)
  :serial t
  :pathname "t"

  :components ((:file "package")
               (checkl:tests "test")
               (checkl:test-values "test-values"
                                   :package :checkl-tests)))

(checkl:define-test-op :checkl-test)
