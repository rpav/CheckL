(defsystem :checkl
  :description "CheckL: Dynamic testing for Common Lisp"
  :version "1.0"
  :author "Ryan Pavlik <rpavlik@gmail.com>"
  :license "LLGPL, BSD"

  :depends-on (:marshal)
  :serial t

  :components ((:file "package")
               (:file "checkl")

               #+5am
               (:file "formalize")))

(defmethod perform ((o test-op) (c (eql (find-system :checkl))))
  (operate 'asdf:load-op :checkl-test)
  (operate 'asdf:test-op :checkl-test))
