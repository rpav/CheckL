(in-package :checkl-tests)

(5am:def-suite :default)

(defvar *x* 1)

(checkl:check-formal (:name :two)
  (values
   (+ 1 1)
   (+ *x* 1)))
