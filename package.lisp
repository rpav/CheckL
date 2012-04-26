(defpackage :checkl
  (:use :cl)
  (:export check run run-all checkl-store checkl-load
           check-formal test-values tests define-test-op
           result-equalp))
