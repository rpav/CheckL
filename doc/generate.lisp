(defpackage :checkl.docs
  (:use #:cl #:gendoc)
  (:export #:generate))

(in-package :checkl.docs)

(defun generate ()
  (gendoc (:output-system :checkl-docs
           :output-filename "index.html"
           :css "simple.css")
    (:mdf "intro.md")
    (:apiref :checkl)))
