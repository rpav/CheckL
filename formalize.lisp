(in-package :checkl)

(defvar *formal-testing-only* nil)

(defmacro check-formal ((&key name (category :default) depends-on)
                        &body body)
  (unless (and name (symbolp name))
    (error "NAME must be a non-NIL symbol for formal tests."))
  (let ((fun (gensym))
        (catsym (gensym))
        (bodysym (gensym)))
    `(let* ((,catsym ,category)
            (,bodysym ',body)
            (,fun (lambda () ,@body)))
       (ensure-test ,name ,catsym ,bodysym ,fun)
       (5am:test (,name ,@(if depends-on `(:depends-on ,depends-on))
                        :suite ,category)
         (let* ((result (multiple-value-list (progn ,@body)))
                (tests (current-tests))
                (last-result (gethash ,name (package-tests-results tests))))
           (loop for val in result
                 as prev in last-result
                 do (5am:is (result-equalp last-result result)))))
       (unless *formal-testing-only*
         (values-list (run ,name))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass test-values (asdf:static-file)
    ((package :accessor test-values-package :initarg :package))
    (:documentation "An ASDF component for loading CheckL test values."))

  (defmacro define-test-op (system-name &optional other-system-name)
    (if other-system-name
        `(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system ',system-name))))
           (operate 'asdf:load-op ',other-system-name)
           (operate 'asdf:test-op ',other-system-name))
        `(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system ',system-name))))
           (let ((5am:*test-dribble* *error-output*))
             (5am:run! :default)))))

  (defmethod asdf:perform ((op asdf:load-op) (c test-values))
    (let ((*package* (find-package (test-values-package c))))
      (let ((pathname (asdf:component-pathname c))
            (tests (current-tests)))
        (setf (package-tests-default-checkl-store tests) pathname)
        (if (probe-file pathname)
            (checkl-load pathname)
            (warn "CheckL test values not loaded: ~A" pathname)))))


  (defclass tests (asdf:cl-source-file) ()
    (:documentation "Load a file with CHECK-FORMAL tests."))

  (defmethod asdf:perform ((op asdf:load-op) (c tests))
    (let ((*formal-testing-only* t))
      (call-next-method))))
