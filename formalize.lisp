(in-package :checkl)

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
       (unless *definitions-only*
         (values-list (run ,name))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-test-op (system-name &optional other-system-name)
    (if other-system-name
        `(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system ',system-name))))
           (asdf:operate 'asdf:load-op ',other-system-name)
           (asdf:operate 'asdf:test-op ',other-system-name))
        `(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system ',system-name))))
           (let ((5am:*test-dribble* *error-output*))
             (5am:run! :default))))))
