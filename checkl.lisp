(in-package :checkl)

(defstruct package-tests
  (results (make-hash-table :test 'equalp))
  (categories (make-hash-table :test 'equalp))
  (lambdas (make-hash-table :test 'equalp))
  (default-checkl-store nil))

(defvar *all-tests* (make-hash-table))

(define-condition result-error (error)
  ((result-index :initarg :result-index :reader result-error-index :initform nil)
   (result-value :initarg :result-value :reader result-error-value :initform nil)
   (last-value :initarg :last-value :reader result-error-last :initform nil))
  (:report (lambda (c s)
             (format s "Result ~A has changed: ~A~%Previous result: ~A"
                     (result-error-index c)
                     (result-error-value c)
                     (result-error-last c)))))

(defgeneric result-equalp (o1 o2))

(defmethod result-equalp (o1 o2)
  (equalp o1 o2))

(defun current-tests ()
  (or (gethash *package* *all-tests*)
      (setf (gethash *package* *all-tests*)
            (make-package-tests))))

(defun ensure-test (test-name test-category test-body function)
  (let* ((tests (current-tests))
         (results (package-tests-results tests))
         (categories (package-tests-categories tests))
         (lambdas (package-tests-lambdas tests)))
    (when (and test-name (gethash test-body results))
      (setf (gethash test-name results)
            (gethash test-body results))
      (remhash test-body results)

      (when test-category
        (map 'nil (lambda (x)
                    (if (equalp x test-body) test-name x))
             (gethash test-category categories))))

    (when test-category
      (let ((name (or test-name test-body)))
        (pushnew name (gethash test-category categories)
                 :test 'equalp)))

    (setf (gethash (or test-name test-body) lambdas)
          function)))

(defun verify-result (name result)
  (let* ((results (package-tests-results (current-tests)))
         (last-result (gethash name results))
         (index-base 0)
         (cur-result result)
         result-index result-value error-p)
    (loop
      (restart-case
          (loop for val in cur-result
                for prev in last-result
                for i from 0
                do (unless (result-equalp val prev)
                     (setf result-index i)
                     (setf result-value val)
                     (error (make-condition 'result-error
                                            :result-index (+ i index-base)
                                            :result-value val
                                            :last-value prev)))
                finally
                   (unless error-p
                     (setf (gethash name results) result))
                   (return-from verify-result result))
        (use-new-value ()
          :report "The new value is correct, use it from now on."
          :test (lambda (c) (typep c 'result-error))
          (incf index-base (1+ result-index))
          (setf (nth result-index last-result) result-value)
          (setf cur-result (nthcdr (1+ result-index) cur-result))
          (setf last-result (nthcdr (1+ result-index) last-result)))
        (skip-test ()
          :report "Skip this, leaving the old value, but continue testing"
          :test (lambda (c) (typep c 'result-error))
          (incf index-base (1+ result-index))
          (setf cur-result (nthcdr (1+ result-index) cur-result))
          (setf last-result (nthcdr (1+ result-index) last-result))
          (setf error-p t))))))

(defmacro check ((&key name (category :default)) &body body)
  (let ((fun (gensym))
        (result (gensym))
        (namesym (gensym))
        (catsym (gensym))
        (bodysym (gensym)))
    `(let* ((,namesym ,name)
            (,catsym ,category)
            (,bodysym ',body)
            (,fun (lambda () ,@body))
            (,result (multiple-value-list (funcall ,fun))))
       (ensure-test ,namesym ,catsym ,bodysym ,fun)
       (values-list (verify-result (or ,namesym ,bodysym) ,result)))))

(defun run (&rest names)
  (let ((lambdas (package-tests-lambdas (current-tests))))
    (loop for name in names
          as fn = (gethash name lambdas)
          collect (verify-result name (multiple-value-list (funcall fn)))
            into results
          finally (return (values-list results)))))

(defun run-all (&optional (category :default) &rest categories)
  (push category categories)
  (let ((current-categories (package-tests-categories (current-tests))))
    (loop for cat in categories
          appending (gethash cat current-categories) into names
          finally (return (apply #'run names)))))

(defun checkl-store (&optional filespec)
  (let ((filespec (or filespec (package-tests-default-checkl-store (current-tests))))
        (results (package-tests-results (current-tests))))
    (unless (> (hash-table-count results) 0)
      (warn "Not writing blank test results to ~A! CHECKL-LOAD, or write some tests." filespec))
    (with-open-file (stream filespec :direction :output :if-exists :supersede)
      (let ((*print-readably* t)) 
        (write (ms:marshal results) :stream stream)))
    (values)))

(defun checkl-load (&optional filespec)
  (let* ((tests (current-tests))
         (filespec (or filespec (package-tests-default-checkl-store tests))))
    (with-open-file (stream filespec)
      (setf (package-tests-results tests) (ms:unmarshal (read stream))))))

(defmacro do-categories ((var tests) &body body)
  `(map 'nil
        (lambda (,var) ,@body)
        (loop for k being the hash-keys of (package-tests-categories ,tests)
              collect k)))

(defun clear (&rest names)
  (let ((tests (current-tests)))
    (loop for name in names do
      (remhash name (package-tests-results tests))
      (remhash name (package-tests-lambdas tests))
      (do-categories (c tests)
        (setf (gethash c (package-tests-categories tests))
              (delete name (gethash c (package-tests-categories tests))))))))

(defun clear-anonymous ()
  (let ((tests (current-tests)))
    (loop for name being the hash-keys of (package-tests-results tests) do
      (when (not (symbolp name))
        (remhash name (package-tests-results tests))
        (remhash name (package-tests-lambdas tests))
        (do-categories (c tests)
          (setf (gethash c (package-tests-categories tests))
                (delete name (gethash c (package-tests-categories tests)))))))))
