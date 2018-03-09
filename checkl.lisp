(in-package :checkl)

(defstruct package-tests
  (results (make-hash-table :test 'equalp))
  (categories (make-hash-table :test 'equalp))
  (lambdas (make-hash-table :test 'equalp))
  (default-checkl-store nil))

(defvar *all-tests* (make-hash-table))
(defvar *definitions-only* nil)

(define-condition result-error (error)
  ((result-index :initarg :result-index :reader result-error-index :initform nil)
   (result-value :initarg :result-value :reader result-error-value :initform nil)
   (last-value :initarg :last-value :reader result-error-last :initform nil))
  (:report (lambda (c s)
             (format s "Result ~A has changed: ~A~%Previous result: ~A"
                     (result-error-index c)
                     (result-error-value c)
                     (result-error-last c)))))

(defgeneric result-equalp (o1 o2)
  (:documentation "Generic function to compare test results `O1` to
`O2`.  Defaults to `EQUALP`."))

(defmethod result-equalp (o1 o2)
  (equalp o1 o2))

(defgeneric result-translate (result)
  (:documentation "RESULT-TRANSLATE is called on RESULT before calling
RESULT-EQUALP and before storing RESULT.  This defaults to RESULT, but
may be useful for converting more complex objects into simpler
objects.  For sequences and structures, COPY-SEQ and COPY-STRUCTURE
are called.  For STANDARD-OBJECT instances, (MS:UNMARSHAL (MS:MARSHAL
OBJECT)) is called."))

(defmethod result-translate (result) result)

(defmethod result-translate ((result sequence))
  (copy-seq result))

(defmethod result-translate ((result structure-object))
  (copy-structure result))

(defmethod result-translate ((result standard-object))
  (ms:unmarshal (ms:marshal result)))

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
          :test
	  #-(or allegro ccl clisp) (lambda (c) (typep c 'result-error))
	  #+(or allegro ccl clisp) (lambda (c) (or (null c) (typep c 'result-error)))
          (incf index-base (1+ result-index))
          (setf (nth result-index last-result) result-value)
          (setf cur-result (nthcdr (1+ result-index) cur-result))
          (setf last-result (nthcdr (1+ result-index) last-result)))
        (skip-test ()
          :report "Skip this, leaving the old value, but continue testing"
	  :test
          #-(or allegro ccl clisp) (lambda (c) (typep c 'result-error))
	  #+(or allegro ccl clisp) (lambda (c) (or (null c) (typep c 'result-error)))
          (incf index-base (1+ result-index))
          (setf cur-result (nthcdr (1+ result-index) cur-result))
          (setf last-result (nthcdr (1+ result-index) last-result))
          (setf error-p t))))))

(defmacro check ((&key name (category :default) (output-p nil)) &body body)
  "=> test-results

Run `BODY`.  Check resulting values against the last run using
`CHECKL:RESULT-EQUALP`, or store them if this is the first run.
Sameness of the test is determined by comparing the body with
`EQUALP`, or by `NAME`.

`NAME` may be specified to name a test.  If the test exists but is
anonymous (i.e., `NAME` has not been specified), specifying `NAME`
will name the test and it will no longer be anonymous.

`CATEGORY` may be specified for running groups of tests.

If `OUTPUT-P` is `t`, the results will be printed to
`*standard-output*` as well as returned.  This may be helpful if the
results are too long to see in your emacs minibuffer."
  (let ((fun (gensym))
        (result (gensym))
        (namesym (gensym))
        (catsym (gensym))
        (bodysym (gensym)))
    `(let* ((,namesym ,name)
            (,catsym ,category)
            (,bodysym ',body)
            (,fun (lambda () ,@body))
            (,result (unless *definitions-only*
                       (multiple-value-list (funcall ,fun)))))
       (ensure-test ,namesym ,catsym ,bodysym ,fun)
       (unless *definitions-only*
         (let ((result-list (verify-result (or ,namesym ,bodysym) ,result)))
           ,(when output-p
              `(loop for result in result-list do
                (pprint result)))
           (values-list result-list))))))

(defun run (&rest names)
  "=> test-results

Run tests named `NAMES`, collecting their results."
  (let ((lambdas (package-tests-lambdas (current-tests))))
    (loop for name in names
          as fn = (gethash name lambdas)
          collect (verify-result name (multiple-value-list (funcall fn)))
            into results
          finally (return (values-list results)))))

(defun run-all (&optional (category :default) &rest categories)
  "=> test-results

Run all tests, optionally specifying categories."
  (push category categories)
  (let ((current-categories (package-tests-categories (current-tests))))
    (loop for cat in categories
          appending (gethash cat current-categories) into names
          finally (return (apply #'run names)))))

(defun checkl-store (&optional filespec)
  "Store package test results to `FILESPEC`"
  (let ((filespec (or filespec (package-tests-default-checkl-store (current-tests))))
        (results (package-tests-results (current-tests))))
    (unless (> (hash-table-count results) 0)
      (warn "Not writing blank test results to ~A! CHECKL-LOAD, or write some tests." filespec))
    (with-open-file (stream filespec :direction :output :if-exists :supersede)
      (let ((*print-readably* t)
            (*print-circle* t))
        (write (ms:marshal results) :stream stream)))
    (values)))

(defun checkl-load (&optional filespec)
  "Load package test results from `FILESPEC`"
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
  "Clear the test results from the tests `NAMES`.  For clearing anonymous
test results, see `CLEAR-ANONYMOUS`."
  (let ((tests (current-tests)))
    (loop for name in names do
      (remhash name (package-tests-results tests))
      (remhash name (package-tests-lambdas tests))
      (do-categories (c tests)
        (setf (gethash c (package-tests-categories tests))
              (delete name (gethash c (package-tests-categories tests))))))))

(defun clear-anonymous ()
  "Clear anonymous test results.  For clearing named tests, see `CLEAR`."
  (let ((tests (current-tests)))
    (loop for name being the hash-keys of (package-tests-results tests) do
      (when (not (symbolp name))
        (remhash name (package-tests-results tests))
        (remhash name (package-tests-lambdas tests))
        (do-categories (c tests)
          (setf (gethash c (package-tests-categories tests))
                (delete name (gethash c (package-tests-categories tests)))))))))

(defmacro check-output (&body body)
  "Use this within a `CHECK` block.  Rebind `*standard-output*` and
`*error-output*` and return a `CHECK`-able result."
  (let ((so (gensym)) (se (gensym)))
    `(let* ((,so (make-string-output-stream))
            (,se (make-string-output-stream))
            (*standard-output* ,so)
            (*error-output* ,se))
       ,@body
       (list (get-output-stream-string ,so)
             (get-output-stream-string ,se)))))

(defmacro results (&rest values)
  "=> VALUES

This will evaluate each subform in order and call RESULT-TRANSLATE on
the result.  This is especially useful if subforms repeatedly modify
and return the object, e.g. `(results (incf *x*) (incf *x*))`.

If multiple values are returned, each value is mapped via
RESULT-TRANSLATE and the result is returned as a list of the
translated values."
  `(values
    ,@(mapcar (lambda (x)
                (let ((vlist (gensym)))
                  `(let ((,vlist (multiple-value-list ,x)))
                     (if (cdr ,vlist)
                         (mapcar #'checkl:result-translate ,vlist)
                         (checkl:result-translate (car ,vlist))))))
              values)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass test-values (asdf:static-file)
    ((package :accessor test-values-package :initarg :package))
    (:documentation "An ASDF component for loading CheckL test values."))

  (defclass tests (asdf:cl-source-file) ()
    (:documentation "Load a file with CHECK or CHECK-FORMAL tests."))

  (defmethod asdf:perform ((op asdf:load-op) (c tests))
    (let ((*definitions-only* t))
      (call-next-method)))

  (defmethod asdf:perform ((op asdf:load-op) (c test-values))
    (let ((*package* (find-package (test-values-package c))))
      (let ((pathname (asdf:component-pathname c))
            (tests (current-tests)))
        (setf (package-tests-default-checkl-store tests) pathname)
        (if (probe-file pathname)
            (checkl-load pathname)
            (warn "CheckL test values not loaded: ~A" pathname))))))
