# CheckL

Why write programs in Common Lisp but tests like Java?

My workflow for writing Common Lisp tends to be like this:

* Write a bit of lisp, perhaps a function, class, or structure
* Write a snippet in a scratch buffer to test
* Fix if necessary and repeat

Testing is already inherent in this process, all we need is a little
bit of Common Lisp magic to take advantage of it.  Thus, CheckL:

```lisp
(defun foo ()
  (+ 1 1))

(check () (foo)) ;; => 2

(defun foo ()
  (+ 1 2))

(check () (foo))

;     |
;     v
; 
; Result 0 has changed: 3
; Previous result: 2
;    [Condition of type CHECKL::RESULT-ERROR]
; 
; Restarts:
;  0: [USE-NEW-VALUE] The new value is correct, use it from now on.
;  1: [SKIP-TEST] Skip this, leaving the old value, but continue testing
;  2: [RETRY] Retry SLIME interactive evaluation request.
;  3: [*ABORT] Return to SLIME's top level.
;  4: [TERMINATE-THREAD] Terminate this thread (#<THREAD "worker" RUNNING {100586AB13}>)
```

# Usage

## Tests

Presumably you already write code to test.  Possibly you even write
something like this, evaluating it and manually checking the result:

```lisp
(progn
  (function-1 ...)
  (function-2 ...))
```

With CheckL, you don't really have to change much.  Your `PROGN`
becomes a `CHECK ()` and you run it.  CheckL notifies you if something
changes!

Results are compared with `CHECKL:RESULT-EQUALP`.  This defaults to
`CL:EQUALP`.  Defining it for other things may be useful.

For very long values, it may be helpful to print them:

```lisp
(check (:output-p t) (some-very-long-result)) => ...
```

If you make changes to the test, it becomes another test:

```lisp
(defun foo () (+ 1 1))
(check () (foo))      ;; => 2
(check () (1- (foo))) ;; => 1
```

However, if you name it before you change it, it'll always compare
against the same list:

```lisp
(check () (foo)) ;; => 2

(defun foo () (+ 1 3))

(check (:name :two) (foo)) ;; => Error!
```

In this case, the old "anonymous" test (actually identified by the
body of the test) is now named `:two`.  Making changes to the test
will alter the same test, now named `:two`, and compare against prior
results.

Finally, you might want to check more than one thing in a single
`CHECK`.  You can do this with `RESULTS` (which is similar but not
identical to `VALUES`; see below):

```lisp
(defun foo () (+ 1 1)
(defun bar () (- 1 1))

(check (:name :two)
  (results (foo) (bar))) ;; => 2, 0

(defun bar () (foo))

(check (:name :two)
  (results (foo) (bar))) ;; => Error!
```

Or, if you want to run one or more tests:

```lisp
(run :two ...)
```

## Results

`RESULTS` is much like `VALUES`; in fact, you can use `VALUES` instead
of `RESULTS`.  However, `RESULTS` calls `RESULT-TRANSLATE` on each
form as it occurs.  By default, this copies structures and sequences,
and marshals/unmarshals standard-objects.

With `VALUES`, the following will likely not be what you want:

```lisp
(defstruct thing (x 0))

(defun incr-thing (thing)
  (incf (thing-x thing))
  thing)

(check (:name :incr-thing)
  (let ((thing (make-thing)))
    (values
      thing
      (incr-thing thing)
      (incr-thing thing))))

      ;; => #S(.. 2), #S(.. 2), #S(.. 2)
```

However, with `RESULTS`, we get a copy each time:

```lisp
(check (:name :incr-thing)
  (let ((thing (make-thing)))
    (results
      thing
      (incr-thing thing)
      (incr-thing thing))))

      ;; => #S(.. 0), #S(.. 1), #S(.. 2)
```

Additionally, it can be very useful to override `RESULT-TRANSLATE`,
especially for complex objects:

```lisp
(defmethod checkl:result-translate ((thing thing))
  (thing-x thing))

(run :incr-thing) ;; => 0, 1, 2
```

This can be useful for checking selected values of deeply-nested
structures and objects.
      
## standard-object

Another option for standard-object instances is to implement
`ms:class-persistant-slots` (sic).  `RESULT-TRANSLATE` calls
`marshal`/`unmarshal` on objects by default, which makes a copy,
if this method is defined.

This method must merely return a list of slot names, and is trivial to
implement.  [See the documentation for
`cl-marshal`](https://github.com/wlbr/cl-marshal) for more details.

## Categories

So you've been writing a bunch of little tests and want to run them
all and see if anything has changed:

```lisp
(run-all)
```

Easy!  And you haven't had to specifically declare it so in three
places.  However maybe you want a bit more structure and split up your
tests when you run them all.  Thus categories:

```lisp
(check (:name :foo :category :some-category) ...)

(run-all :some-category ...)
```

The default category is perhaps unsurprisingly called `:default`.
That's pretty much all there is to categories.

## Remembering results

Since we're not *manually* defining the result, it would be
unfortunate if we *happened* to quit our lisp while our code still had
a bug, and then weren't sure what it was.  Easy enough:

```lisp
(checkl-store "some-file")
;;; - later -
(checkl-load "some-file")
```

This uses `cl-marshal` and `WRITE` to write values to the file
(overwriting it entirely).  `*PRINT-READABLY*` is forced to `t`, but
you can otherwise customize the output as per `WRITE`.

Along with revision control, it should be easy to keep track of test
results and make modifications.

## Formalizing

This is not meant to be a complete replacement for test suites such as
FiveAM, but more of a "deformalization".

But once you've worked on a bit of code and have your buffer cluttered
with `(check (...) ...)` forms, you probably don't want to rewrite
them all as FiveAM constructs.  It'd be nice if you could just sortof
integrate them all with minimal effort, like this:

```lisp
;; 5am doesn't have a find-suite, so you have to do this:
(defsuite :default)

(check-formal (:name :two) (foo))
```

Well, assuming you had FiveAM loaded before you loaded CheckL, this is
exactly what you do.  (I didn't want FiveAM to be a strict
dependency.)  Now you can do one of these, and they still do what they
should:

```lisp
(5am:run! :default) ;; => Pretty dots, one per VALUE
(run-all :default)  ;; => 2
```

We've gone from *very* informal testing to having things in FiveAM
with minimal effort!

Note that you can *still* eval the `CHECK-FORMAL` block in your buffer
and it behaves just like a `CHECK` block, returning its values and
signaling a condition if they change.

## ASDF

Wouldn't it be nice if ASDF loaded your saved CheckL values, and let
you call your newly-created FiveAM tests with minimal effort?  Of
course!

```lisp
(cl:eval-when (:load-toplevel :execute)
  (asdf:load-system :fiveam)
  (asdf:load-system :checkl))

(defsystem :my-system
  :description "A brand new system"
  ...)

(defsystem :my-system-tests
  :description "A system that tests"
  
  :depends-on (:fiveam :checkl)
  :serial t
  :pathname "t"

  :components ((:file "package")
               (checkl:tests "some-test")
               (checkl:test-values "test-values"
                                   :package :my-system-tests)))

(checkl:define-test-op :my-system :my-system-test)
(checkl:define-test-op :my-system-test)
```

That's all!  No long `PERFORM` definitions.  Just make sure to have
the `EVAL-WHEN` at the top.  Now you can do this:

```lisp
(asdf:load-system :my-system)
(asdf:test-system :my-system) ;; => (5am:run! :default)
```

Things of note:

* New ASDF component: `CHECK:TESTS` loads a file with `CHECK-FORMAL`
  tests, but does *not* run them.

* New ASDF component: `CHECK:TEST-VALUES` loads a file with test
  values.  It also sets the path to be the *default* for test values,
  so you can simply do `(checkl-store)` or `(checkl-load)`.

* `define-test-op SYSTEM &optional OTHER-SYSTEM`: This sets up the
  `ASDF:PERFORM` method for system to either run tests, or load
  another system and call `TEST-OP` on *it*.  If you're doing the
  latter, you need both definitions.

# Etc

I've been using this a bit and have added some conveniences.  There
could certainly be more.  Suggestions welcome, theoretical or
otherwise.
