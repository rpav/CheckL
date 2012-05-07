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

    |
    v

Result 0 has changed: 3
Previous result: 2
   [Condition of type CHECKL::RESULT-ERROR]

Restarts:
 0: [USE-NEW-VALUE] The new value is correct, use it from now on.
 1: [SKIP-TEST] Skip this, leaving the old value, but continue testing
 2: [RETRY] Retry SLIME interactive evaluation request.
 3: [*ABORT] Return to SLIME's top level.
 4: [TERMINATE-THREAD] Terminate this thread (#<THREAD "worker" RUNNING {100586AB13}>)
```

[See the full documentation for more details!](http://rpav.github.com/CheckL)
