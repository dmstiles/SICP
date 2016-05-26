;; Exercise 1.5
;; The following is a test for determining the evaluation procedure
;; used by an interpreter. Explain the behavioral differences for the
;; following procedure when an interpreter uses applicative-order evaluation
;; versus when an interpreter uses normal-order evaluation.

(define (p) (p))
;Value: p

(define (test x y)
  (if (= x 0)
      0
      y))
;Value: test

(test 0 (p))

#|
When using applicative-order evaluation, as the LISP interpreter does, the
call to `test` will result in an infinite loop. This is due to the fact that
applicative-order evaluation follows an "evaluate the arguments and then apply"
method of evaluation. This method causes all arguments to the `test` function
to be evaluated. Since the second argument to the `test` function is a
call to an infinitely recursing function, then applicative-order evaluation will
be thrown into an infinite loop regardless of the result of the `if` predicate.
This is due to the fact that applicative-order evaluation attempts to evaluate all
arguments before applying the corresponding procedures.

In contrast, normal-order evaluation would simply replace the operands with the
function parameters, delaying their evaluation until an expression containing only
primitive operators was obtained. Therefore, the normal-order evaluation method would
not be thrown into infinite recursion as the consequent expression would not be evaluated
since the `if` predicate would evaluate to true based on the test arguments used.
|#
