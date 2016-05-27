;; Exercise 1.6
;; Reasoning about the difference between `if` and `cond` expressions.


;; Defining a procedure for finding square roots using Newton's method of successive approximations.

(define (square x) (* x x))
;Value: square

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
;Value: good-enough?

(define (average x y)
  (/ (+ x y) 2))
;Value: average

(define (improve guess x)
  (average guess (/ x guess)))
;Value: improve

(define (sqrt x)
  (sqrt-iter 1.0 x))
;Value: sqrt

;; Testing

(sqrt 9)
;Value: 3

(sqrt (+ 100 37))
;Value: 11.704699910719626

(sqrt (+ (sqrt 2) (sqrt 3)))
;Value: 1.7737712281864233

(square (sqrt 1000))
;Value: 1000.


;; Consider the new definition of special form `if` as an ordinary procedure using `cond` statement.

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))
;Value: new-if

;; Testing

(new-if (= 2 3) 0 5)
;Value: 5

(new-if (= 1 1) 0 5)
;Value: 0


;; Consider the following redefinition of the `sqrt-iter` procedure using the new `if` procedure.
;; What happens when the following attempts to compute square roots?

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
	  guess
	  (sqrt-iter (improve guess x) x)))
;Value: sqrt-iter


(sqrt 3)
;Aborting!: maximum recursion depth exceeded

;; In a `cond` statement the interpreter will attempt to evaluate the consequent expression
;; of each predicate in the case analysis. This attempted evaluation will result in a recursion
;; depth limit due to the fact that calls to the `sqrt-iter` procedure will continue indefinitely
;; regardless of whether or not the `good-enough?` predicate has evaluated to true.

