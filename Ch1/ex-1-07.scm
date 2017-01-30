;; Exercise 1.7


;; Definitions:

;; Design a square root procedure that watches how `guess` changes between iterations, terminating 
;; when the change is a very small fraction of the guess.

(define (sqrt-iter old-guess guess x)
  (if (good-enough? old-guess guess x)
      guess
      (sqrt-iter guess (improve guess x) x)))
;Value: sqrt-iter

(define (good-enough? old-guess new-guess x)
  (< (abs (- old-guess new-guess)) 0.001))
;Value: good-enough?


(define (square x) (* x x))
;Value: square


(define (average x y)
  (/ (+ x y) 2))
;Value: average


(define (improve guess x)
  (average guess (/ x guess)))
;Value: improve


(define (sqrt x)
  (sqrt-iter 0.0 1.0 x))
;Value: sqrt


;; The original `good-enough?` procedure will not be effective for very small numbers.
;; Also limited precision in computers makes the test inadequate for very large numbers.
;; Explain these statements and design tests to highlight these behaviors.

;; Large numbers.
(sqrt 2e20)

;; The procedure above will never return since the precision maintained for a number of
;; this magnitude is above level of precision required to meet the `good-enough?` predicate.

;; Small Numbers.
(sqrt 2e-2)
;;Approx: .1444238094866232
;;Actual: 0.1414213562
;;Ratio: 1.021

(sqrt 2e-3)
;;Approx: 5.0131352980478244e-2
;;Actual: 0.04472135955
;;Ratio: 1.121

;; The two examples above show that as the argument approaches zero the ratio between the
;; approximate and actual answers grows to reflect the increasing margin of error in the approximations.
;; This difference in answers is due to the predetermined tolerance of 0.001 defined in the `good-enough?`
;; procedure, which allows processing to terminate before an accurate value at this scale is found.



;; Testing.

;; Small numbers.
(sqrt 2e-2)
;Value: .14142135968022695

(sqrt 2e-3)
;Value: .04472230608683239

;; Large numbers.
(sqrt 2e20)
;Value: 14142135623.73095

;; The redefinition of `good-enough?` allows both small and large square roots to be evaluated
;; using Newton's method of successive approximation. In the large number case the termination upon
;; unchanging guesses prevents the infinite recursion caused by the limited numeric precision when
;; calculating the difference between the guess and the square in the `good-enough?` procedure. In
;; the small number case this redefinition allows the square root of numbers close to zero to be
;; evaluated more accurately since succession continues until the guess is unchanging rather than
;; terminating when the guess is within a large, by comparison, predefined tolerance.

(sqrt 2e40)
;Value: 1.4142135623730951e20


(sqrt 2e-40)
;Value: .0009765625

;; This redefinition works better for larger numbers since the `good-enough?` procedure's
;; execution is terminated once the delta between guesses has become sufficiently small.
;; This property prohibits the `good-enough?` procedure for progressing toward smaller, more
;; accurate, guesses since a sufficently small guess will appear too similar to the previous
;; guess as determined by the specified tolerance causing the procedure to return prematurely.
