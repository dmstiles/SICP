;; Exercise 2.4
;;
;; Description:
;;
;; A.
;;
;; Use the alternative procedural representation of pairs defined below
;; to verify that (car (cons x y)) yields `x` for any objects `x` and `y`.
;;
;; B.
;;
;; Implement the corresponding definition for `cdr`.



;; Definitions:

(define (cons x y)
  (lambda (m) (m x y)))
;Value: cons

(define (car z)
  (z (lambda (p q) p)))
;Value: car

(define (cdr z)
  (z (lambda (p q) q)))
;Value: cdr



;; Testing:

(define pair1 (cons 1 2))
;Value: pair1

(car pair1)
;Value: 1

(cdr pair1)
;Value: 2

(define pi 3.14)
;Value: pi

(define e 2.71)
;Value: e

(define pair2 (cons pi e))
;Value: pair2

(car pair2)
;Value: 3.14

(cdr pair2)
;Value: 2.71




	      