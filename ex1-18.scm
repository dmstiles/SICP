;; Exercise 1.18
;; Devise a procedure that generates an iterative process for multiplying two
;; integers in terms of adding, doubling, and halving that uses a 
;; logarithmic number of steps.


;; Definitions.

(define (even? n)
  ( = (remainder n 2) 0))
;Value: even?

(define (double n) (+ n n))
;Value: double

(define (halve n) (/ n 2))
;Value: halve

(define (mult a b)
  (define (mult-iter a b state)
    (cond ((= b 0) 0)
	  ((= b 1) (+ a state))
	  ((even? b) (mult-iter (double a) (halve b) state))
	  (else (mult-iter a (- b 1) (+ state a)))))
  (mult-iter a b 0))
;Value: mult


;; Testing.

(mult 2 3)
;Value: 6

(mult 2 4)
;Value: 8

(mult 3 3)
;Value: 9

(mult 13 13)
;Value: 169

(mult 1 0)
;Value: 0

(mult 0 10)
;Value: 0

(mult 15 6)
;Value: 90









