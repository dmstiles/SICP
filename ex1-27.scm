;; Exercise 1.27
;;
;; Question:
;;
;; Demonstrate the Carmichael numbers listed in the book really
;; do fool the Fermat test by writing procedure that takes an integer
;; `n` and tests whether or not a^n is congruent to a % n for every
;; a < n
;;
;; Then try this procedure for the aforementioned Carmichael numbers.


;; Definitions:

(define (fermat-prime? n)
  (define (fermat-prime-iter a n)
    (cond ((= a n) true)
	  ((= (expmod a n n) (remainder a n)) (fermat-prime-iter (+ a 1) n))
	  (else false)))
  (fermat-prime-iter 0 n))
;Value: fermat-prime?


(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	  (remainder (square (expmod base (/ exp 2) m)) m))
	(else
	  (remainder (* base (expmod base (- exp 1) m)) m))))
;Value: expmod


;; Testing:
(fermat-prime? 10)
;Value: #f

(fermat-prime? 31)
;Value: #t

(fermat-prime? 100)
;Value: #f

(fermat-prime? 101)
;Value: #t


;; Solution:
;;
;; Testing the `fermat-prime?` procedure with the Carmichael numbers.

(fermat-prime? 561)
;Value: #t

(fermat-prime? 1105)
;Value: #t

(fermat-prime? 1729)
;Value: #t

(fermat-prime? 2465)
;Value: #t

(fermat-prime? 2821)
;Value: #t

(fermat-prime? 6601)
;Value: #t


