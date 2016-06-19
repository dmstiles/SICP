;; Exercise 1.28
;;
;; Description:
;;
;; Modify the `expmod` procedure  to signal if it discovers a nontrivial
;; square root of 1, and use this to implement the Miller-Rabin test with
;; a procedure analogous to `fermat-test`. Check the procedure by testing
;; various known primes and non-primes.

;; Definitions:

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp) 
	 (non-trivial-root-filter (expmod base (/ exp 2) m) m))
	(else
	 (remainder (* base (expmod base (- exp 1) m)) m))))
;Value: expmod


(define (non-trivial-root-filter value n)
  (cond	((or (= value 1) 
	     (= value (- n 1))) 
	 (remainder (square value) n))
	(else non-trivial-root-test (remainder (square value) n))))
;Value: non-trivial-root-filter


(define (non-trivial-root-test mod)
  (if (= mod 1)
      0
      mod))
;Value: non-trivial-root-test


(define (miller-rabin-test? n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))
;Value: miller-rabin-test?

;; Test congruence of ((a^n)%n) with 1, denoting prime.
(define (fast-prime? n times)
  (cond ((= times 0) true)
	((miller-rabin-test? n)) (fast-prime? n (- times 1))
	(else false)))
;Value: fast-prime?


(define (square n)
  (* n n))
;Value: square


(define (even? n)
  (= (remainder n 2) 0))
;Value: even?


;; Testing:
(expmod 2 3 4)
;Value: 0

(expmod 8 9 10)
;Value: 8

(expmod 77 99 100)
;Value: 13

(expmod 12 100 101)
;Value: 1

(expmod 4 12 13)
;Value: 1

(expmod 55 112 113)
;Value: 1

(expmod 120 560 561)
;Value: 375

(expmod 118 560 561)
;Value: 1


;; Solution:
;;
;; Implementing a the `fast-prime?` procedure to use the Miller-Rabin test
;; in order to solidify the procedure against the Carmichael numbers.

(fast-prime? 10 100)
;Value: #t


