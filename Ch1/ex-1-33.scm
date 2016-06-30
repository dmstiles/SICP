;; Exercise 1.33
;;
;; Description:
;;
;; Obtain a more generalized version of `accumulate` by introducing a
;; `filter` predicate that determines which values in a given range
;; will be combined. Write `filtered-accumulate` as a procedure.
;;
;; Express the following using `filtered-accumulate`
;;
;; A. 
;;
;; The sum of squares of the prime numbers in interval a to b.
;;
;; B.
;;
;; The product of all positive integers that are relatively prime
;; to `n`. That is, all positive integers `i` < `n` such that 
;; GCD(i,n) = 1


;; Definitions.

(define (filtered-accumulate combiner null-value term a next b filter?)
  (define (iter a result)
    (cond ((> a b) result)
	  ((filter? a) (iter (next a) (combiner (term a) result)))
	  (else (iter (next a) result))))
  (iter a null-value))
;Value: filtered-accumulate


(define (prime? n)
  (= n (smallest-divisor n)))
;Value: prime?


(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
;Value: gcd


(define (smallest-divisor n)
  (find-divisor n 2))
;Value: smallest-divisor


(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? n test-divisor) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))
;Value: find-divisor


(define (divides? a b)
  (= (remainder a b) 0))
;Value: divides?


(define (identity x) x)
;Value: identity

(define (inc x) (+ x 1))
;Value: inc


(define (sum-of-prime-squares a b)
  (filtered-accumulate + 0 square a inc b prime?))
;Value: sum-of-prime-squares


(define (product-of-relative-primes a b)
  (define (relative-prime? x)
    (= 1 (gcd x b)))
  (filtered-accumulate * 1 identity a inc b relative-prime?))
;Value: product-of-relative-primes


(prime? 9)
;Value: #f

(prime? 13)
;Value: #t

(sum-of-squares 3 4)
;Value: 25

(sum-of-prime-squares 3 4)
;Value: 9

(sum-of-prime-squares 3 5)
;Value: 34

(sum-of-prime-squares 8 13)
;Value: 290

(gcd 8 15)
;Value: 1

(relative-prime? 8 15)
;Value: #t

(relative-prime? 10 15)
;Value: #f

(product-of-relative-primes 2 9)
;Value: 2240

(product-of-relative-primes 5 15)
;Value: 112112

