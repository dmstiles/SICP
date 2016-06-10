;; Exercise 1.21
;;
;; Write a procedure `search-for-primes` that checks the primality of
;; consecutive odd integers in a specified range. Use the procedure
;; to find the three smallest primes larger than 1000; larger than 10,000
;; larger than 100,000; larger than 1,000,000. Not the time needed to test
;; each prime. Since the tesint algorithm has a sqrt(n) order of growth,
;; testing primes around 10,000 should be 10x as long as testing primes
;; around 1000.

;; Questions. 
;;
;; A. Does the timing data support the prediction made above?
;;
;; B. How well does the data for 100,000 and 1,000,000 support the sqrt(n)
;;    predictions?
;;
;; C. Is the results compatible with the notion that programs run in time
;;    proportional to the number of steps required for computation?


;; Definitions.

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
;Value: timed-prime-test


(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
;Value: start-prime-test


(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
;Value: report-prime


(define (smallest-divisor n)
  (find-divisor n 2))
;Value: smallest-divisor


(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))
;Value: find-divisor


(define (divides? a b)
  (= (remainder b a) 0))
;Value: divides?

(define (square n)
  (* n n))
;Value: square


(define (prime? n)
  (= (smallest-divisor n) n))
;Value: prime?


(define (search-for-primes n max)
  (if (< n max) ((timed-prime-test n)
		 (search-for-primes (+ n 1)
				    max))))
;Value: search-for-primes


;; Testing.

