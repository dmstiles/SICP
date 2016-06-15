;; Exercise 1.23
;;
;; Define a procedure next that returns 3 if its input is equal to
;; else return the input plus 2. Modify the smallest-divisor procedure
;; to use the newly define `next` procedure. Run the `timed-prime-test`
;; for the 12 primes found in exercise 1.22


;; Questions. 
;;
;; A. Since this procedure halves the number of test steps, it should run
;;    about twice as fast, is the prediction confirmed?
;;
;; B. How do you explain the fact that the runtime ratio is different
;;    from 2?


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
	(else (find-divisor n (next test-divisor)))))
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


(define (even? n)
  (= (remainder n 2) 0))
;Value: even?


(define (next n)
 (if (= n 2)
     3
     (+ n 2)))
;Value: next


(define (search-for-primes min max)
  (cond ((= min max) (newline) (display "Done"))
	((even? min) (search-for-primes (+ min 1) max))
	(else (timed-prime-test min)
	      (search-for-primes (+ min 1) max))))
;Value: search-for-primes


;; Testing.
(smallest-divisor 10)
;Value: 2

(smallest-divisor 13)
;Value: 13

(smallest-divisor 9)
;Value: 3

(prime? 5)
;Value: #t

(prime? 25)
;Value: #f


(search-for-primes 1 10)

1 *** 0.
3 *** 0.
5 *** 0.
7 *** 0.
9
Done
;Unspecified return value


;; Solution.
;;
;; Note - As of 2008 computers are too fast to realize the difference in testing
;; primality of small numbers, therefore tests here have been scaled to magnitudes
;; starting at 1 million.

(search-for-primes 1000000 1001000)
;; Unoptimized
;; 1000003 => 0
;; 1000033 => 0
;; 1000037 => 0
;; Optimized
;; 1000003 => 0
;; 1000033 => 0
;; 1000037 => 0.009

(search-for-primes 10000000 10001000)
;; Unoptimized
;; 10000019 => 0
;; 10000079 => 0.01
;; 10000103 => 0
;; Optimized
;; 10000019 => 0
;; 10000079 => 0
;; 10000103 => 0.01

(search-for-primes 100000000 100001000)
;; Optimized
;; 100000007 => 0.02
;; 100000037 => 0.02
;; 100000039 => 0.02
;; Unoptimized
;; 100000007 => 0.02
;; 100000037 => 0.01
;; 100000039 => 0.02

(search-for-primes 1000000000 1000001000)
;; Optimized
;; 1000000007 => 0.06
;; 1000000009 => 0.06
;; 1000000021 => 0.06
;; Unoptimized
;; 1000000007 => 0.04
;; 1000000009 => 0.04
;; 1000000021 => 0.04

(search-for-primes 10000000000 10000001000)
;; Optimized
;; 10000000019 => 0.2
;; 10000000033 => 0.19
;; 10000000061 => 0.19
;; Unoptimized
;; 10000000019 => 0.12
;; 10000000033 => 0.12
;; 10000000061 => 0.12


;; Solutions.
;;
;; A. In the observed results the ratios begin to differ primarily at 
;;    a magnitude around 1 billion where the unoptimized algorithm
;;    required about 0.06 seconds to test the arguments primarlity,
;;    and the optimized version take about 0.04 seconds. At a magnitude
;;    of 10 billion the ratio continues to decrease approaching the
;;    theoretical prediction of a runtime half that of the unoptimized
;;    algorithm with the unoptimized version requiring 0.2 seconds and
;;    the optimized version requiring 0.12 seconds approximately 40%
;;    faster than the unoptimized version.
;;
;; B. This conflict between the predicted and actual runtimes
;;    is caused by the if statement introduced as part of the
;;    optimization halving the number of tested divisors. The
;;    work required by the if statement, especially when comparing
;;    large numbers introduced computational overhead to the ideal
;;    runtime halving predicted in theory.