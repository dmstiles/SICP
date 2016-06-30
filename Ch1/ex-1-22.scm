;; Exercise 1.22
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

(define (even? n)
  (= (remainder n 2) 0))
;Value: even?


(define (search-for-primes min max)
  (cond ((= min max) (newline) (display "Done"))
	((even? min) (search-for-primes (+ min 1) max))
	(else (timed-prime-test min)
	      (search-for-primes (+ min 1) max))))
;Value: search-for-primes


;; Testing.

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
;; 1000003 => 0
;; 1000033 => 0
;; 1000037 => 0

(search-for-primes 10000000 10001000)
;; 10000019 => 0
;; 10000079 => 0.01
;; 10000103 => 0

(search-for-primes 100000000 100001000)
;; 100000007 => 0.02
;; 100000037 => 0.02
;; 100000039 => 0.02

(search-for-primes 1000000000 1000001000)
;; 1000000007 => 0.06
;; 1000000009 => 0.06
;; 1000000021 => 0.06

(search-for-primes 10000000000 10000001000)
;; 10000000019 => 0.2
;; 10000000033 => 0.19
;; 10000000061 => 0.19


;; Although the sequence of numbers tested above grew by a magnitude of 10 with
;; each subsequent search, it can be seen that the time required to test such
;; numbers only grew by an order of somewhere roughly around 3, or between 2 and 4 here. 
;; Since the `prime?` procdure used in the primality test displays a sqrt(n) 
;; order of growth the results obtained above seem to confirm the notion
;; that the time required for programs run on a machine are proportional to the
;; steps required in the computation. More plainly, since the number tested in each
;; test grew by an order of 10, the order or growth for the `prime?` procedure
;; should theoretically be sqrt(10), which is approximately 3.16, a value that
;; corresponds directly to the observed runtime growth of the tested numbers.
