;; Exercise 1.24
;;
;; Description.
;;
;; Modify the 'timed-prime-test' of exercise 1.22 to use `fast-prime?`
;; which uses the Fermat probabilistic method. Test each of the 12 primes
;; found in that exercise. Since the Fermat method has log(n) growth, how
;; should the time to test primes near 1,000,000 compare to the time to
;; test primes near 1000. Does the timing bear this out? Provide explanations
;; for any discrepencies if needed.


;; Definitions.

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m)) m))
	(else
	 (remainder (* base (expmod base (- exp 1) m)) m))))
;Value: expmod


(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
;Value: fermat-test


(define (fast-prime? n times)
  (cond ((= n 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))
;Value: fast-prime?


(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
;Value: timed-prime-test


(define (start-prime-test n start-time)
  (if (fast-prime? n)
      (report-prime (- (runtime) start-time))))
;Value: start-prime-test


(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
;Value: report-prime


(define (square n)
  (* n n))
;Value: square


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

(expmod 2 4 3)
;Value: 1

(fast-prime? 1000003 10)

;; Solution.

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
