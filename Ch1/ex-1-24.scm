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
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))
;Value: fast-prime?


(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
;Value: timed-prime-test


(define (start-prime-test n start-time)
  (if (fast-prime? n 100)
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
;Value: #t

(search-for-primes 100 110)

101 *** 0.
103 *** 0.
105 *** 0.
107 *** 0.
109 *** 0.
Done
;Unspecified return value 


;; Solution.

(search-for-primes 1000000 1001000)
;; 1000003 => 0
;; 1000033 => 0
;; 1000037 => 0

(search-for-primes 10000000 10001000)
;; 10000019 => 0
;; 10000079 => 0
;; 10000103 => 0

(search-for-primes 100000000 100001000)
;; 100000007 => 0
;; 100000037 => 0
;; 100000039 => 0

(search-for-primes 1000000000 1000001000)
;; 1000000007 => 0
;; 1000000009 => 0
;; 1000000021 => 0

(search-for-primes 10000000000 10000001000)
;; 10000000019 => 0
;; 10000000033 => 0
;; 10000000061 => 0

;; Previous test entries are too small of magnitude for new algorithm
;; with log(n) growth. Since the original implementation of the new Fermat
;; probabilistic approach was being used with a repition factor of 1
;; that repitition factor has now been increased to 100 in order to extend
;; the execution time, as well as increase the confidence in the result.
;; 
;; The following tests use this new implementation with values doubling in
;; magnitude for each subsequent test in order to observe the runtime growth
;; for the Fermat probabilistic algorithm.

;; 1e11
(search-for-primes 100000000000 100000001000)
;; 100000000003 *** 1.0000000000001563e-2
;; 100000000019 *** 1.9999999999999574e-2
;; 100000000057 *** 1.9999999999999574e-2

;; 1e22
(search-for-primes 10000000000000000000000 10000000000000000001000)
;; 10000000000000000000009 *** 2.9999999999997584e-2
;; 10000000000000000000057 *** 3.0000000000001137e-2
;; 10000000000000000000081 *** 2.9999999999997584e-2

;; 1e44
(search-for-primes 100000000000000000000000000000000000000000000 100000000000000000000000000000000000000001000)
;; 100000000000000000000000000000000000000000031 *** .07000000000000028
;; 100000000000000000000000000000000000000000057 *** .07000000000000028
;; 100000000000000000000000000000000000000000309 *** .05999999999999872


;; From the proceeding tests one can observe the algorithmic runtime growth
;; for the `fast-prime?` procedure using the Fermat probabilistic method.
;; As the order of magnitude doubled in each subsequent tests the runtime
;; growth also doubled reflecting the predicted logrithmic order of growth.
;; This shows that the size of the input must double its order of magnitude
;; in order to double the time required to operate on this input. Such growth
;; is a stark contrast to the previous sqrt(n) growth used in the `prime?`
;; procedure where an input value 4 times as large was enough to double
;; the runtime duration, since sqrt(4*n) => 2*sqrt(n)