;; Exercise 1.21
;;
;; Use the smallest-divisor procedure to find the smallest divisor
;; of the following numbers: 199, 1999, 19999.


;; Definitions.

(define (smallest-divisor n)
  (find-divisor n 2))
;Value: smallest-divisor

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))
;Value: find-divisor

(define (square n)
  (* n n))
;Value: square

(define (divides? a b)
  (= (remainder b a) 0))
;Value: divides?


;; Testing.

(smallest-divisor 4)
;Value: 2

(smallest-divisor 9)
;Value: 3

(smallest-divisor 101)
;Value: 101


;; Solution.

(smallest-divisor 199)
;Value: 199

(smallest-divisor 1999)
;Value: 1999

(smallest-divisor 19999)
;Value: 7
