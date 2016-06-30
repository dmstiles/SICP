;; Exercise 1.43
;;
;; Description:
;;
;; Write a procedure `repeated` that takes as inputs a procedure that computes `f`
;; and a positive integer `n` and returns the procedure that computes the `n`th
;; repeated application of `f`.



;; Definitions:

(define (repeated f n)
  (define (repeated-iter g count)
    (if (= count n)
	g
	(repeated-iter (compose f g) (inc count))))
  (repeated-iter f 1))
;Value: repeated


(define (compose f g)
  (lambda (x) (f (g x))))
;Value: compose


(define (sqaure x)
  (* x x))
;Value: sqaure

(define (inc x)
  (+ x 1))
;Value: inc


;; Testing:

((repeated square 2) 5)
;Value: 625


