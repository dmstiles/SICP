;; Exercise 1.30
;;
;; Description:
;;
;; The given sum procedure generates linear recursion. The procedure can
;; be rewritten so that the sum is performed iteratively. Show how to do
;; this by using the given procedure template.

;; Definitions:

(define (sum term a next b)
  (define (iter x result)
    (if (> x b)
	result
	(iter (next x) (+ (term x) result))))
  (iter a 0))
;Value: sum


(define (identity x) x)
;Value: identity


(define (inc a) (+ a 1))
;Value: inc


(define (sum-integers a b)
  (sum identity a inc b))
;Value: sum-integers

;; Solution:

(sum-integers 1 10)
;Value: 55





