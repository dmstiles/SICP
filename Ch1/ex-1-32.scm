;; Exercise 1.32
;;
;; Descriptions:
;;
;; A.
;;
;; Show that sum and product are both special cases of a still more
;; general notion called `accumulate` that combines a collection of
;; terms, using some general accumulation function.
;;
;; B. 
;;
;; If the initial implementation of `accumulate` generates a recursive
;; process, write one generating an iterative process, and vice versa.


;; Definitions:

;; Recursive definition.
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) 
		(accumulate combiner null-value term (next a) next  b))))
;Value: accumulate

;; Iterative defintion.
(define (accumulate-iter combiner null-value term a next b)
  (define (iter x result)
    (if (> x b)
	result
	(iter (next x)
	      (combiner (term x) result))))
  (iter a null-value))
;Value: accumulate-iter


(define (sum-over-range a b)
  (accumulate + 0 identity a inc b))
;Value: sum-over-range


(define (product-over-range a b)
  (accumulate * 1 identity a inc b))
;Value: product-over-range


(define (sum-over-range-iter a b)
  (accumulate-iter + 0 identity a inc b))
;Value: sum-over-range-iter


(define (product-over-range-iter a b)
  (accumulate-iter * 1 identity a inc b))
;Value: product-over-range-iter


(define (inc x)
  (+ x 1))
;Value: inc


(define (identity x) x)
;Value: identity


;; Testing:

(sum-over-range 1 10)
;Value: 55

(product-over-range 1 10)
;Value: 3628800

(sum-over-range-iter 1 10)
;Value: 55

(product-over-range-iter 1 10)
;Value: 3628800

