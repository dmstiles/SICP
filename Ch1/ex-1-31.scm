;; Exercise 1.31
;;
;; Description:
;;
;; Write a `prodcut` procedure analogous to higher-order `sum` procedure defined
;; previously. The procedure should return the product of values over a given
;; range. Define the factorial function in terms of `product`. Also use `product` 
;; to compute pi using the Wallis product.
;;
;; If the `product` procedure defined generates a recursive process, write an
;; analagous iterative process, or vice versa.


;; Definitions:

(define (product term a next b)
  (if (> a b)
      1.0
      (* (term a)
	 (product term (next a) next b))))
;Value: product


(define (product-iter term a next b)
  (define (product-iter-helper x result)
    (if (> x b)
	result
	(product-iter-helper (next x)
			     (* (term x) result))))
    (product-iter-helper a 1.0))
;Value: product-iter


(define (identity x) x)
;Value: identity


(define (inc x) (+ x 1.0))
;Value: inc


(define (product-for-range a b)
  (product identity a inc b))
;Value: product-for-range

(define (product-for-range-iter a b)
  (product-iter identity a inc b))
;Value: product-for-range-iter


(define (fact x)
  (product identity 1 inc x))
;Value: fact

(define (wallis-pi n)
  (define (num i)
   (cond ((even? i) (+ i 2.0))
	  (else (+ i 3.0))))
  (define (den i)
    (cond ((even? i) (+ i 3.0))
	  (else (+ i 2.0))))
    (* 4.0
       (/ (product num 0.0 inc n)
	  (product den 0.0 inc n))))
;Value: wallis-pi



;; Testing:

(product-for-range 1 10)
;Value: 3628800.


(product-for-range-iter 1 10)
;Value: 3628800.


;; Solutions:

;; Implementing the the factorial function using the `product` procedure.

(fact 10)
;Value: 3628800.

(fact 4)
;Value: 24.

(fact 0)
;Value: 1.


;; Implementing the Wallis infinite product using the `product` procedure.
;;
;; Note - solution is not ideal in that `n` larger than approximately 170 will 
;; cause overflow as the product procedure grows too large. Also the margin of 
;; error is outside standard tolerance of a thousandth place.

(wallis-pi 150)
;Value: 3.1313428410856967

(wallis-pi 100)
;Value: 3.126379398042982

(wallis-pi 168)
;Value: 3.1324201790229074

(wallis-pi 169)
;Value: #[NaN]
