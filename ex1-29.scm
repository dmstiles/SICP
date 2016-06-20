;; Exercise 1.29
;;
;; Description:
;;
;; Using Simpson's rule for approximating integrals, define a procedure
;; that takes as arguments `f`, `a`, `b`, and `n` and returns the value
;; of the integral.
;;
;; Use the procedure to integrate `cube` between 0 and 1 (with `n` = 100
;; and `n` = 1000), and compare the results to those of the `integral`
;; procedure defined in the book.

;; Definitions:

(define (inc x)
  (+ x 1.0))
;Value: inc


(define (square x)
  (* x x))
;Value: square


(define (cube x)
  (* x x x))
;Value: cube


(define (even? n)
  (= (remainder n 2) 0))
;Value: even?


(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))
;Value: sum


(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
;Value: integral


(define (simpson-integral f a b n)
  (define (g index)
    (f (+ a (* index (/ (- b a) n)))))
  (define (y i)
    (cond ((or (= i 0) (= i n)) (g i))
	  ((even? i) (* 2.0 (g i)))
	  (else (* 4.0 (g i)))))
  (* (/ (/ (- b a) n) 3.0)
     (sum y 0 inc n)))
;Value: simpson-integral


;; Testing:

(simpson-integral square 0 1 100)
;Value: .33333333333333337


;; Solution:

(integral cube 0 1 0.01)
;Value: .24998750000000042

(integral cube 0 1 0.001)
;Value: .249999875000001

(simpson-integral cube 0 1 100)
;Value: .24999999999999992

(simpson-integral cube 0 1 1000)
;Value: .2500000000000002








