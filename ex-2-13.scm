;; Exercise 2.13
;;
;; Description:
;;
;; Show that under the assumptions of small percentage tolerances
;; this is a simple formula for approximating the percentage tolerance
;; of the product of two intervals in terms of tolerances of the factors.
;; This problem may be simplified by assuming that all numbers are positive.


;; Definitions:

(define (make-center-percent c p)
  (let ((w (* c p)))
    (make-interval (- c w) (+ c w))))
;Value: make-center-percent

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
;Value: center

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
;Value: width

(define (percent i)
  (/ (width i) (center i)))
;Value: percent

(define (make-interval a b) (cons a b))
;Value: make-interval

(define (upper-bound interval)
  (max (car interval)
       (cdr interval)))
;Value: upper-bound

(define (lower-bound interval)
  (min (car interval)
       (cdr interval)))
;Value: lower-bound

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))
;Value: mul-interval


;; Solution:
;;
;; As defined above an interval can be expressed as follows.
;;
;;     i = [c - c(p), c + c(p)]
;;
;; Which states that an interval is defined by its bounds given
;; by the center and a distance expressed as a percent tolerance.
;; The percent tolerance is the ratio of the width of interal to the
;; intervals midpoint.
;;
;; Therefore the product of two interavls, for positive-only intervals,
;; is the following product.
;;
;;     [a,b] * [c,d] = [ac, bd]
;;
;; Expanding this out into component parts yields
;;
;;     [c1 - (c1)(p1), c1 + (c1)(p1)] * [c2 - (c2)(p2), c2 + (c2)(p2)]
;;     
;;     => [(c1)(c2) - (c1)(c2)(p2) - (c1)(c2)(p1) + (c1)(c2)(p1)(p2),
;;         (c1)(c2) + (c1)(c2)(p2) + (c1)(c2)(p1) + (c1)(c2)(p1)(p2)]
;;
;; Factoring out the (c1)(c2) term in each expressions yields
;;
;;    [(c1)(c2)(1 - p2 - p1 + (p1)(p2)), (c1)(c2)(1 + p2 + p1 + (p1)(p2))]
;;
;; Note that working under the assumption of small percentage tolerances, the
;; product of the two percentages in both endpoints will give a results orders 
;; of magnitude less than the other terms in the equations, thus these terms can
;; be ignored with little affect on the approximate result.
;;
;; Removing this product gives the form
;;
;;    [(c1)(c2)(1 - p2 - p1), (c1)(c2)(1 + p2 + p1)]
;;
;; Factoring out the negative in the lower bound, and rearranging the sum
;; between the two percentage terms into their own expression gives
;;
;;    [(c1)(c2)(1 - (p1 + p2)), (c1)(c2)(1 + (p1 + p2))]
;;
;; Multiplying the product between the two centers back into the expression
;; involving the percentages yields a from similar to the one presented initially.
;;
;; [(c1)(c2) - (c1)(c2)(p1 + p2), (c1)(c2) + (c1)(c2)(p1 + p2)]
;;
;; This forms shows that the simple formula for calculating percent tolerance is
;; merely the sum of the two individual percent tolerances.
;;
;;    pz = px + py    
;;
;; Where pz is the percent tolerance of the product.


;; Showing the preceding argument as an example.

(define i1 (make-center-percent 5 0.20))
;Value: i1

(define i2 (make-center-percent 10 0.1))
;Value: i2

(percent i1)
;Value: .2

(percent i2)
;Value: .1

(percent (mul-interval i1 i2))
;Value: .29411764705882354

