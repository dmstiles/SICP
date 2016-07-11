;; Exercise 2.16
;;
;; Description:
;;
;; Explain in general, why equivalent algebraic expressions may lead to different
;; answers. Is it possible to devise an interval-arithmetic package that does not
;; have this shortcoming?



;; Definitions:

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))
;Value: par1

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))
;Value: par2

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

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))
;Value: add-interval

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))
;Value: mul-interval

(define (div-interval x y)
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))
;Value: div-interval

;; Solution:
;;
;; In general, algebraic manipulations suppose inifinte precision in reasoning
;; about the relationship amongst quantities. Therefore even slight
;; uncertainties inherent in an interval data type breaks this precision model,
;; introducing error as algebraic manipulations are carried out to transform
;; equations. Concretly, in the case of the `par1` and `par2` procedures, the 
;; manipulation to achieve identical forms involves a multplication to the
;; numerator and denominator of some quantity involving `r1` and `r2`. Since
;; `r1` and `r2` are inexact quantities their self division is not one, but an
;; approximation to one, therefore rendering the resulting transformation inaccurate.
;; This issue is what causes the `par1` and `par2` procedures to return different
;; results as seen below.

(define i1 (make-center-percent 10 0.1))
;Value: i1

(define i2 (make-center-percent 2 0.5))
;Value: i2

(define r1 (par1 i1 i2))
;Value: r1

(center r1)
;Value: 1.9714285714285715

(percent r1)
;Value: .6739130434782609

(define r2 (par2 i1 i2))
;Value: r2

(center r2)
;Value: 1.6285714285714286

(percent r2)
;Value: .44736842105263164

;; Note the result of the self division by an interval is not one.
(center (div-interval i1 i1))
;Value: 1.02020202020202

;; This effect is proportional to the uncertainty (width) of the interval.
(center (div-interval i2 i2))
;Value: 1.6666666666666667


;; In conlusion, an interval based arithmetic package that correct even across
;; new equations acheived through algebraic manipulation is general not possible
;; as long as the quantities are represented with uncertainty, which is essentially
;; any interval since the data type's purpose is to span a range. Therefore the
;; best application of an interval arithmetic package is one which avoids the 
;; repitition of variables representing intervals in order to reduce the compounding
;; effect of their respective uncertainties when combined.