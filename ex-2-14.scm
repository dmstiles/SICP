;; Exercise 2.14
;;
;; Description:
;;
;; Demsonstrate that the two methods for calculating parallel resistance below
;; yield different results although they are algebraically equal.



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

;; Testing:

(define i1 (make-center-percent 5 0.2))
;Value: i1

(define i2 (make-center-percent 10 0.1))
;Value: i2

(define one (make-interval 1.0 1.0))
;Value: one

(center one)
;Value: 1.

(percent one)
;Value: 0.

(center (div-interval i1 i1))
;Value: 1.0833333333333333

(percent (div-interval i1 i1))
;Value: .38461538461538464

(div-interval i1 i1)
;; (.6666666666666666 . 1.5)

(center (div-interval i1 i2))
;Value: .5151515151515151

(percent (div-interval i1 i2))
;Value: .2941176470588235

(div-interval i1 i2)
;; (.36363636363636365 . .6666666666666666)



;; Solution:
;;
;; As noted  the two formulas return different results for the same intervals.

(center (par1 i1 i2))
;Value: 3.5972850678733037

;Value: 3.5972850678733037

(percent (par1 i1 i2))
;Value: .41132075471698115

(center (par2 i1 i2))
;Value: 3.3257918552036196

(percent (par2 i1 i2))
;Value: .1673469387755102
