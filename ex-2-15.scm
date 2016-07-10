;; Exercise 2.15
;;
;; Description:
;; 
;; Explain why the `par2` procedure is a better approximation 
;; to the actual resistance between two parallel resistors.


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
;; Since the `par2` procedure does not perform any operations in which both
;; operands involve uncertainties, its result achieves a much lower margin of
;; error. Contrast this with the `par1` procedure which performs three separate
;; operations in which both operands possess margins of error giving a result
;; whose answer contains a marign of error that was compounded in each of 
;; these three operations. The conclusion being that each operation involving
;; uncertainties returns output that is resultant of the respective uncertainty 
;; in its inputs.

(define i1 (make-center-percent 50 0.2))
;Value: i1

(define i2 (make-center-percent 100 0.1))
;Value: i2

;; Measure of uncertainty for `par1` procedure.
(percent (par1 i1 i2))
;Value: .4113207547169812

;; Measure of uncertainty for `par2` procedure.
(percent (par2 i1 i2))
;Value: .1673469387755103









