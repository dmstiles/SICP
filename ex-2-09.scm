;; Exercise 2.9
;;
;; Description:
;;
;; Show that the width of the sum or difference of two intervals
;; is a function only of the widths of the two intervals being added
;; or subtracted. Also give examples to show that this is not true
;; for multiplication or division.


;; Definitions:

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

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
		 (- (upper-bound x) (upper-bound y))))
;Value: sub-interval

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

(define (width-interval interval)
  (/ (- (upper-bound interval) (lower-bound interval))
	   2))
;Value: width-interval

	    
;; Testing:

(width-interval (make-interval 0 10))
;Value: 5


;; Solution:

(define i1 (make-interval 1 3))
;Value: i1

(define i2 (make-interval 1 5))
;Value: i2

;; Showing that the the sum of two intervals is equal
;; to the sum of the widths.

(width-interval (add-interval i1 i2))
;Value: 3

(+ (width-interval i1)
   (width-interval i2))
;Value: 3

;; Showing that the difference between two intervals 
;; is equal to the absolute value of the difference 
;; between the widths.

(width-interval (sub-interval i1 i2))
;Value: 1

(abs (- (width-interval i1)
        (width-interval i2)))
;Value: 1

;; Showing that the product of two interval is not equal
;; to the product of the widths.

(width-interval (mul-interval i1 i2))
;Value: 7

(* (width-interval i1)
   (width-interval i2))
;Value: 2

;; Showing that the quotient of two intervals is not
;; equal to the quotient of the two widths.

(width-interval (div-interval i3 i4))
;Value: 2.4285714285714284

(/ (width-interval i3)
   (width-interval i4))
;Value: 2/3

