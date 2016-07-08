;; Exercise 2.10
;;
;; Description:
;;
;; Modify the `div-interval` procedure to check for a divisor which
;; spans zero, signaling an error if such a divisor is found.



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

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))
;Value: mul-interval

(define (div-interval x y)
  (if (and (< (lower-bound y) 0) (> (upper-bound y) 0))
       (error "Interval used as the divisor cannot span zero" y)
       (mul-interval x
		     (make-interval (/ 1.0 (upper-bound y))
				    (/ 1.0 (lower-bound y))))))
;Value: div-interval



;; Testing:

(define i1 (make-interval 1 11))
;Value: i1

(define i2 (make-interval -10 10))
;Value: i2

(define i3 (make-interval -11 -1))
;Value: i3

(div-interval i1 i2)
;Interval used as the divisor cannot span zero (-10 . 10)

(div-interval i1 i3)
;Value 13: (-11. . -.09090909090909091)

