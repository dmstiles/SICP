;; Exercise 2.8
;;
;; Description:
;;
;; Define the `sub-interval` procedure analagous to the definition
;; for `add-interval` procedure presented in the book.


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


;; Testing:

(define i1 (make-interval 1 3))
;Value: i1

(define i2 (make-interval 2 4))
;Value: i2

(sub-interval i1 i2)
;Value 14: (-1 . -1)

(sub-interval i2 i1)
;Value 15: (1 . 1)

