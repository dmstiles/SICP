;; Exercise 2.7
;;
;; Description:
;;
;; Define the constructor and selectors `upper-bound` and `lower-bound` for
;; the interval abstraction.


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


;; Testing:

(define i (make-interval 1 10))
;Value: i

(lower-bound i)
;Value: 1

(upper-bound i)
;Value: 10

