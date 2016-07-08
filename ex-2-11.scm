;; Exercise 2.11
;;
;; Description:
;;
;; Rewrite the `mul-interval` procedure so that it decomposes the problem
;; into nine cases, only one of which requires more than two multiplications.



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


(define (pos? x) (> x 0))
;Value: pos?

(define (neg? x) (< x 0))
;Value: neg?

