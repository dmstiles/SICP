;; Exercise 2.12
;;
;; Description:
;;
;; Define a constructor `make-center-percent` that takes a center and a 
;; percentage tolerance and produces the desired interval. Also define a
;; selector `percent` that produces the percentage tolerance for a given
;; interval. Use the same `center` selector shown in the book.


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



;; Testing:
(define i (make-center-percent 5 0.2))
;Value: i

(center i)
;Value: 5.

(width i)
;Value: 1.

(percent i)
;Value: .2

