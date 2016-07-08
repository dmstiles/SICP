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

(define (pos? x) (>= x 0))
;Value: pos?


(define (neg? x) (< x 0))
;Value: neg?

(define (mul-interval x y)
  (let ((lx (lower-bound x))
	(ux (upper-bound x))
	(ly (lower-bound y))
	(uy (upper-bound y)))
    (cond ((and (neg? lx) (neg? ux) (neg? ly) (neg? uy)) ;; [-,-] [-,-]
	   (make-interval (* ux uy) (* lx ly)))
	  ((and (neg? lx) (neg? ux) (neg? ly) (pos? uy)) ;; [-,-] [-,+]
	   (make-interval (* lx uy) (* lx ly)))
	  ((and (neg? lx) (neg? ux) (pos? ly) (pos? uy)) ;; [-,-] [+,+]
	   (make-interval (* lx uy) (* ux ly)))
	  ((and (neg? lx) (pos? ux) (neg? ly) (neg? uy)) ;; [-,+] [-,-]
	   (make-interval (* ux ly) (* lx ly)))
	  ((and (neg? lx) (pos? ux) (neg? ly) (pos? uy)) ;; [-,+] [-,+]
	   (make-interval (min (* lx uy) (* ux ly)) (max (* lx ly) (* ux uy))))
	  ((and (neg? lx) (pos? ux) (pos? ly) (pos? uy)) ;; [-,+] [+,+]
	   (make-interval (* lx uy) (* ux uy)))
	  ((and (pos? lx) (pos? ux) (neg? ly) (neg? uy)) ;; [+,+] [-,-]
	   (make-interval (* ux ly) (* lx uy)))
	  ((and (pos? lx) (pos? ux) (neg? ly) (pos? uy)) ;; [+,+] [-,+]
	   (make-interval (* ux ly) (* ux uy)))
	  ((and (pos? lx) (pos? ux) (pos? ly) (pos? uy)) ;; [+,+] [+,+]
	   (make-interval (* lx ly) (* ux uy))))))
;Value: mul-interval


;; Testing:

(define i1 (make-interval -10 -1))
;Value: i1

(define i2 (make-interval -10 10))
;Value: i2

(define i3 (make-interval 1 10))
;Value: i3

(mul-interval i1 i1)
;Value 21: (1 . 100)

(mul-interval i1 i2)
;Value 22: (-100 . 100)

(mul-interval i1 i3)
;Value 23: (-100 . -1)

(mul-interval i2 i1)
;Value 24: (-100 . 100)

(mul-interval i2 i2)
;Value 25: (-100 . 100)

(mul-interval i2 i3)
;Value 26: (-100 . 100)

(mul-interval i3 i1)
;Value 27: (-100 . -1)

(mul-interval i3 i2)
;Value 28: (-100 . 100)

(mul-interval i3 i3)
;Value 29: (1 . 100)




