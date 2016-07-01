;; Exercise 2.2
;;
;; Description:
;;
;; A.
;;
;; Define a constructor `make-segment` and selectors `start-segment` and
;; `end-segment` that define the representation of segments in terms of
;; points. 
;;
;; B.
;; 
;; Also specify a constructor `make-point` and selectors `x-point`
;; and `y-point` that define the representation of a point in an 2D
;; coordinate system. 
;;
;; C.
;;
;; Finally, specify a procedure `midpoint-segment` that takes a line
;; segment as an argument and returns its midpoint.



;; Definitions:

(define (make-point x y) (cons x y))
;Value: make-point


(define (x-point p) (car p))
;Value: x-point


(define (y-point p) (cdr p))
;Value: y-point


(define (make-segment p1 p2) (cons p1 p2))
;Value: make-segment


(define (start-segment s) (car s))
;Value: start-segment


(define (end-segment s) (cdr s))
;Value: end-segment


(define (midpoint-segment s)
  (make-point (average (x-point (start-segment s)) (x-point (end-segment s)))
	      (average (y-point (start-segment s)) (y-point (end-segment s)))))
;Value: midpoint-segment


(define (average x y) (/ (+ x y) 2))
;Value: average


(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))
;Value: print-point



;; Testing:

(print-point (make-point 0 0))
(0,0)

(make-segment (make-point 0 0) (make-point 2 2))
;; ((0 . 0) 2 . 2)

(print-point (midpoint-segment (make-segment (make-point 0 0)
					     (make-point 2 2))))
(1,1)

(print-point (midpoint-segment (make-segment (make-point 0 0)
					     (make-point 0 2))))
(0,1)


(print-point (midpoint-segment (make-segment (make-point 0 0)
					     (make-point 2 0))))
(1,0)

