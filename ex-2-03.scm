;; Exercise 2.3
;;
;; Description:
;;
;; A.
;;
;; Implement a representation of rectangles in a plane. The implementation
;; should be in terms of constructors and selectors as well as procedures
;; that compute the perimeter and area of the rectangle.
;;
;; B.
;;
;; Use a different implementation then the initial one chose in step A. Design
;; The abstraction barrier so that the `perimeter` and `area` procedures work
;; with either implementation.



;; Definitions:

;; First implementation. 
(define (make-rect origin w h)
  (cons origin (cons (make-point w 0) 
			       (make-point 0 h))))
;Value: make-rect


;; Second implementation.
(define (make-rect a b c d)
  (cons a (cons (make-point (abs (- (x-point b) (x-point a))) 0)
		(make-point 0 (abs (- (y-point a) (y-point c)))))))
;Value: make-rect


(define (origin-rect r) (car r))
;Value: origin-rect


(define (width-rect r) (x-point (car (cdr r))))
;Value: width-rect


(define (height-rect r) (y-point (cdr (cdr r))))
;Value: height-rect


(define (perim r) 
  (+ (* 2 (width-rect r)) (* 2 (height-rect r))))
;Value: perim


(define (area r)
  (* (width-rect r) (height-rect r)))
;Value: area


(define (make-point x y) (cons x y))
;Value: make-point


(define (x-point p) (car p))
;Value: x-point


(define (y-point p) (cdr p))
;Value: y-point



;; Testing;

(define r1 (make-rect (make-point 0 0) 2 2))
;Value: r1

(perim r1)
;Value: 8

(area r1)
;Value: 4

(define r2 (make-rect (make-point 0 10)
		      (make-point 10 10)
		      (make-point 10 0)
		      (make-point 0 0)))
;Value: r2

(perim r2)
;Value: 40

(area r2)
;Value: 100


