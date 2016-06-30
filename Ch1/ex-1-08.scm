;; Exercise 1.8

;; Use the rational expression, defined in the text, to implement a cube root procedure
;; analagous to the square root procedure defined in previous exercises.


(define (square x) (* x x))
;Value: square

(define (cube x) (* (square x) x))
;Value: cube

(define (good-enough? guess x)
  (< (abs (- (improve guess x) guess)) 0.001))
;Value: good-enough?

(define (average x y)
  (/ (+ x y) 2))
;Value: average

(define (improve y x)
  (/ (+ (/ x
	   (square y))
	(* 2 y))
     3))
;Value: improve

(define (cube-root x)
  (cube-root-iter 1.0 x))
;Value: cube-root

(define (cube-root-iter guess x)
  (if (good-enough? guess x)
	  guess
	  (cube-root-iter (improve guess x) x)))
;Value: cube-root-iter


;; Testing normal values.
(cube-root 8)
;Value: 2.000004911675504

(cube-root 64)
;Value: 4.000017449510739


;; Testing small values.
(cube-root 8e-9)
;Value: 3.566332162406482e-3
;; Current implementation not accurate for small values.

;; Testing large values.
(cube-root 37035925937037)
;Value: 33333.00052843487
