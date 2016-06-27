;; Exercise 1.40
;;
;; Description:
;;
;; Define a procedure `cubic` that can be used together with the 
;; `newtons-method` procedure to approximate zeros of the cubic
;;
;;     x^3 + ax^2 + bx + c = 0
;;


;; Definitions:

(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))
;Value: cubic


(define (cube x)
  (* x x x))
;Value: cube


(define (sqaure x)
  (* x x))
;Value: sqaure


(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
;Value: newtons-method


(define tolerance 0.00001)
;Value: tolerance


(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))
;Value: fixed-point


(define (newton-transform f)
  (lambda (x)
    (- x (/ (f x) ((deriv f) x)))))
;Value: newton-transform


(define (deriv f)
  (lambda (x)
    (/ (- (f (+ x dx)) (f x))
       dx)))
;Value: deriv


(define dx 0.00001)
;Value: dx



;; Testing:

(newtons-method (cubic 1 1 1) 1)
;Value: -.9999999999997796

(newtons-method (cubic 3 4 5) 1)
;Value: -2.2134116627621956

(newtons-method (cubic 3 0 0) -4)
;Value: -2.999999999987453

