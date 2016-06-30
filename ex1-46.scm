;; Exercise 1.46
;;
;; Description:
;;
;; Write a procedure `iterative-improve` that takes two procedures as arguments:
;; a method for telling whether a guess is good enough and a method for improving
;; a guess. `iterative-improve` should return a procedure that takes a guess as
;; an argument and keeps improving the guess until it is good enough.



;; Definitions:

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
	  guess
	  (iter (improve guess))))
  (lambda (guess) (iter guess)))
;Value: iterative-improve


(define (sqrt x)
  ((iterative-improve (lambda (guess) (close-enough? (square guess) x))
		     (lambda (guess) (average guess (/ x guess))))
   1.0))
;Value: sqrt


(define (fixed-point f first-guess)
  ((iterative-improve (lambda (guess) (close-enough? guess (f guess)))
		      (lambda (guess) (f guess)))
   1.0))
;Value: fixed-point

						     
(define (close-enough? v1 v2)
  (< (abs (- v1 v2)) 0.00001))
;Value: close-enough?


(define (average x y)
  (/ (+ x y) 2))
;Value: average



;; Testing:

(sqrt 9)
;Value: 3.000000001396984

(fixed-point cos 1.0)
;Value: .7390893414033928

