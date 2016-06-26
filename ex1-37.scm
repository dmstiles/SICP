;; Exercise 1.37
;;
;; Description:
;;
;; Define a procedure `cont-frac` such that evaluating (cont-frac n d k)
;; computes the value of the k-term finite continued fraction. Arguments
;; `n` and `d` are procedures with argument `i` that returns the value for
;; the numerator and denominator at that index.
;;
;; Check your procedure by approximating 1/phi using the given lambda
;; expressions. Note how large you must k in order to approximate a value
;; accurate to 4 decimal places.
;;
;; If the original implemenation for `cont-frac` generates a recursive
;; process write one generating an iterative process and vice versa.



;; Definitions:

;; Recursive definition.
(define (cont-frac n d k)
  (define (cont-frac-helper i)
    (if (= i k) 
	(/ (n i) (d i))
	(/ (n i) (+ (d i)
		     (cont-frac-helper (inc i))))))
  (cont-frac-helper 0))
;Value: cont-frac

;; Iterative definition.
(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0) 
	(/ (n i) (+ (d i) result))
	(iter (dec i)
	      (/ (n i) (+ (d i) result)))))
  (iter k 0))
;Value: cont-frac-iter


(define (dec x)
  (- x 1))
;Value: dec


(define (inc x)
  (+ x 1))
;Value: inc



;Value: phi-approx



;; Testing:

(cont-frac (lambda (i) 1.0)
	   (lambda (i) 1.0)
	   9)
;Value: .6179775280898876

(cont-frac-iter (lambda (i) 1.0)
		(lambda (i) 1.0)
		9)
;Value: .6179775280898876

(cont-frac (lambda (i) 1.0)
	   (lambda (i) 1.0)
	   10)
;Value: .6180555555555556

(cont-frac-iter (lambda (i) 1.0)
		(lambda (i) 1.0)
		10)
;Value: .6180555555555556



;; Solutions:
;;
;; 10 iterations required to get approximaiton accurate to 4 decimal places.

