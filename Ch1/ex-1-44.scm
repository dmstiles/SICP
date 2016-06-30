;; Exercise 1.44
;;
;; Description:
;;
;; Write a procedure `smooth` that takes as input a procedure that computes
;; `f` and returns procedure that computes the smoothed `f`. Also show how to 
;; generate a n-fold smoothed function of any given function using the `smooth`
;; and `repeated` procedures.



;; Definitions:

(define dx 0.00001)
;Value: dx


(define (n-fold-smooth f n)
 ((repeated smooth n) f))
;Value: n-fold-smooth


(define (smooth f)
  (lambda (x) (/ (+ (f (+ x dx))
		    (f x)
		    (f (- x dx)))
		 3)))
;Value: smooth


(define (repeated f n)
  (define (repeated-iter g count)
    (if (= count n)
	g
	(repeated-iter (compose f g) (inc count))))
  (repeated-iter f 1))
;Value: repeated


(define (compose f g)
  (lambda (x) (f (g x))))
;Value: compose


(define (inc x)
  (+ x 1))
;Value: inc



;; Testing:

((smooth cos) 0.7)
;Value: .7648421872589938

((n-fold-smooth cos 10) 0.7)
;Value: .7648421870295413

