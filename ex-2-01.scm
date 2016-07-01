;; Exercise 2.1
;;
;; Description:
;;
;; Define a better version of `make-rat` that handles both positive and
;; negative arguments. `make-rat` should normalize the sign so that if
;; the rational number is positive both the numerator and denominator
;; are positive, and if the rational number is negative, only the
;; numerator is negative.



;; Definitions:

(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (pos? (/ n d))
	(cons (/ n g) (/ d g))
	(cons (* -1 (/ (abs n) g)) 
	      (/ (abs d) g)))))
;Value: make-rat


(define (print-rat x)
  (display (car x))
  (display "/")
  (display (cdr x))
  (newline))
;Value: print-rat


(define (pos? x) (> x 0))
;Value: pos?


(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
;Value: gcd



;; Testing:

(print-rat (make-rat 1 2))
1/2

(print-rat (make-rat 3 9))
1/3

(print-rat (make-rat -4 6))
-2/3

(print-rat (make-rat -10 -100))
1/10

