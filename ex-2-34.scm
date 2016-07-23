;; Exercise 2.34
;;
;; Description:
;;
;; Complete the given procedure to implement Horner's rule.


;; Definitions:

(define nil (list ))
;Value: nil

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))
;Value: accumulate

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* x higher-terms) this-coeff))
	       0
	       coefficient-sequence))
;Value: horner-eval


;; Testing:

;; 1 + 3x + 2x^2 + 3x^3 + 4x^4

(horner-eval 1 (list 1 3 2 3 4))
;Value: 13

(horner-eval 2 (list 1 3 2 3 4))
;Value: 103

;; 1 + 3x + 2x^2 + 4x^4

(horner-eval 1 (list 1 3 2 0 4))
;Value: 10

(horner-eval 2 (list 1 3 2 0 4))
;Value: 79

