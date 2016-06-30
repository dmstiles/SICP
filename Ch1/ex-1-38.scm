;; Exercise 1.37
;;
;; Definition:
;;
;; Write a program that uses the `cont-frac` procedure to approximate
;; e using Euler's expansion.



;; Definitions:

;; Iterative definition.
(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0) 
	(/ (n i) (+ (d i) result))
	(iter (dec i)
	      (/ (n i) (+ (d i) result)))))
  (iter k 0.0))
;Value: cont-frac-iter


(define (dec x)
  (- x 1))
;Value: dec


(define (euler-e k)
  (+ 2.0
     (cont-frac-iter (lambda (i) 1.0)
		     (lambda (i) 
		       (if (not (= (remainder i 3) 1))
			   1
			   (* 2 (+ 1 (/ i 3)))))
		     k)))
;Value: euler-e


;; Testing:

(euler-e 10)
;Value: 2.654039983544254


