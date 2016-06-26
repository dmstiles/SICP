;; Exercise 1.38
;;
;; Description:
;;
;; Define a procedure (tan-cf x k) which computes the tangent function
;; based on Lambert's formula.



;; Definitions:

(define (tan-cf x k)
  (cont-frac-iter (lambda (i) (if (= i 0) x (square x)))
		  (lambda (i) (+ 1 (* i 2)))
		  k))
;Value: tan-cf


;; Iterative definition.
(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0) 
	(/ (n i) (- (d i) result))
	(iter (dec i)
	      (/ (n i) (- (d i) result)))))
  (iter k 0.0))
;Value: cont-frac-iter


(define (dec x)
  (- x 1))
;Value: dec


(define (square x)
  (* x x))
;Value: square


;; Testing;
(tan-cf 1 10)
;Value: 1.557407724654902

(tan-cf 2 10)
;Value: -2.1850398632615287

;; Near pi/2
(tan-cf 1.57 10)
;Value: 1255.7655915008268

