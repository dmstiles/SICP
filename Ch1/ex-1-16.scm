;; Exercise 1.16
;; Design a procedure that evolves an iterative exponentiaiton process using
;; successive squaring and a logarithmic number of steps.


;; Definitions.

(define (even? n)
  (= (remainder n 2) 0))
;Value: even?


(define (expt b n)
  (expt-iter b n 1))
;Value: expt


(define (expt-iter b n a)
  (cond ((= n 0) a)
	((even? n) (expt-iter (* b b) (/ n 2) a))
	(else (expt-iter b (- n 1) (* a b)))))
;Value: expt-iter


;; Testing. 

(expt 2 4)
;Value: 16

(expt 2 10)
;Value: 1024

(expt 3 3)
;Value: 27

