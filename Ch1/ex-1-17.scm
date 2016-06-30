;; Exercise 1.17
;; Design a multiplication procedure that uses only the `double` and `halve`
;; procedures and takes a logarithmic number of steps.


;; Definitions

(define (even? n)
  (= (remainder n 2) 0))
;Value: even?


(define (double n)
  (+ n n))
;Value: double


(define (halve n)
  (/ n 2))
;Value: halve


(define (* a b)
  (cond ((= b 1) a)
	((even? b) (* (double a) (halve b)))
	(else (+ a (* a (- b 1))))))
;Value: *


;; Testing. 

(* 2 3)
;Value: 6

(* 2 4)
;Value: 8

(* 4 5)
;Value: 20

(* 10 10)
;Value: 100

(* 8 7)
;Value: 56

(* 13 13)
;Value: 169

