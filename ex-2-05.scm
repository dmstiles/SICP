;; Exercise 2.5
;;
;; Description:
;;
;; Show that pairs of nonnegative integers can be represented using
;; only numbers and arithmetic operations if the pair (a,b) is represented
;; as the product of the following formula.
;;
;;     2^a * 3^b
;;
;; Give the corresponding definitions of `cons`, `car`, and `cdr`.



;; Definitions:

(define (cons a b)
  (* (exp 2 a) (exp 3 b)))
;Value: cons


(define (car n)
  (number-of-factors 2 n))
;Value: car


(define (cdr n)
  (number-of-factors 3 n))
;Value: cdr

   
(define (number-of-factors of-num in-num)
   (define (iter q count)
      (if (not (= 0 (remainder q of-num)))
	  count
	  (iter (/ q of-num) (inc count))))
    (iter (/ in-num of-num) 1))
;Value: number-of-factors


(define (exp x n)
  (cond ((= n 0) 1)
	((even? n) (square (exp x (/ n 2))))
	(else (* x (exp x (- n 1))))))
;Value: exp


(define (square x) (* x x))
;Value: square


(define (even? x)
  (= 0 (remainder x 2)))
;Value: even?

(define (inc x) (+ x 1))
;Value: inc



;; Testing:
(exp 10 3)
;Value: 1000

(exp 2 0)
;Value: 1

(exp 2 10)
;Value: 1024

(define pair1 (cons 2 4))
;Value: pair1

(car pair1)
;Value: 2

(cdr pair1)
;Value: 4

(define pair2 (cons 10 6))
;Value: pair2

(car pair2)
;Value: 10

(cdr pair2)
;Value: 6

