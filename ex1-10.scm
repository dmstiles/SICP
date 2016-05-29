;; Exercise 1.10

;; The following procedure computes Ackermann's function.
(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))
;Value: a

;; What are the value of the following expressions?
(A 1 10)
;Value: 1024

(A 2 4)
;Value: 65536

(A 3 3)
;Value: 65536

;; Consider the following procedures where `A` is defined above.

(define (f n) (A 0 n))
;Value: f

(define (g n) (A 1 n))
;Value: g

(define (h n) (A 2 n))
;Value: h

;; Give concise mathematical definitions for the functions computed by
;; `f`, `g`, and `h` for positive integer values of n

;; f = 2 * n
(f 10)
;Value: 20

;; g = 2^n
(g 5)
;Value: 32

;; h = 2^2^2... {n - 1 powers of two}
(h 3)
;Value: 16
