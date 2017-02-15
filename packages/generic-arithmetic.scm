;; Generic Arithmetic Package

;; Definitions:

(load "packages/operation-table.scm")

(load "packages/complex.scm")

(load "packages/rational.scm")

(load "packages/scheme-number.scm")

(define (add x y) (apply-generic 'add x y))

(define (sub x y) (apply-generic 'sub x y))

(define (mul x y) (apply-generic 'mul x y))

(define (div x y) (apply-generic 'div x y))

(define (equ? x y) (apply-generic 'equ? x y))

(define (=zero? x) (apply-generic '=zero? x))

(define (raise x) (apply-generic 'raise x))

(define type-tower (list 'scheme-number 'rational 'real 'complex))

(define (type-rank t)
  (define (iter t types n)
    (cond ((null? types) (error "Unkown type -- TYPE-RANK" t))
	  ((eq? t (car types)) n)
	  (else (iter t (cdr types) (+ n 1)))))
  (iter t type-tower 0))
