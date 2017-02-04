;; Generic Arithmetic Package

;; Definitions:

(load "packages/operation-table.scm")

(load "packages/complex.scm")

(load "packages/rational.scm")

(load "packages/scheme-number.scm")

(define (add x y) (apply-generic 'add x y))

(define (sub x y) (apply-generic 'sub x y))

(define (mul x y) (appl-generic 'mul x y))

(define (div x y) (appl-generic 'div x y))
