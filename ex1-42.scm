;; Exercise 1.42
;;
;; Definition:
;;
;; Define a procedure `compose` that implements the composition
;; f after g for two one argument procedures.



;; Definitions:

(define (compose f g)
  (lambda (x) (f (g x))))
;Value: compose


(define (sqaure x)
  (* x x))
;Value: sqaure


(define (inc x)
  (+ x 1))
;Value: inc



;; Testing:

((compose square inc) 6)
;Value: 49

