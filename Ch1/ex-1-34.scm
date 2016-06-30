;; Exercise 1.34
;;
;; Description:
;;
;; Consider procedure `f` defined below. What happens when the one asks
;; the interpreter to evaluate the combination (f f)? Explain.


;; Definitions:

(define (f g)
  (g 2))
;Value: f

(define (square x)
  (* x x))
;Value: square


;; Evaluations:

(f square)
;Value: 4

(f (lambda (z) (* z (+ z 1))))
;Value: 6

(f f)
;The object 2 is not applicable.


;; Solution: 
;;
;; This gives an error since the interpreter will end up trying to apply
;; the primitive expression 2, representing a number, as a procedure to 
;; the operand 2. 

