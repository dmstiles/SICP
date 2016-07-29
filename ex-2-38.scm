;; Exercise 2.38
;;
;; Description:
;;
;; Consider the `fold-left` procedure defined below. State the property that
;; the `op` argument must satisfy in order to guarantee that `fold-right` and
;; `fold-left` produce the same values for any sequence.


;; Definitions:

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))
;Value: fold-left

;; Same as `accumulate` procedure.
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (fold-right op initial (cdr sequence)))))
;Value: fold-right

(define nil (list ))
;Value: nil

(fold-right / 1 (list 1 2 3))
;Value: 3/2

(fold-left / 1 (list 1 2 3))
;Value: 1/6

(fold-right list nil (list 1 2 3))
;Value: (1 (2 (3 ())))

(fold-left list nil (list 1 2 3))
;Value: (((() 1) 2) 3)


;; Solution:
;;
;; In order to guarantee that `fold-right` and `fold-left` both produce the same
;; value for a given list, the `op` given to each procedure must satisfy the 
;; commutative property. That is, the operation must be such that changing the
;; order of the operands does not change the result of their combination.