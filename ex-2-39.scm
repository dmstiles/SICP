;; Exercise 2.39
;;
;; Description:
;;
;; Complete the given definitions of `reverse` in terms of `fold-left`
;; and `fold-right`.


;; Definitions:

(define (reverse-left sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))
;Value: reverse-left

(define (reverse-right sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))
;Value: reverse-right

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


;; Testing:

(reverse-left (list 1 2 3 4))
;Value: (4 3 2 1)

(reverse-right (list 1 2 3 4))
;Value: (4 3 2 1)

