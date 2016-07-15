;; Exercise 2.20
;;
;; Description:
;;
;; Using dotted-tail notation define a procedure `same-parity` that
;; takes one or more arguments and returns a list of all arguments
;; that have the same even-odd parity as the first argument.


;; Definitions:

(define (same-parity x . args)
  (let ((items (cons x args)))
    (if (even? x)
	(extract even? items)
	(extract odd? items))))
;Value: same-parity

(define (extract keep? args)
  (cond ((null? args) args)
	((keep? (car args)) (append (list (car args)) (extract keep? (cdr args))))
 	(else (extract keep? (cdr args)))))
;Value: extract

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
;Value: append

(define (even? x) (= (remainder x 2) 0))
;Value: even?

(define (odd? x) (= (remainder x 2) 1))
;Value: odd?



;; Testing:

(same-parity 1 2 3 4 5 6 7)
;; (1 3 5 7)

(same-parity 2 3 4 5 6 7)
;; (2 4 6)

