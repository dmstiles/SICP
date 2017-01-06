;; Exercise 2.54
;;
;; Description:
;;
;; Two lists are equal if they contain equal elements arranged in the same order. Using
;; this definition define a procedure `equal?` to test the equality of two lists.


;; Definitions:

(define (equal? l1 l2)
  (let ((h1 (car l1))
	(h2 (car l2)))
    (cond ((and (null? l1) (null? l2)) true)
	  ((and (not (pair? h1)) (not (pair? h2))) (and (eq? h1 h2) (equal? t1 t2)))
	  (else (and (equal? h1 h2) (equal? t1 t2))))))
;Value: equal?



;; Testing:

(equal? '(this is a list) '(this is a list))
;Value: #t

(equal? '(this is a list) '(this (is a) list))
;Value: #f

(equal? '(a b c) '(a b c d))
;Value: #f

(equal? '() '(1 2 3 4 5))
;Value: #f

(equal? '(this (is (a (deeply) nested) list)) '(this (is (a (deeply) nested) list))
;Value: #t

(equal? '(this (is (a (deeply))) list) '(this (is (a (deeply)) nested) list))
;Value: #f


