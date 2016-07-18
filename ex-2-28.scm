;; Exercise 2.28
;;
;; Description:
;;
;; Write a procedure `fringe` that takes as an argument a tree and returns
;; a list whose elements are all letters of the tree arranged in
;; left-to-right order.


;; Definitions

(define x (list (list 1 2) (list 3 4)))
;Value: x

(define (fringe tree)
  (cond ((null? tree) tree)
	((not (pair? tree)) (list tree))
	(else (append (fringe (car tree))
		      (fringe (cdr tree))))))
;Value: fringe

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
;Value: append


;; Testing:

(fringe x)
;; (1 2 3 4)

(fringe (list x x))
;; (1 2 3 4 1 2 3 4)



