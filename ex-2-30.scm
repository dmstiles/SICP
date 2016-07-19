;; Exercise 2.30
;;
;; Description:
;;
;; Define a procedure `square-tree` analagous to the `square-list` procedure of
;; exercise 2.21. Define `sqaure-tree` both directly and also using map with
;; recursion.


;; Definitions:

(define (square-tree tree)
  (cond ((null? tree) tree)
	((not (pair? tree)) (* tree tree))
	(else (cons (square-tree (car tree))
		    (square-tree (cdr tree))))))
;Value: square-tree

(define (square-tree-map tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree-map sub-tree)
	     (* sub-tree sub-tree)))
       tree))
;Value: square-tree-map



;; Testing:

(square-tree 
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
;; (1 (4 (9 16) 25) (36 49))

(square-tree-map
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
;; (1 (4 (9 16) 25) (36 49))


