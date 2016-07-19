;; Exercise 2.31
;;
;; Description:
;;
;; Abstract the solution from exercise 2.30 to produce a procedure `tree-map`
;; with the property `square-tree` that could be defined as follows.
;;
;;     (define (square-tree tree) (tree-map square tree))


;; Definitions:

(define (square-tree tree)
  (tree-map square tree))
;Value: square-tree

(define (tree-map func tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (tree-map func sub-tree)
	     (func sub-tree)))
       tree))
;Value: tree-map

(define (square x) (* x x))
;Value: square


;; Testing:

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
;Value 14: (1 (4 (9 16) 25) (36 49))

