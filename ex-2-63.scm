;; Exercise 2.63
;;
;; Description:
;;
;; Compare the `tree->list-1` and `tree->list-2` procedures given in the text.
;;
;; a. Do the two procedures produce the same result for every tree?
;;
;; b. Do the two procedures have the same order of growth in number of steps required
;;    to convert a balanced tree with n elements to a list? Which grows more slowly?


;; Definitions:

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
	      (cons (entry tree)
		    (tree->list-1 (right-branch tree))))))
;Value: tree->list-1


(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list (right-branch tree)
					  result-list)))))
  (copy-to-list tree '()))
;Value: tree->list-2


(define (make-tree entry left right)
  (list entry left right))
;Value: make-tree


(define (entry tree) (car tree))
;Value: entry


(define (left-branch tree) (cadr tree))
;Value: left-branch


(define (right-branch tree) (caddr tree))
;Value: right-branch

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
;Value: append



;; Solution:

(define tree1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
;Value: tree1

(define tree2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
;Value: tree2

(define tree3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 ()()))))
;Value: tree3


tree1
;Value 14: (7 (3 (1 () ()) (5 () ())) (9 () (11 () ())))

tree2
;Value 17: (3 (1 () ()) (7 (5 () ()) (9 () (11 () ()))))

tree3
;Value 19: (5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ())))


(tree->list-1 tree1)
;Value 15: (1 3 5 7 9 11)

(tree->list-2 tree1)
;Value 21: (1 3 5 7 9 11)

(tree->list-1 tree2)
;Value 18: (1 3 5 7 9 11)

(tree->list-2 tree2)
;Value 22: (1 3 5 7 9 11)

(tree->list-1 tree3)
;Value 20: (1 3 5 7 9 11)

(tree->list-2 tree3)
;Value 23: (1 3 5 7 9 11)


;; a. By workin down the the tree in order, appending the resulting list from the right sub-tree
;;    to the resulting list from the left sub-tree both procedures do return the same list. That
;;    is, regardless of the tree's structure, the list is built from the largest element (furthest)
;;    right subtree) to the smallest element (furthest left). Since the defining property of the
;;    tree is ordering by subtree's the resulting lists are always identical.
;;
;; b. While the first procedure generates a process requiring O(n) steps. The second procedure
;;    generates a recursive process requiring O(n^2) worst case steps. Since each recursive call
;;    will need to traverse the subproblem size in order to perform the append. For a balanced
;;    tree, each subproblem halving doubles the number of subproblem, therefore each level of the
;;    recursive process will, in aggregate, perform n comptational steps (for log(n) total sublevels).
;;    For an unbalanced tree, the number of subproblems at each level can vary, and the time required
;;    to solve the problem will be proportional to the size of the subproblem. Therefore in the worst
;;    case (completely unbalanced tree) each recursive step will produce a subproblem of size n - 1.
;;    This will take n steps to reach the tree's depth with each step subsequent requiring n - 1
;;    computational steps for the `append`. This will results in O(n^2) growth
;;    [(n) + (n - 1) + ... + 1] which is known to be equivalent to (n*(n + 1))/2 or O(n^2).
