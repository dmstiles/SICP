;; Exercise 2.64
;;
;; Description:
;;
;; Consider the procedure `list->tree` given in the text.
;;
;;    a. Explain how the `partial-tree` procedure works.
;;
;;    b. What is the order of growth in number of steps required by `list->tree`
;;       to convert a list of n elements?



;; Definitions:

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
;Value: list->tree


(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
	(let ((left-result (partial-tree elts left-size)))
	  (let ((left-tree (car left-result))
		(non-left-elts (cdr left-result))
		(right-size (- n (+ left-size 1))))
	    (let ((this-entry (car non-left-elts))
		  (right-result (partial-tree (cdr non-left-elts)
					      right-size)))
	      (let ((right-tree (car right-result))
		    (remaining-elts (cdr right-result)))
		(cons (make-tree this-entry left-tree right-tree)
		      remaining-elts))))))))
;Value: partial-tree


(define (make-tree entry left right)
  (list entry left right))
;Value: make-tree


(define (entry tree) (car tree))
;Value: entry


(define (left-branch tree) (cadr tree))
;Value: left-branch


(define (right-branch tree) (caddr tree))
;Value: right-branch



;; Testing:

(list->tree '(1 3 5 7 9 11))
;Value 15: (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))



;; Solution:
;;
;; a. The procedure works by halving this list into left and right portions excluding
;;    the middle element which is to become the parent to these halves. The first recursive
;;    call to `partial-tree` will return the left half of the tree as the first element
;;    of its returned list, the remaining elements will be the flat ordered list items
;;    not in the the left-tree. The `car` of this list is the parent and the `cdr` represents
;;    elements to be passed to the second recursive call to `partial-tree` which will construct
;;    the right sub-tree. The tree is formed at the procedure's end with a call to `make-tree`
;;    using the parent and left and right sub-trees. The procedure achieves balances by halving
;;    the list in composing the left and right subtrees therefore each sub-tree will be composed
;;    with an equal number of elements of the opposite sub-tree. The base case for the recursion
;;    is when the list size is 0, at which point an empty list is `cons`d on to remaining list
;;    items which do not belong to the current subtree.
;;
;; b. As mentioned in section (a) each call to `partial-tree` results in a halving of the
;;    problem size, however `partial-tree` must be applied to each halve of the list. Therefore
;;    although the sub-problem is halving, the sub-problem proliferation is doubling. These
;;    competing tensions cancel each other out, and give the problem a run-time bound of O(n).
;;    Logically this is the result of having to visit each element in the list to construct the
;;    sub-tree of which that element is the parent.
