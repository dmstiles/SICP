;; Exercise 2.65
;;
;; Description:
;;
;; Give O(n) implementations for `union-set` and `intersection-set` for sets
;; implemented as balanced binary trees.


;; Definitions:

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


(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list (right-branch tree)
					  result-list)))))
  (copy-to-list tree '()))
;Value: tree->list


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


(define (element-of-set? x set)
  (cond ((null? set) false)
	((= x (entry set)) true)
	((< x (entry set))
	 (element-of-set? x (left-branch set)))
	((> x (entry set))
	 (element-of-set? x (right-branch set)))))
;Value: element-of-set?


(define (union-set tree-set1 tree-set2)
  (define (union-set-inner set1 set2)
    (cond ((null? set1) set2)
	  ((null? set2) set1)
	  (else
	   (let ((x1 (car set1))
		 (x2 (car set2)))
	     (cond ((= x1 x2)
		    (cons x1 (union-set-inner (cdr set1) (cdr set2))))
		   ((< x1 x2)
		    (cons x1 (union-set-inner (cdr set1) set2)))
		   ((< x2 x1)
		    (cons x2 (union-set-inner set1 (cdr set2)))))))))
  (list->tree (union-set-inner (tree->list tree-set1)
			       (tree->list tree-set2))))
;Value: union-set


(define (intersection-set tree-set1 tree-set2)
  (define (intersection-set-inner set1 set2)
    (if (or (null? set1) (null? set2))
	'()
	(let ((x1 (car set1))
	      (x2 (car set2)))
	  (cond ((= x1 x2)
		 (cons x1 (intersection-set-inner (cdr set1) (cdr set2))))
		((< x1 x2)
		 (intersection-set-inner (cdr set1) set2))
		((> x1 x2)
		 (intersection-set-inner set1 (cdr set2)))))))
  (list->tree (intersection-set-inner (tree->list tree-set1)
				      (tree->list tree-set2))))
;Value: intersection-set



;; Testing:

(define odds (list->tree '(1 3 5 7 9)))
;Value: odds

(define evens (list->tree '(2 4 6 8 10)))
;Value: evens

(define rand1 (list->tree '(13 23 37 45 52)))
;Value: rand1

(define rand2 (list->tree '(1 3 13 22 45 87 99)))
;Value: rand2

(union-set odds evens)
;Value 20: (5 (2 (1 () ()) (3 () (4 () ()))) (8 (6 () (7 () ())) (9 () (10 () ()))))

(tree->list (union-set odds evens))
;Value 21: (1 2 3 4 5 6 7 8 9 10)

(intersection-set odds evens)
;Value: ()

(intersection-set rand1 rand2)
;Value 22: (13 () (45 () ()))


;; Notes:
;;
;; By converting the binary tree representation of sets in order to perfrom the union
;; and intersection operations, the processes achieve O(n) unions and intersections
;; previously seen with the ordered list representation. However in converting the sets
;; back to a binary tree representation, the O(n) time complexity remains unchanged,
;; but gives the benefit of O(log(n)) insertion and contain operations. Therefore
;; having the O(n) `tree->list` and `list->tree` procedures makes the binary tree
;; representation optimal in the concrete implementation of sets.
