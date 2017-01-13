;; Exercise 2.60
;;
;; Description:
;;
;; Design the procedures `element-of-set?`, `adjoin-set`, `union-set`, and
;; `intersection-set` which operate on list-based set representation which allows
;; duplicates. How does the efficiency of each procedure compare with the non-duplicate
;; representation? Are there applications which a duplicate representation could be
;; preferred to a non-duplicate representation?



;; Definitions

(define (element-of-set? e set)
  (cond ((null? set) false)
	((equal? e (car set)) true)
	(else (element-of-set? e (cdr set)))))
;Value: element-of-set?


(define (adjoin-set e set)
  (cons e set))
;Value: adjoin-set


(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else (cons (car set1)
		    (union-set (cdr set1) set2)))))
;Value: union-set


(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1)
	       (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))
;Value: intersection-set



;; Testing:

(element-of-set? 'x '(x y z))
;Value: #t

(element-of-set? 'w '(x y z))
;Value: #f

(adjoin-set 'a '(x y z))
;Value 13: (a x y z)

(union-set '(a b c) '(x y z))
;Value 14: (a b c x y z)

(union-set '(x y z) '(x y z))
;Value 15: (x y z x y z)

(intersection-set '(x y) '(x y z))
;Value 16: (x y)

(intersection-set '(2 2 2) '(1 2 3))
;Value 17: (2 2 2)

(intersection-set '(a b c) '(x y z))
;Value: ()



;; Notes:
;;
;; `element-of-set?`
;;
;; Between the two implementations, the run-time complexity remains O(n). However since
;; the storage of duplicates is likely to increase average time to locate a particular
;; element, the duplicate representation will perform slightly worse on average. This
;; implementation while slightly worse on average will actually perform better in cases
;; where temporal locality optimizations may be applied, since recently adjoined
;; elements will be available near the front of the list.
;;
;; `adjoin-set`
;;
;; By eliminating the uniqueness requirement, `adjoin-set` is reduced from an O(n) to
;; an O(1) run-time compexity. The procedures merely appends new elements to the front
;; of the list without concern for it's current elements, eliminating the need to search
;; the list to ensure there is no preexisting match.
;;
;; `union-set`
;;
;; Given two sets A and B with n and m respective elements, the non-duplicate
;; representation, `union-set` is decreased from an O(n*m) run-time complexity to an
;; O(n) complexity. This is because the union is the set of elements in either or
;; both sets, therefore without regard for duplication, one can simply join the
;; two lists. This eliminates the need to check membership of elements between sets (an
;; O(n) operation) for each element in the second  set (m elements), and gives a
;; run-time bound only to the time to append one element of set A to the elements in
;; set B, this time is just O(n).
;;
;; `intersection-set`
;;
;; Due the requirement the the intersection of sets feature only elements in both sets
;; the implementation does not change between a representation that allows duplication
;; and one that does not. Therefore the representation allowing duplication will perform
;; worse on average since the `element-of-set?` procedure will take longer in set that
;; contains duplicates.

