;; Exercise 2.62
;;
;; Description:
;;
;; Give an O(n) implementation of `union-set` for sets represented as ordered lists.



;; Definition:

(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else
	 (let ((x1 (car set1))
	       (x2 (car set2)))
	   (cond ((= x1 x2)
		  (cons x1 (union-set (cdr set1) (cdr set2))))
		 ((< x1 x2)
		  (cons x1 (union-set (cdr set1) set2)))
		 ((< x2 x1)
		  (cons x2 (union-set set1 (cdr set2)))))))))
;Value: union-set



;; Testing:

(union-set '(1 2 3) '(4 5 6))
;Value 13: (1 2 3 4 5 6)

(union-set '(1 2 3) '(2 8 9))
;Value 14: (1 2 3 8 9)

(union-set '(1 3 5 7 9) '(2 4 6 8))
;Value 15: (1 2 3 4 5 6 7 8 9)



;; Notes:
;;
;; Strange resemblance to mergesort.
