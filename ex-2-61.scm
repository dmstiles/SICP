;; Exercise 2.61
;;
;; Description:
;;
;; Give an implementation for `adjoin-set` using the ordered-list representation of sets.
;; Take advantage of this prdering to produce a procedure that requires, on average,
;; half the number of steps as the unordered representation.



;; Definitions:

(define (adjoin-set e set)
  (cond ((null? set) (cons e set))
	((= (car set) e) set)
	((> (car set) e) (cons e set))
	(else (cons (car set)
		    (adjoin-set e (cdr set))))))
;Value: adjoin-set



;; Testing:

(adjoin-set 5 '(0 1 2 3 4 6 7 8 9))
;Value 13: (0 1 2 3 4 5 6 7 8 9)

(adjoin-set 5 '(0 1 2 3 4))
;Value 14: (0 1 2 3 4 5)

(adjoin-set 0 '(1 2 3 4 5))
;Value 15: (0 1 2 3 4 5)

(adjoin-set 3 '(1 2 3 4 5))
;Value 16: (1 2 3 4 5)

(adjoin-set 6 '())
;Value 17: (6)

