;; Exercise 2.32
;;
;; Description:
;;
;; Complete the definition for the given `subset` procedure that generates the set
;; of subsets of a set and give a clear explanation of why it works.


;; Definitions:

(define (subsets s)
  (if (null? s)
      (list s)
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (x) (cons (car s) x))
			  rest)))))
;Value: subsets



;; Testing:
(subsets (list 1 2 3))
;; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))


;; Solution:
;;
;; The `subsets` procedure works by recursively reducing the problem to a subproblem
;; involving only the `cdr` portion of the list until ultimately reducing the list
;; to the null list (set). At this point the empty list will return to the previous
;; procedure call (stack frame) as the value of `rest` to execute to the else statement of
;; the `subsets` procedure. In the else statement each value within `rest` is mapped
;; as a pair to the `car` of the argument list which appends the running sublists of the
;; arguments `cdr` list, along with addition of the all the arguments `car` value to each
;; of pair in the running sublist. Therefore upon appending these two list, the procedure
;; yields both the subsets not containing the `car` value along with the subsets containing the
;; `car` value giving all possible subsets for the entire argument list.

