;; Exercise 2.22
;;
;; Description:
;;
;; Explain why the `square-list-1` procedure return its output backwards. 
;; Also explain why the `square-list-2` returns its procedure in reverse
;; nested form.


;; Definitions:

(define (square-list-1 items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons (square (car things))
		    answer))))
  (iter items '()))
;Value: square-list-1


(define (square-list-2 items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons answer 
		    (square (car things))))))
  (iter items '()))
;Value: square-list-2



;; Testing:

(square-list-1 (list 1 2 3 4))
;; (16 9 4 1)

(square-list-2 (list 1 2 3 4))
;; ((((() . 1) . 4) . 9) . 16)



;; Solution:
;;
;; The `sqaure-list-1` procedure returns its output in reverse order due to the 
;; fact that as each iteration passes the solution along, the next value calculated
;; for the ith iteration is added to the front of this list, with the previously
;; calculated i-1 to 0 values append to the end of the list. This causes values at
;; the front of the input list to propagate downwards with the output adding the 
;; last calculated value to the front causing the procedure to conclude with its output
;; in reverse order.
;;
;; The `sqaure-list-2` procedure returns its output in reverse nested form due to the fact
;; that each subsequent call to iter performs another nesting of the answer from the i-1 call.
;; Therefore the first `cons` creates a pair between `nil` and the square of item 0 passing this
;; pair along to the i+1 call. When the i+1 call executes it creates a pair between the argument
;; pair and the square of item 1, where the argument pair is the `car` of the new pair. Therefore
;; the `car` of this pair is nested in the new `pair` resulting in a reverse nesting that is
;; compounded for each subsequent call to iter. Simply, the first value is the deepest nesting in
;; the list, whereas a proper list form results in the last value having the deepest nesting.