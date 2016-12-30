;; Exercise 2.43
;;
;; Description:
;;
;; Explain why interchanging the nested mappings in the `flatamp` procedure within
;; `queens` produces a dramatically slower runtime. Estimate the runtime for this
;; implementation in relation to the original solution which runs in time T. 


;; Definitions:

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (queen-cols (- k 1))))
	  (enumerate-interval 1 board-size)))))
  (queen-cols board-size))



;; Solution:
;;
;; By swapping the recursive call and the interval generation call, the procedure
;; is transformed from a linear-recursive to a tree-recursive process. This is because
;; the original implementation featured a single recursive call following the mapping
;; over the produced interval from `enumerate-interval`. Each recursive call reduced the
;; problem size by one and featured only a single recursive call to solve the given
;; problem size. In contrast, the new implementation features `board-size` recursive calls
;; for every recursive call to `queen-cols` (`board-size` calls in total). This
;; tree-recursive call structure totals in n^n recursive calls. Therefore an original 
;; linear-recursive solution taking time T, would mean that a tree-recursive solution
;; featuring n^n recursive calls, with each call taking time T, would result in a runtime
;; of T^n.



;; Notes:
;;
;; Based on the solution by Bill the Lizard.
;; http://www.billthelizard.com/2011/06/sicp-242-243-n-queens-problem.html