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
;Value 13: (16 9 4 1)

(square-list-2 (list 1 2 3 4))
;Value 14: ((((() . 1) . 4) . 9) . 16)



;; Solution:




