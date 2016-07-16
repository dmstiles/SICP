;; Exercise 2.23
;;
;; Description:
;;
;; The `for-each` procedure takes as arguments a procedure of one argument
;; and a list. The procedure applies the argument procedure to each argument,
;; however does not return the resulting list, rather just applies the 
;; procedure to each element in the list. Give an implementation for such a
;; `for-each` procedure.



;; Definitions:

(define (for-each proc items)
  (cond ((null? items) true)
	(else (proc (car items))
	      (for-each proc (cdr items)))))
;Value: for-each


;; Testing: 

(for-each (lambda (x) (newline) (display x))
	  (list 57 321 88))

57
321
88
;Value: #t

