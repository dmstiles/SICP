;; Exercise 1.12
;; Write a procedure that computes elements of Pascal's 
;; triangle by means of recursive process.

(define (pascal r c)
  (cond ((< c 1) 0)
	((> c r) 0)
	((= c 1) 1)
	((+ (pascal (- r 1) (- c 1))
	    (pascal (- r 1) c)))))
;Value: pascal


;; Testing.
(pascal 1 1)
;Value: 1

(pascal 3 2)
;Value: 2

(pascal 5 3)
;Value: 6
