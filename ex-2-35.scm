;; Exercise 2.35
;;
;; Description:
;; 
;; Redefine the `count-leaves` procedure of section 2.2.2 as an an accumulator.



;; Definitions:

(define nil (list ))
;Value: nil

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))
;Value: accumulate

;; Recusively crawl the tree propagating subtree leave counts upward
;; and accumulating the result at each level.
(define (count-leaves tree)
  (accumulate +
	      0
	      (map (lambda (x) (if (not (pair? x))
				   1
				   (count-leaves x)))
		   tree)))
;Value: count-leaves

(define (fringe tree)
  (cond ((null? tree) tree)
	((not (pair? tree)) (list tree))
	(else (append (fringe (car tree))
		      (fringe (cdr tree))))))
;Value: fringe

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
;Value: append

;; Testing:

(define list1  (cons (list 1 2) (list 3 4)))
;Value: list1

(define list2 (cons (list 1 (list 2 3)) (list 4 5)))
;Value: list2

(define list3 (cons (list 1 (list 2 (list 3 4))) (list (list 5 6) (list 7 8))))
;Value: list3

(count-leaves list1)
;Value: 4

(count-leaves list2)
;Value: 5

(count-leaves list3)
;Value: 8

(count-leaves (list list1 list2))
;Value: 9

(count-leaves (list list1 list2 list3))
;Value: 17

;; Test cases from Bill The Lizard.

(count-leaves (list (list 1 2) (list 1 2 3) 1))
;Value: 6

(count-leaves nil)
;Value: 0



;; Notes:
;;
;; A `count-leaves` implementation using enumeration would lead to a more
;; elegant solution.