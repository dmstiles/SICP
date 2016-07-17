;; Exercise 2.27
;;
;; Definitions:
;;
;; Modify the `reverse` procedure of exercise 2.18 to produce a `deep-reverse`
;; procedure that takes a list as argument and returns as its value the list
;; with its elements reversed and all sublists deep-reversed as well.



;; Definitions:

(define x (list (list 1 2) (list 3 4)))
;Value: x

(define (deep-reverse items)
  (cond ((null? items) items)
	((not (pair? items)) items)
	(else (append (deep-reverse (cdr items))
		      (list (deep-reverse (car items)))))))
;Value: deep-reverse

(define (reverse items)
  (if (null? items)
      items
      (append (reverse (cdr items)) (list (car items)))))
;Value: reverse

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
;Value: append



;; Testing:

x
;; ((1 2) (3 4))

(reverse x)
;; ((3 4) (1 2))

(deep-reverse x)
;; ((4 3) (2 1))

