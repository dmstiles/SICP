;; Exercise 2.26
;;
;; Description:
;;
;; Consider the definitions for lists `x` and `y` below. Show the results printed
;; by the interpreter in response to the the three procedures also defined below.


;; Definitions:

(define x (list 1 2 3))
;Value: x

(define y (list 4 5 6))
;Value: y

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
;Value: append



;; Solutions:

(append x y)
;; (1 2 3 4 5 6)

(cons x y)
;; ((1 2 3) 4 5 6)

(list x y)
;; ((1 2 3) (4 5 6))



;; Testing:

(define cons-list (cons x y))
;Value: cons-list

(car cons-list)
;Value 19: (1 2 3)

(cdr cons-list)
;Value 17: (4 5 6)

(cdr (cdr cons-list))
;Value 18: (5 6)

(car (cdr cons-list))
;Value: 4

(define list-list (list x y))
;Value: list-list

(cdr (cdr list-list))
;Value: ()

(car (cdr list-list))
;Value 17: (4 5 6)




