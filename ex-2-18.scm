;; Exercise 2.18
;;
;; Description:
;;
;; Define a procedure `reverse` that takes a list as an argument
;; and returns a list of the same elements in reverse order.
;;
;; (reverse (list 1 4 9 16 25))
;; (25 16 9 4 1)


;; Definitions:

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

(reverse (list 1 4 9 16 25))
;;Value: (25 16 9 4 1)

