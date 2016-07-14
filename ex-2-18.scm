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
  (if (= (length items) 1)
      (car items)
      (cons (reverse (cdr items)) (car items))))
;Value: reverse

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
;Value: length



;; Testing:

(length (list 1 2 3))
;Value: 3

(reverse (list 1 4 9 16 25))
;; ((((25 . 16) . 9) . 4) . 1)

