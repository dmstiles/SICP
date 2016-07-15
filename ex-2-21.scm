;; Exercise 2.21
;;
;; Description:
;;
;; Implement the two different `square-list` procedures defined in the text.



;; Definitions:

(define (square x) (* x x))
;Value: square

(define (square-list items)
  (if (null? items)
      items
      (cons (square (car items)) (square-list (cdr items)))))
;Value: square-list

(define (square-list-map items)
  (map square items))
;Value: square-list-map


;; Testing:

(square-list (list 1 2 3 4))
;; (1 4 9 16)

(square-list-map (list 1 2 3 4))
;; (1 4 9 16)
