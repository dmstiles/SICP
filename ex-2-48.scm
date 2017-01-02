;; Exercise 2.48
;;
;; Description:
;;
;; Use the vector data abstraction from exercise 2.46 to define the a representation
;; for sequences with the constructor `make-segment` and the selectors `start-segment`
;; and `end-segment`


;; Imports:

(load "ex-2-46.scm")
;Loading "ex-2-46.scm"... done



;; Definitions

(define (make-segment u v)
  (cons u v))
;Value: make-segment


(define (start-segment s)
  (car s))
;Value: start-segment


(define (end-segment s)
  (cdr s))
;Value: end-segment



;; Testing

(define s1 (make-segment (make-vect 1 0)
			 (make-vect 0 1)))
;Value: s1

s1
;Value 17: ((1 . 0) 0 . 1)

(start-segment s1)
;Value 18: (1 . 0)

(end-segment s1)
;Value 19: (0 . 1)

