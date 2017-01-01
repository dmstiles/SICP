;; Exercise 2.46
;;
;; Description:
;;
;; Implement a data abstraction for vectors by giving a constructor `make-vect` and
;; corresponding selectors `xcor-vect` and `ycor-vect`. Implement the procedures
;; `add-vect`, `sub-vect`, and `scale-vect` in terms of these constructors and
;; selectors.


;; Definitions

(define (make-vect x y)
  (cons x y))
;Value: make-vect


(define (xcor-vect v)
  (car v))
;Value: xcor-vect


(define (ycor-vect v)
  (cdr v))
;Value: ycor-vect


(define (add-vect u v)
  (comb-vect + u v))
;Value: add-vect


(define (sub-vect u v)
  (comb-vect - u v))
;Value: sub-vect


(define (scale-vect v s)
  (make-vect (* s (xcor-vect v))
	     (* s (xcor-vect v))))
;Value: scale-vect


(define (comb-vect op u v)
  (make-vect (op (xcor-vect u)
		(xcor-vect v))
	     (op (ycor-vect u)
		(ycor-vect v))))
;Value: comb-vect


;; Testing:

(define a (make-vect 1 1))
;Value: a

a
;Value 13: (1 . 1)

(define b (make-vect -1 -1))
;Value: b

b
;Value 15: (-1 . -1)

(add-vect a b)
;Value 16: (0 . 0)

(sub-vect a b)
;Value 19: (2 . 2)

(scale-vect a 0.5)
;Value 20: (.5 . .5)


