;; Exercise 2.47
;;
;; Description:
;;
;; Consider the two potential `make-frame` constructors given in the text. For each,
;; define the corresponding selectors to produce an implementation for frames.


;; Definitions:

;; Implemntation A

(define (make-frame-a origin edge1 edge2)
  (list origin edge1 edge2))
;Value: make-frame-a


(define (frame-origin-a frame)
  (car frame))
;Value: frame-origin-a


(define (frame-edge1-a frame)
  (cadr frame))
;Value: frame-edge1-a


(define (frame-edge2-a frame)
  (cadr (cdr frame)))
;Value: frame-edge2-a


;; Implementation B
;;
;; Note the only difference is the selector for edge 2.

(define (make-frame-b origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
;Value: make-frame-b

(define (frame-origin-b frame)
  (car frame))
;Value: frame-origin-b


(define (frame-edge1-b frame)
  (cadr frame))
;Value: frame-edge1-b


(define (frame-edge2-b frame)
  (cdr (cdr frame)))
;Value: frame-edge2-b


;; Vector data abstraction

(define (make-vect x y)
  (cons x y))
;Value: make-vect


(define (xcor-vect v)
  (car v))
;Value: xcor-vect


(define (ycor-vect v)
  (cdr v))
;Value: ycor-vect



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




;; Testing:

(define f1 (make-frame-a (make-vect 0 0)
			 (make-vect 1 0)
			 (make-vect 0 1)))
;Value: f1

(frame-origin-a f1)
;Value 13: (0 . 0)

(frame-edge1-a f1)
;Value 14: (1 . 0)

(frame-edge2-a f1)
;Value 16: (0 . 1)

(define f2 (make-frame-b (make-vect 0 0)
			 (make-vect 1 0)
			 (make-vect 0 1)))
;Value: f2

(frame-origin-b f2)
;Value 17: (0 . 0)

(frame-edge1-b f2)
;Value 18: (1 . 0)

(frame-edge2-b f2)
;Value 19: (0 . 1)



