;; Exercise 2.37
;;
;; Description:
;;
;; Complete the given procedures for computing matrix operations.


;; Definitions:

(define (dot-product v w)
  (accumulate + 0 (map * v w)))
;Value: dot-product

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))
;Value: matrix-*-vector

(define (transpose m)
  (accumulate-n cons nil m))
;Value: transpose

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))
;Value: matrix-*-matrix

(define nil (list ))
;Value: nil

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))
;Value: accumulate-n

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))
;Value: accumulate

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

(define m (list (list 1 2 3)
		(list 4 5 6)
		(list 7 8 9)))
;Value: m

(define n (list (list 1 2 3)
		(list 3 2 1)
		(list 2 1 3)))
;Value: n

(define v (list 1 2 3))
;Value: v

(define w (list 4 5 6))
;Value: w



;; Testing:

(dot-product v w)
;Value: 32

(matrix-*-vector m v)
;Value: (14 32 50)

(transpose m)
;Value: ((1 4 7) (2 5 8) (3 6 9))

(matrix-*-matrix m n)
;Value: ((13 9 14) (31 24 35) (49 39 56))

