;; Exercise 2.33
;;
;; Definitions:
;;
;; Complete the definitions for `map`, `append`, and `length` to perform basic
;; list-manipulation operations as accumulations.


;; Definitions:

(define nil (list ))
;Value: nil

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))
;Value: accumulate

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
;Value: map

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
;Value: append

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))
;Value: length

(define square (lambda (x) (* x x)))
;Value: square

;; Testing:

(define list1 (list 1 2 3 4))
;Value: list1

(define list2 (list 5 6 7 8))
;Value: list2

(map square list1)
;; (1 4 9 16)

(append list1 list2)
;; (1 2 3 4 5 6 7 8)

(length list1)
;Value: 4

(length (append list1 list2))
;Value: 8

