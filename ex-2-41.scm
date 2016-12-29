;; Exercise 2.41
;;
;; Description:
;;
;; Write a procedure to find all ordered triples of distinct postive integers
;; i, j, k less than or equal to n that sum to given integer s.


;; Definitions:


(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
;Value: flatmap


(define (sums-to-num? seq num)
  (= num (accumulate + 0 seq)))
;Value: sums-to-num?


(define (make-triple-sum triple)
  (let ((first (car triple))
	(second (cdr triple))
	(third (cadr (cdr triple))))
    (list first second third (+ first second third))))
;Value: make-triple-sum


(define (triple-sums n sum)
  (filter (lambda (triple) (sums-to-num? triple sum))
	  (unique-triples n)))
;Value: triple-sums


(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
;Value: enumerate-interval


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))
;Value: accumulate


(define (filter pred seq)
  (cond ((null? seq) nil)
	((pred (car seq))
	 (cons (car seq) (filter pred (cdr seq))))
	(else (filter pred (cdr seq)))))
;Value: filter


(define nil (list ))
;Value: nil


(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
	  (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))
;Value: unique-pairs


(define (unique-triples n)
  (flatmap
   (lambda (i)
     (map (lambda (pair) (cons i pair))
	  (unique-pairs (- i 1))))
   (enumerate-interval 1 n)))
;Value: unique-triples


;; Testing:

(unique-triples 3)
;Value 16: ((3 2 1))

(unique-triples 4)
;Value 17: ((3 2 1) (4 2 1) (4 3 1) (4 3 2))

(triple-sums 4 8)
;Value 20: ((4 3 1))

(unique-triples 5)
;Value 21: ((3 2 1) (4 2 1) (4 3 1) (4 3 2) (5 2 1) (5 3 1) (5 3 2) (5 4 1) (5 4 2) (5 4 3))

(triple-sums 5 8)
;Value 22: ((4 3 1) (5 2 1))

