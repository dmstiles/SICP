;; Exercise 2.40
;;
;; Description:
;;
;; Define a procedure `unique-pairs` that, given an integer n, generates
;; the sequence of pairs (i, j) with 1 <= j < i <= n. Use this procedure
;; to simplify the definition of `prime-sum-pairs` given in the text.

;; Definitions:


(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
;Value: flatmap


(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
;Value: prime-sum?


(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
;Value: make-pair-sum


(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (unique-pairs n))))
;Value: prime-sum-pairs


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


(define (divides? a b)
  (= (remainder a b) 0))
;Value: divides?


(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? n test-divisor) test-divisor)
	(else (find-divisor n (+ 1 test-divisor)))))
;Value: find-divisor


(define (square n)
  (* n n))
;Value: square


(define (smallest-divisor n)
  (find-divisor n 2))
;Value: smallest-divisor


(define (prime? n)
  (= (smallest-divisor n) n))
;Value: prime?


(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
	  (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))
;Value: unique-pairs


;; Testing:

(unique-pairs 4)
;Value 16: ((2 1) (3 1) (3 2) (4 1) (4 2) (4 3))

; Using original definition from text

(prime-sum-pairs 3)
;Value 13: ((2 1 3) (3 2 5))

(prime-sum-pairs 4)
;Value 14: ((2 1 3) (3 2 5) (4 1 5) (4 3 7))

; Using refactored definition with `unique-pairs`

(prime-sum-pairs 4)
;Value 17: ((2 1 3) (3 2 5) (4 1 5) (4 3 7))





