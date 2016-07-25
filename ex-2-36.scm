;; Exercise 2.36
;;
;; Description:
;;
;; Complete the given `accumulate-n` procedure which takes a list of lists, all
;; assumed to be the same length, and returns the accumulation of each respective
;; index in the sublists.


;; Definitions:

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))
;Value: accumulate-n

(define nil (list ))
;Value: nil

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))
;Value: accumulate


;; Testing:

(define s (list (list 1 2 3)
		(list 4 5 6)
		(list 7 8 9)
		(list 10 11 12)))
;Value: s

(accumulate-n + 0 s)
;Value: (22 26 30)

