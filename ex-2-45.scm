;; Exercise 2.45
;;
;; Description:
;;
;; `right-split` and `up-split` (ex-2-44) can be expressed as instances of a 
;; general `split` procedure. Define this procedure so that the given definitions
;; of `right-split` and `up-split` produce the same behavior as their original
;; definitions.


;; Imports:

#lang racket
(require sicp-pict)


;; Definitions:

;; `sup` denotes the superpositioned painting operation
;; `sub` deontes the subpositioned painting operation
;;
;; Results in the smaller painters being painted in the
;; subpositions, while the larger painters are painted in
;; the superpositions (relative to the smaller painters
;; which result from the recursive subproblem result).
(define (split sup sub)
  (lambda (painter n)
    (define (split-helper painter n)
      (if (= n 0)
	  painter
	  (let ((smaller (split-helper painter (- n 1))))
	    (sup painter (sub smaller smaller)))))
    (split-helper painter n)))



(define right-split (split beside below))


(define up-split (split below beside))


;; Testing:

(paint (right-split einstein 3))

(paint (up-split einstein 3))