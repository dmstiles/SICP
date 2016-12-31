;; Exercise 2.44
;;
;; Description:
;;
;; Define the procedure `up-split` used by `corner-split`.


;; Imports

;; In order to view graphical displays, the DrRacket editor was used
;; (http://racket-lang.org). In order to use Scheme feature from sicp
;; (`beside`, `below`, `paint` primitives) in the Racket language, the sicp
;;  package was imported into DrRackets package manager using the repo at
;; (https://github.com/sicp-lang/sicp.git). For this all too work, the follwing
;; two lines of code were required.

#lang racket
(require sicp-pict)


;; Definitions:

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1))))
	(let ((top-left (beside up up))
	      (bottom-right (below right right))
	      (corner (corner-split painter (- n 1))))
	  (beside (below painter top-left)
		  (below bottom-right corner))))))
;Value: corner-split


(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter (beside smaller smaller)))))
;Value: up-split


(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
	(beside painter (below (smaller smaller))))))
;Value: right-split


;; Testing

(paint (up-split einstein 5))