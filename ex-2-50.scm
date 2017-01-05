;; Exercise 2.50
;;
;; Description:
;;
;; Define the following transformation.
;;
;;    a. `flip-horiz` which flips painters horizontally
;;
;;    b. A transformation which rotates painters counterclockwise by 180 degrees
;;
;;    c. A transformation which rotates painters counterclockwise by 270 degrees



;; Imports:
#lang racket
(require sicp-pict)

;; Definitions:

(define (flip-horiz painter)
  ((transform-painter (make-vect 0.99 0.00)
		      (make-vect 0.00 0.00)
		      (make-vect 0.99 0.99))
   painter))
;Value: flip-horiz


(define (rotate180 painter)
  ((transform-painter (make-vect 0.99 0.99)
		      (make-vect 0.00 0.99)
		      (make-vect 0.99 0.00))
   painter))
;Value: rotate180


(define (rotate270 painter)
 ((transform-painter (make-vect 0.00 0.99)
		     (make-vect 0.00 0.00)
		     (make-vect 0.99 0.99))
  painter))
;Value: rotate270


;; Testing:

(paint (flip-horiz einstein))

(paint (rotate180 einstein))

(paint (rotate270 einstein))
