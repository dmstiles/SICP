;; Exercise 2.51
;;
;; Description:
;;
;; Define the below operation for painters in two different ways. First as a procedure
;; anaogous to the beside procedure given in the text. Second in terms of beside and a
;; suitable rotation operations.



;; Imports:

#lang racket
(require sicp-pict)



;; Definitions:


(define (custom-below-1 painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5))) 
    (let ((paint-top
	   ((transform-painter split-point
			       (make-vect 1.0 0.5)
			       (make-vect 0.0 1.0))
	    painter1))
	  (paint-bottom
	   ((transform-painter (make-vect 0.0 0.0)
			       (make-vect 1.0 0.0)
			       split-point)
	    painter2)))
    (lambda (frame)
      (paint-top frame)
      (paint-bottom frame)))))
;Value: custom-below-1y


(define (custom-below-2 painter1 painter2)
  (rotate180 (rotate270 (beside (rotate270 painter1)
				(rotate270 painter2)))))
;Value: custom-below-2


(define (rotate270 painter)
 ((transform-painter (make-vect 0.00 1.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0))
  painter))
;Value: rotate270



;; Testing:

(paint (custom-below-1 einstein einstein))

(paint (custom-below-2 einstein einstein))
