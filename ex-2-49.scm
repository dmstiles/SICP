;; Exercise 2.49
;;
;; Description:
;;
;; Use the `segments->painter` procedure to define the following primitive painters
;;
;;    a. The painter that draws the outline of the designated frame
;;
;;    b. The painter that draws an "X" by connecting opposite corners of the frame
;;
;;    c. The painter that draws a diamond shape by connecting the midpoints of the
;;       sides of the frame
;;
;;    d. The `wave` painter



;; Imports:

#lang racket
(require sicp-pict)

;; Definitions:

(define outline-segs (list (make-segment (make-vect 0 0)
					 (make-vect 0 0.99))
			   (make-segment (make-vect 0 0.99)
					 (make-vect 0.99 0.99))
			   (make-segment (make-vect 0.99 0.99)
					 (make-vect 0.99 0))
			   (make-segment (make-vect 0.99 0)
					 (make-vect 0 0))))

(define x-segs (list (make-segment (make-vect 0 0)
				   (make-vect 0.99 0.99))
		     (make-segment (make-vect 0 0.99)
				   (make-vect 0.99 0))))

(define diamond-segs (list (make-segment (make-vect 0.5 0)
					 (make-vect 0.99 0.5))
			   (make-segment (make-vect 0.99 0.5)
					 (make-vect 0.5 0.99))
			   (make-segment (make-vect 0.5 0.99)
					 (make-vect 0 0.5))
			   (make-segment (make-vect 0 0.5)
					 (make-vect 0.5 0))))

;; Definition by Bill The Lizard.
;; (http://www.billthelizard.com/2011/10/sicp-249-defining-primitive-painters.html)
(define wave-segs
 (list
  (make-segment
   (make-vect 0.006 0.840)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.006 0.635)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.403 0.646))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.354 0.492))
  (make-segment
   (make-vect 0.403 0.646)
   (make-vect 0.348 0.845))
  (make-segment
   (make-vect 0.354 0.492)
   (make-vect 0.249 0.000))
  (make-segment
   (make-vect 0.403 0.000)
   (make-vect 0.502 0.293))
  (make-segment
   (make-vect 0.502 0.293)
   (make-vect 0.602 0.000))
  (make-segment
   (make-vect 0.348 0.845)
   (make-vect 0.403 0.999))
  (make-segment
   (make-vect 0.602 0.999)
   (make-vect 0.652 0.845))
  (make-segment
   (make-vect 0.652 0.845)
   (make-vect 0.602 0.646))
  (make-segment
   (make-vect 0.602 0.646)
   (make-vect 0.751 0.646))
  (make-segment
   (make-vect 0.751 0.646)
   (make-vect 0.999 0.343))
  (make-segment
   (make-vect 0.751 0.000)
   (make-vect 0.597 0.442))
  (make-segment
   (make-vect 0.597 0.442)
   (make-vect 0.999 0.144))))

(paint (segments->painter outline-segs))
(paint (segments->painter x-segs))
(paint (segments->painter diamond-segs))
(paint (segments->painter wave-segs))

