;; Exercise 2.52
;;
;; Description:
;;
;; Make changes to the `square-limit` procedure for the `wave` painter by working at
;; the following levels of the painting languages stratified design.
;;
;;    a. Add segments to the `wave` painter, creating a smile.
;;
;;    b. Change the pattern constructed by `corner-split` to use one copy of the
;;       `up-split` and `right-split` images instead of two.
;;
;;    c. Modify the `square-limit` procedure using the `square-of-four` procedure so
;;       as to assemble the corners in a different pattern.



;; Imports:
#lang racket
(require sicp-pict)

;; Definitions:

;; a.

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
   (make-vect 0.999 0.144))
  (make-segment ;; eye
   (make-vect 0.450 0.900)
   (make-vect 0.450 0.850))
  (make-segment ;; eye
   (make-vect 0.550 0.900)
   (make-vect 0.550 0.850))
  (make-segment ;; mouth
   (make-vect 0.450 0.750)
   (make-vect 0.500 0.700))
  (make-segment ;; mouth
   (make-vect 0.500 0.700)
   (make-vect 0.550 0.750))))


;; b.

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1))))
	(let ((corner (corner-split painter (- n 1))))
	  (beside (below painter up)
		  (below right corner))))))
;Value: corner-split


(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter (beside smaller smaller)))))


(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
	(beside painter (below smaller smaller)))))


;; c.

(define (square-limit painter n)
  (let ((quarter (rotate180 (corner-split painter n))))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))
;Value: square-limit



;; Testing:

(paint (segments->painter wave-segs))

(paint (corner-split einstein 3))

(paint (square-limit einstein 2)) 
