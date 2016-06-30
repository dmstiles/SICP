;; Exercise 1.45
;;
;; Description:
;;
;; Do some experiments to determine how many average damps are 
;; required to compute nth roots as a fixed point search based upon
;; the repeated damping of y => x / (y^(n-1)) for a given nth power.
;; Use these results to implement a simple procedure for computing
;; nth roots using `fixed-point`, `average-damp`, and the `repeated`
;; procedures.



;; Definitions:

(define (root x n)
  (fixed-point ((repeated average-damp 8) (lambda (y) (/ x (fast-expt y (- n 1)))))
	       1.0))
;Value: root


(define (average-damp f)
  (lambda (x) (average x (f x))))
;Value: average-damp


(define tolerance 0.00001)
;Value: tolerance


(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))
;Value: fixed-point


(define (repeated f n)
  (define (repeated-iter g count)
    (if (= count n)
	g
	(repeated-iter (compose f g) (inc count))))
  (repeated-iter f 1))
;Value: repeated


(define (compose f g)
  (lambda (x) (f (g x))))
;Value: compose


(define (inc x)
  (+ x 1))
;Value: inc


(define (average a b)
  (/ (+ a b) 2))
;Value: average


(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt  b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))
;Value: fast-expt



;; Testing:

(fast-expt 2 10)
;Value: 1024

(fast-expt 2 9)
;Value: 512

;; With 2 average damps applied.
(root (fast-expt 2 4) 4)
;Value: 2.0000000000021965

;; With 4 average damps applied.
(root (fast-expt 2 8) 8)
;Value: 2.000008820504543

;; With 4 average damps applied.
(root (fast-expt 2 16) 16)
;Value: 2.000000000076957 

;; With 6 average damps applied.
(root (fast-expt 2 32) 32)
;Value: 2.0000052794490126

;; With 6 average damps applied.
(root (fast-expt 2 64) 64)
;Value: 2.0000000000000853

;; With 8 average damps applied.
(root (fast-expt 2 128) 128)
;Value: 2.000007068673832

;; With 8 average damps applied.
(root (fast-expt 2 256) 256)
;Value: 2.0000000003645573

;; With 8 average damps applied.
(root (fast-expt 2 512) 512)
;Value: 1.9999950031986762

;; With 8 average damps applied.
(root (fast-expt 2 1024) 1024)
;; Results in overflow.


;; Solution:

;; 8 repeated applications of `average-damp` to the function x / (y^(n-1))
;; is sufficient to guarantee the convergence of `fixed-point` for n <= 512