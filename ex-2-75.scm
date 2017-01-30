;; Exercise 2.75:
;;
;; Description:
;;
;; Implement the complex number constructor `make-from-mag-ang` in message passing style.



;; Definitions:

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
	  ((eq? op 'imag-part) (* r (sin a)))
	  ((eq? op 'magnitude) r)
	  ((eq? op 'angle) a)
	  (else
	   (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)
;Value: make-from-mag-ang

(define (apply-generic op arg)
  (arg op))
;Value: apply-generic

(define (real-part c)
  (apply-generic 'real-part c))
;Value: real-part

(define (imag-part c)
  (apply-generic 'imag-part c))
;Value: imag-part

(define (magnitude c)
  (apply-generic 'magnitude c))
;Value: magnitude

(define (angle c)
  (apply-generic 'angle c))
;Value: angle


(define pi 3.14159)
;Value: pi


;; Testing:

(define c1 (make-from-mag-ang 1 0))
;Value: c1

(real-part c1)
;Value: 1

(imag-part c1)
;Value: 0

(magnitude c1)
;Value: 1

(angle c1)
;Value: 0

(define c2 (make-from-mag-ang 5 (atan (/ 3 4))))
;Value: c2

(real-part c2)
;Value: 4.

(imag-part c2)
;Value: 3.

(magnitude c2)
;Value: 5

(angle c2)
;Value: .6435011087932844

;; Import `sqrt`
(load "Ch1/ex-1-07.scm")
;  Loading "Ch1/ex-1-07.scm"... done

(define c3 (make-from-mag-ang 1 (/ pi 4)))
;Value: c3

(/ (sqrt 2) 2)
;Value: .7071067811873449

(real-part c3)
;Value: .7071072502792263

(imag-part c3)
;Value: .7071063120935576

(sqrt (+ (square (real-part c3)) (square (imag-part c3))))
;Value: 1.

(magnitude c3)
;Value: 1

(angle c3)
;Value: .7853975

