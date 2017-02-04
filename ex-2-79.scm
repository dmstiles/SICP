;; Exercise 2.79

;; Description:
;;
;; Define a generic equality predicate `equ?` that tests the equality of two numbers.
;; Install the predicate, in the generic arithmetic package.


;; Imports:

(load "packages/generic-arithmetic.scm")

;Loading "packages/generic-arithmetic.scm"...
;  Loading "packages/operation-table.scm"... done
;  Loading "packages/complex.scm"...
;    Loading "packages/rectangular.scm"... done
;    Loading "packages/polar.scm"... done
;  ... done
;  Loading "packages/rational.scm"... done
;  Loading "packages/scheme-number.scm"... done



;; Definitions:

;; In 'generic-arithmetic' package
(define (equ? x y)
  (apply-generic 'equ? x y))
;Value: equ?

;; In 'complex' package
(define (equ? z1 z2)
  (and (= (real-part z1) (real-part z2))
       (= (imag-part z1) (imag-part z2))))

(put 'equ? '(complex complex)
     (lambda (z1 z2) (equ? z1 z2)))

;; In 'rational' package
(define (equ? x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))

(put 'equ? '(rational rational)
     (lambda (x y) (equ? x y)))
     

;; In 'scheme-number' package
(put 'equ? '(scheme-number scheme-number) =)



;; Testing:

(define a (make-scheme-number 1))
;Value: a

(define b (make-scheme-number 2))
;Value: b

(define c (make-scheme-number 1))
;Value: c

(equ? a b)
;Value: #f

(equ? a c)
;Value: #t

(define a-rat (make-rational 1 2))
;Value: a-rat

(define b-rat (make-rational 1 3))
;Value: b-rat

(define c-rat (make-rational 2 4))
;Value: c-rat

(equ? a-rat b-rat)
;Value: #f

(equ? a-rat c-rat)
;Value: #t


(define theta (atan (/ 4 3)))
;Value: theta

(define a-com (make-complex-from-real-imag (* 5 (cos theta))
					   (* 5 (sin theta))))
;Value: a-com

(define b-com (make-complex-from-real-imag 1 2))
;Value: b-com

(define c-com (make-complex-from-mag-ang 5 theta))
;Value: c-com

(equ? a-com b-com)
;Value: #f

(equ? a-com c-com)
;Value: #t


