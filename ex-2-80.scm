;; Exercise 2.80

;; Description:
;;
;; Define a procedure =zero? that tests if its argument is zero and install
;; it in the generic arithmetic package.



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
;... done



;; Definitions:

;; In 'generic-arithmetic' package
(define (=zero? x) (apply-generic '=zero? x))

;; In 'complex' package
(put '=zero? '(complex)
     (lambda (z) (= 0 (real-part z) (imag-part z))))

;; In 'rational' package
(put '=zero? '(rational)
     (lambda (r) (= 0 (numer r))))

;; In 'scheme-number' package
(put '=zero? '(scheme-number)
     (lambda (x) (= 0 x)))


;; Testing:

(define a-com (make-complex-from-real-imag 0 0))
;Value: a-com

(define b-com (make-complex-from-real-imag 3 4))
;Value: b-com

(=zero? a-com)
;Value: #t

(=zero? b-com)
;Value: #f

(define c-com (make-complex-from-mag-ang 0 0))
;Value: c-com

(define d-com (make-complex-from-mag-ang 0 3.14))
;Value: d-com

(define e-com (make-complex-from-mag-ang 1 3.14))
;Value: e-com

(=zero? c-com)
;Value: #t

(=zero? d-com)
;Value: #t

(=zero? e-com)
;Value: #f

(define a-rat (make-rational 0 1))
;Value: a-rat

(define b-rat (make-rational 1 2))
;Value: b-rat

(=zero? a-rat)
;Value: #t

(=zero? b-rat)
;Value: #f

(define a 0)
;Value: a

(define b 1)
;Value: b

(=zero? a)
;Value: #t

(=zero? b)
;Value: #f



