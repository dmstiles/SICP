;; Exercise 2.83

;; Description:
;;
;; For each type in the tower of numeric types, design a `raise` procedures that
;; raises that type one level in the tower. Install a generic `raise` procedure
;; to work with all implementing types.


(load "packages/generic-arithmetic.scm")

;; in the 'scheme-number' package
(put 'raise '(scheme-number)
     (lambda (x) ((get 'make 'rational) x 1)))

;; in the 'rational' package
(put 'raise '(rational)
     (lambda (r) ((get 'make-from-real-imag 'complex)
		  (/ (numer r) (denom r)) 0)))

;; in the 'generic-arithmetic' package
(define (raise x) (apply-generic 'raise x))




;; Testing:

(raise 4)
;Value 13: (rational 4 . 1)

(raise (make-rational 1 2))
;Value 14: (complex rectangular 1/2 . 0)

