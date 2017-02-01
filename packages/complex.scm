;; complex.scm
;;
;; Description:
;;
;; A package for working with complex numbers.


;; Definitions:

(define (install-complex-package)

  ;; imports
  (load "rectangular.scm")
  (load "polar.scm")

  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a )
    ((get 'make-from-mag-ang 'polar) r a ))

  ;; internal procedures
  (define (add z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
			 (+ (imag-part z1) (imag-part z2))))
  (define (sub z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
			 (- (imag-part z1) (imag-part z2))))
  (define (mul z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		       (+ (angle z1) (angle z2))))
  (define (div z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		       (- (angle z1) (angle z2))))

  ;; system interface
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'complex-package)
       		 
(install-complex-package)

