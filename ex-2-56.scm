;; Exercise 2.56
;;
;; Description:
;;
;; Extend the basic differentiator to handle the differentiation of exponents.


;; Definitions:

(define (variable? x) (symbol? x))
;Value: variable?


(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
;Value: same-variable?


(define (=number? sym num)
  (and (number? sym) (= sym num)))
;Value: =number?


(define (make-sum a b)
  (cond ((=number? a 0) b)
	((=number? b 0) a)
	((and (number? a) (number? b)) (+ a b))
	(else (list '+ a b))))
;Value: make-sum


(define (make-product a b)
  (cond ((or (=number? a 0) (=number? b 0)) 0)
	((=number? a 1) b)
	((=number? b 1) a)
	((and (number? a) (number? b) (* a b)))
	(else (list '* a b))))
;Value: make-product


(define (sum? exp)
  (and (pair? exp) (eq? (car exp) '+)))
;Value: sum?


(define (addend sum)
  (cadr sum))
;Value: addend


(define (augend sum)
  (caddr sum))
;Value: augend


(define (product? exp)
  (and (pair? exp) (eq? (car exp) '*)))
;Value: product?


(define (multiplier prod)
  (cadr prod))
;Value: multiplier


(define (multiplicand prod)
  (caddr prod))
;Value: multiplicand


(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '^)))
;Value: exponentiation?


(define (base exp)
  (cadr exp))
;Value: base

(define (exponent exp)
  (caddr exp))
;Value: exponent

(define (make-exponentiation a b)
  (cond ((=number? a 0) 0)
	((=number? a 1) 1)
	((=number? b 0) 1)
	((=number? b 1) a)
	((and (number? a) (number? b)) (^ a b))
	(else (list '^ a b))))
;Value: make-exponentiation


(define (^ base exponent)
  (define (*^ exponent acc)
    (if (= exponent 0)
        acc
        (*^ (- exponent 1) (* acc base))))
  (*^ exponent 1))
;Value: ^


(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum (make-product (multiplier exp)
			      (deriv (multiplicand exp) var))
		   (make-product (multiplicand exp)
			      (deriv (multiplier exp) var))))
	((exponentiation? exp)
	 (make-product (make-product (exponent exp)
				     (make-exponentiation (base exp) (- (exponent exp) 1)))
		       (deriv (base exp) var)))
	(else
	 (error "unknown expression type -- DERIV" exp))))
;Value: deriv



;; Testing:

(deriv '(+ x 3) 'x)
;Value: 1

(deriv '(* x y) 'x)
;Value: y

(deriv '(* (* x y) (+ x 3)) 'x)
;Value 13: (+ (* x y) (* (+ x 3) y))

(deriv '(^ x 4) 'x)
;Value 16: (* 4 (^ x 3))

