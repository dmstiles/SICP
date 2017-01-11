;; Exercise 2.57
;;
;; Description:
;;
;; Extend the differentiation program to handle sums and products of arbitrary terms.
;; Accomplish this by altering the representations for sums and products, without
;; altering the `deriv` procedure.



;; Definitions:


(define (variable? x) (symbol? x))
;Value: variable?


(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
;Value: same-variable?


(define (=number? sym num)
  (and (number? sym) (= sym num)))
;Value: =number?


(define (op? exp)
  (or (sum? exp)
      (product? exp)
      (exponentiation? exp)))
;Value: op?


(define (make-sum a b)
  (cond ((null? b) a)
	((=number? a 0) b)
	((=number? b 0) a)
	((and (number? a) (number? b)) (+ a b))
	((not (pair? b)) (list '+ a b))
	((op? b) (list '+ a b))
	(else (list '+ a (make-sum (car b) (cdr b))))))
;Value: make-sum


(define (make-product a b)
  (cond ((null? b) a)
	((or (=number? a 0) (=number? b 0)) 0)
	((=number? a 1) b)
	((=number? b 1) a)
	((and (number? a) (number? b)) (* a b))
	((not (pair? b)) (list '* a b))
	((op? b) (list '* a b))
	(else (list '* a (make-product (car b) (cdr b))))))
;Value: make-product


(define (sum? exp)
  (and (pair? exp) (eq? (car exp) '+)))
;Value: sum?


(define (addend sum)
  (cadr sum))
;Value: addend


(define (augend sum)
  (make-sum (caddr sum) (cdddr sum)))
;Value: augend


(define (product? exp)
  (and (pair? exp) (eq? (car exp) '*)))
;Value: product?


(define (multiplier prod)
  (cadr prod))
;Value: multiplier


(define (multiplicand prod)
  (make-product (caddr prod) (cdddr prod)))
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
  (display exp)
  (newline)
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

(deriv '(* x y (+ x 3)) 'x)
;Value 15: (+ (* x y) (* y (+ x 3)))

(deriv '(* x (+ x 3) y) 'x)
;Value 14: (+ (* x y) (* (+ x 3) y))

(deriv '(* (+ 1 2 3 4) x y) 'x)
;Value 17: (* (+ 1 2 3 4) y)
