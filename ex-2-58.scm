;; Exercise 2.58
;;
;; Description:
;;
;; Modify the differentiation program to work under the following conditions:
;;
;;    a. Algebraic expression are presented in fully paranthesized infix notation:
;;
;;        (x + (3 * (x + (y + 2))))
;;
;;    b. Algebraic expressions are presented in standard algebraic notation:
;;
;;        (x + 3 + * (x + y + 2))



;; Definitions:
n
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
	(else (list a '+ b))))
;Value: make-sum


(define (make-product a b)
  (cond ((or (=number? a 0) (=number? b 0)) 0)
	((=number? a 1) b)
	((=number? b 1) a)
	((and (number? a) (number? b) (* a b)))
	(else (list a '* b))))
;Value: make-product


(define (sum? exp)
  (and (pair? exp) (eq? (cadr exp) '+)))
;Value: sum?


(define (addend sum)
  (car sum))
;Value: addend


(define (augend sum)
  (if (null? (cdddr sum))
      (caddr sum)
      (cddr sum)))
;Value: augend


(define (product? exp)
  (and (pair? exp) (eq? (cadr exp) '*)))
;Value: product?


(define (multiplier prod)
  (car prod))
;Value: multiplier


(define (multiplicand prod)
  (if (null? (cdddr prod))
      (caddr prod)
      (cddr prod)))
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

(define (reverse items)
  (if (null? items)
      items
      (append (reverse (cdr items)) (list (car items)))))
;Value: reverse

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
;Value: append


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

(deriv '(x + 3 * (x + y + 2)) 'x)
;Value: 4

(deriv '(x + 3 * (x + y + 2)) 'y)
;Value: 3

(deriv '(x + 3) 'x)
;Value: 1

(deriv '(x * y * (x + 3)) 'x)
;Value 15: ((x * y) + (y * (x + 3)))

