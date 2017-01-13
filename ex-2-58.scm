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
;;        (x + 3 * (x + y + 2))



;; Definitions:

(define (var? x) (symbol? x))
;Value: variable?


(define (same-variable? v1 v2)
  (and (var? v1) (var? v2) (eq? v1 v2)))
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
	((and (number? a) (number? b)) (* a b))
	(else (list a '* b))))
;Value: make-product


(define (sum? exp)
  (and (pair? exp)
       (contains? exp '+)))
;Value: sum?


(define (addend sum)
  (define (add-in head tail)
    (if (eq? (cadr tail) '+)
	(append head (list (car tail)))
	(add-in (append head (list (car tail) (cadr tail)))
		(cddr tail))))
  (simplify (add-in '() sum)))
;Value: addend


(define (augend sum)
  (define (aug-in tail)
    (if (eq? (cadr tail) '+)
	(cddr tail)
	(augend (cddr tail))))
  (simplify (aug-in sum)))
;Value: augend


;; Delay multiplication while sums exist.
;; Because recurision reverse the execution order.
(define (product? exp)
  (and (pair? exp)
       (not (sum? exp))
       (contains? exp '*)))
;Value: product?


(define (multiplier prod)
  (define (lier-in head tail)
    (if (eq? (cadr tail) '*)
	(append head (list (car tail)))
	(lier-in (append head (list (car head) (cadr head)))
		 (cddr tail))))
  (simplify (lier-in '() prod)))
;Value: multiplier


(define (multiplicand prod)
  (define (licand-in tail)
    (if (eq? (cadr tail) '*)
	(cddr tail)
	(multiplicand (cddr tail))))
  (simplify (licand-in prod)))
;Value: multiplicand


;; Delay exponentiation while sums and product exist.
;; Because recurision reverse the execution order.
(define (exponentiation? exp)
  (and (pair? exp)
       (not (sum? exp))
       (not (product? exp))
       (contains? exp '^)))
	    
;Value: exponentiation?


(define (base exp)
  (define (base-in head tail)
    (if (eq? (cadr tail) '^)
	(append head (list (car tail)))
	(base-in (append head (list (car head) (cadr head)))
		 (cddr tail))))
  (simplify (base-in '() exp)))
;Value: base

(define (exponent exp)
  (simplify (if (eq? (cadr exp) '^)
		(cddr exp)
		(exponent (cddr exp)))))
;Value: exponent

(define (make-exponentiation a b)
  (cond ((=number? a 0) 0)
	((=number? a 1) 1)
	((=number? b 0) 1)
	((=number? b 1) a)
	((and (number? a) (number? b)) (^ a b))
	(else (list a '^ b))))
;Value: make-exponentiation


(define (^ base exponent)
  (define (*^ exponent acc)
    (if (= exponent 0)
        acc
        (*^ (- exponent 1) (* acc base))))
  (*^ exponent 1))
;Value: ^


(define (contains? seq sym)
  (cond ((null? seq) false)
	((eq? (car seq) sym) true)
	(else (contains? (cdr seq) sym))))
;Value: contains?


(define (simplify seq)
  (cond ((not (pair? seq)) seq)
	((null? (cdr seq)) (car seq))
	(else seq)))
;Value: simplify


(define (deriv exp var)
  (cond ((number? exp) 0)
	((var? exp)
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

(contains? '(a b c 1 2 3) '1)
;Value: #t

(contains? '(a b c 1 2 3) 'x)
;Value: #f

(addend '(2 * 3 + 4 * 6))
;Value 18: (2 * 3)

(augend '(2 * 3 + 4 * 6))
;Value 19: (4 * 6)

(sum? '(2 * 3 + 4 * 6))
;Value: #t

(product? '(2 * 3 + 4 * 6))
;Value: #f

(deriv '(x + 3 * (x + y + 2)) 'x)
;Value: 4

(deriv '(x + 3 * (x + y + 2)) 'y)
;Value: 3

(deriv '(x + 3) 'x)
;Value: 1

(deriv '(x * y * (x + 3)) 'x)
;Value 21: ((x * y) + (y * (x + 3)))

(deriv '(1 + 2 * x * 4 + 5) 'x)
;Value: 8

(deriv '(x * 3 + 5 * (x + y + 2)) 'x)
;Value: 8

(deriv '(3 + 4 * x ^ 2) 'x)
;Value 28: (4 * (2 * x))

(deriv '(x ^ 3 + 4 * x ^ 7) 'x)
;Value 30: ((3 * (x ^ 2)) + (4 * (7 * (x ^ 6))))

;; Not perfect yet...
(deriv '(x ^ 2 * 4 * x ^ 7) 'x)
;The object (), passed as an argument to safe-cdr, is not a pair.

;; But close enough.
(deriv '((x ^ 3) * 4 * (x ^ 7)) 'x)
;Value 32: (((x ^ 3) * (4 * (7 * (x ^ 6)))) + ((4 * (x ^ 7)) * (3 * (x ^ 2))))
;; => (x^3 * 28x^6) + (4x^7 * 3x^2)
;; => 28x^9 + 12x^9
;; => 40x^9
