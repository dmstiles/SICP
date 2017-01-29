;; Exercise 2.73
;;
;; Description:
;;
;; The `deriv` procedure from section 2.3.2 can be regarded as a procedure that performs
;; a dispatch on the type of expression to be differentiated. The "type tag" of the
;; datum is the algebraic operator symbol, and the operation is `deriv`. Consider the
;; transformation of the program into data-directed style shown below.
;;
;;    a. Explain the transformation below, why can't predicates `number?` and `same-variable`
;;       be assimilated into the data-directed dispatch?
;;
;;    b. Write the procedures for derivatives of sums and product, and the auxiliary code
;;       code required to install them in the table used by the new `deriv` procedure.
;;
;;    c. Install a new differentaition rule into the data-directed system.
;;
;;    d. What corresponding changes are needed if dispatch line in `deriv` appeared as such:
;;
;;          ((get (operator exp) 'deriv) (operands exp) var)



;; Definitions:

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	(else ((get 'deriv (operator exp)) (operands exp)
	       var))))
;Value: deriv


(define (operator exp) (car exp))
;Value: operator


(define (operands exp) (cdr exp))
;Value: operands


(define (variable? x) (symbol? x))
;Value: variable?


(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
;Value: same-variable?


(define (=number? sym num)
  (and (number? sym) (= sym num)))
;Value: =number?


(define (install-sum-package)
  (define (deriv-sum sum var)
    (make-sum (deriv (addend sum) var)
	      (deriv (augend sum) var)))
  (define (addend sum)
    (car sum))
  (define (augend sum)
    (cadr sum))
  (define (make-sum a b)
    (cond ((=number? a 0) b)
	  ((=number? b 0) a)
	  ((and (number? a) (number? b)) (+ a b))
	  (else (list '+ a b))))

  ;; System interface
  (put 'deriv '+ deriv-sum)
  'done)
;Value: install-sum-package


(define (install-product-package)
  (define (deriv-product product var)
    (make-sum (make-product (multiplier product)
			    (deriv (multiplicand product) var))
	      (make-product (deriv (multiplier product) var)
			    (multiplicand product))))
  (define (multiplier product)
    (car product))
  (define (multiplicand product)
    (cadr product))
  (define (make-product a b)
    (cond ((or (=number? a 0) (=number? b 0)) 0)
	  ((=number? a 1) b)
	  ((=number? b 1) a)
	  ((and (number? a) (number? b) (* a b)))
	  (else (list '* a b))))
  
  ;; System interface.
  (put 'deriv '* deriv-product)
  'done)
;Value: install-product-package


(define (install-exponentiation-package)
  (define (deriv-exponentiation exponentiation var)
    (make-product (exponent exponentiation)
		  (make-exponentiation (base exponentiation)
				       (- (exponent exponentiation) 1))))
  (define (make-exponentiation a b)
    (cond ((=number? a 0) 0)
	  ((=number? a 1) 1)
	  ((=number? b 0) 1)
	  ((=number? b 1) a)
	  ((and (number? a) (number? b)) (^ a b))
	  (else (list '^ a b))))
  (define (base exp)
    (car exp))
  (define (exponent exp)
    (cadr exp))
  (define (^ base exponent)
    (define (*^ exponent acc)
      (if (= exponent 0)
	  acc
	  (*^ (- exponent 1) (* acc base))))
    (*^ exponent 1))

  ;; System interface.
  (put 'deriv '^ deriv-exponentiation)
  'done)
;Value: install-exponentiation-package


;; === Table Object ===
;;
;; Since the `put` and `get` operations referenced in the procedures above are not implemented
;; by the core of mit-scheme the table implementation here comes from future section 3.3.3

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key-2 (cdr subtable))))
	      (if record
		  (cdr record)
		  false))
	    false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key-2 (cdr subtable))))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable
			    (cons (cons key-2 value)
				  (cdr subtable)))))
	    (set-cdr! local-table
		      (cons (list key-1
				  (cons key-2 value))
			    (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "Unknown operation -- TABLE" m))))
    dispatch))
;Value: make-table


(define operation-table (make-table))
;Value: operation-table


(define get (operation-table 'lookup-proc))
;Value: get


(define put (operation-table 'insert-proc!))
;Value: put


;; Testing:

(install-sum-package)
;Value: done

(install-product-package)
;Value: done

(install-exponentiation-package)
;Value: done

(deriv '(+ x 1) 'x)
; Value: 1

(deriv '(+ (* 2 x) (* 5 x)) 'x)
; Value: 7

(deriv '(+ (* 5 (^ x 2)) (* 7 x)) 'x)
;Value 16: (+ (* 5 (* 2 x)) 7)


;; Solutions:
;;
;; a. The program is now reduced to its base cases, and a dispatch on the recursive call to
;;    further differentiate an expression. Since `number?` and `same-variable?` operate on
;;    primitive data there will be no type tag (operator symbol) to dispatch on, therefore
;;    they can not be assimilated into the data-directed dispatch.
;;
;; b. (See above)
;;
;; c. (See above)
;;
;; d. The installation packages would simply have to index first by the algebraic symbol, as the
;;    operation, and then as the 'deriv symbol, as the type, still returning the same `deriv`
;;    procedure defined within the corresponding package for the given expression denoted by
;;    the algebraic symbol.
