;; Exercise 2.82

;; Description:
;;
;; Generalize `apply-generic` to handle coercion in the case of multiple arguments.
;; Give an example of a case where this strategy is not sufficiently general.


;; Definitions:
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

;; defined in the 'operation-table' package

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (homogenous? type-tags)
	      (error "No method for these types"
		     (list op type-tags))
	      (coerce type-tags args op))))))

(define (coerce type-tags args op)
  (if (null? type-tags)
      (error "No coercion for these types"
	     (map type-tag args))
      (let ((type (car type-tags)))
	(let ((coercions (apply-coercion type args)))
	  (if coercions
	      (apply apply-generic (cons op coercions))
	      (coerce (cdr type-tags) args op))))))

(define (apply-coercion type args)
  (define (iter type args items)
    (if (null? args)
	items
	(let ((arg (car args)))
	  (if (eq? type (type-tag arg))
	      (iter type (cdr args) (cons arg items))
	      (let ((coerce-proc (get-coercion (type-tag arg) type)))
		(if coerce-proc
		    (iter type
			  (cdr args)
			  (cons (coerce-proc arg) items))
		    false))))))
  (iter type args '()))
		    
(define (homogenous? items)
  (define (iter first rest)
    (cond ((null? rest) true)
	  ((eq? first (car rest))
	   (iter (car rest) (cdr rest)))
	  (else false)))
  (iter (car items) (cdr items)))
      

(define (contains? elem items)
  (cond ((null? items) false)
	((eq? elem (car items)) true)
	(else (contains? elem (cdr items)))))


;; Testing:

(define (addd x y z)
  (apply-generic 'addd x y z))

(define (real-part z)
  (apply-generic 'real-part z))

(define (imag-part z)
  (apply-generic 'imag-part z))

(put 'addd '(complex complex complex)
     (lambda (z1 z2 z3)
       (make-complex-from-real-imag (+ (real-part z1)
				       (real-part z2)
				       (real-part z3))
				    (+ (imag-part z1)
				       (imag-part z2)
				       (imag-part z3)))))

     
(define a-comp (make-complex-from-real-imag 1 2))

(define b-comp (make-complex-from-real-imag 3 4))

(define a 5)

(define b 6)

;; Test same-type operations
(add a b)
;Value: 11

(add a-comp b-comp)
;Value 13: (complex rectangular 4 . 6)

;; Test mixed-type opeartions
(add a a-comp)
;Value 14: (complex rectangular 6 . 2)

;; Test greater than 2 operands
(addd a a-comp b-comp)
;Value 15: (complex rectangular 9 . 6)

(addd a b a-comp)
;Value 16: (complex rectangular 12 . 2)

;; Solution:
;;
;; This strategy does not allow for coercion to shared supertype's since the type
;; coercions tried come only from the supplied arguments. Therefore a valid operation
;; may exist between two arguments who share a superclass, however neither will be coerced
;; to this class and the operation defined for this superclass will not be found.
