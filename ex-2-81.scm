;; Exercise 2.81

;; Description:
;;
;; `apply-generic`, as implemented initially, may try to coerce arguments of the
;; same type. Therefore one may reason the need for the following procedures:

(define (scheme-number->scheme-number n) n)

(define (complex->complex z) z)

(put-coercion 'scheme-number 'scheme-number
	      scheme-number->scheme-number)

(put-coercion 'complex 'complex complex->complex)

;; a. With the previous procedures installed, what happens if `apply-generic` is
;;    called with two arguments of the same type? Concretely, assume the following
;;    operations are defined:

(define (exp x y) (apply-generic 'exp x y))

;; installed in the scheme-number package
(put 'exp '(scheme-number scheme-number)
     (lambda (x y) (tag (expt x y))))

;;    What happens when `exp` is called with arguments of type `complex`?
;;
;;
;; b. Did the same type coercion procedures need to be defined in order for
;;    `apply-generic` to work?
;;
;; c. Modify `apply-generic` so that it doesn't coerce arguments of the same
;;    type?


;; Imports:

(load "packages/generic-arithmetic.scm")
;    Loading "packages/generic-arithmetic.scm"...
;      Loading "packages/operation-table.scm"... done
;      Loading "packages/complex.scm"...
;        Loading "packages/rectangular.scm"... done
;        Loading "packages/polar.scm"... done
;      ... done
;      Loading "packages/rational.scm"... done
;      Loading "packages/scheme-number.scm"... done
;    ... done


;; Definitions:

;; initial definition
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (= (length args) 2) ;; only consider binary ops
	      (let ((type1 (car type-tags))
		    (type2 (cadr type-tags))
		    (a1 (car args))
		    (a2 (cadr args)))
		(let ((t1->t2 (get-coercion type1 type2))
		      (t2->t1 (get-coercion type2 type1)))
		  (cond (t1->t2
			 (apply-generic op (t1->t2 a1) a2))
			(t2->t1
			 (apply-generic op a1 (t2->t1 a2)))
			(else
			 (error "No method for these types"
				(list op type-tags))))))
	  (error "No method for these types"
		 (list op type-tags)))))))



;; Solutions:
;;
;; a. If `exp` were called with two arguments of type `complex` the `apply-generic`
;;    procedure would first check to see if an implementation for `exp` was provided
;;    for arguments of type `complex` seeing that there wasn't, `apply-generic` would
;;    perform the coercion `complex->complex` resulting in unchanged arguments.
;;    `apply-generic` would then be called again, with the same arguments` resulting
;;    in infinite recursion.

(define z1 (make-complex-from-real-imag 1 1))

(define z2 (make-complex-from-real-imag 2 2))

(exp z1 z2)
;; never returns


;; b. Although the initial implementation will unnecessarily lookup coercion procedures
;;    arguments of the same type, the procedure still works since no coercion procedures
;;    will be found.

;; without installing `complex->complex` coercion procedure

(exp z1 z2)
;No method for these types (exp (complex complex))


;; c.

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (= (length args) 2) ;; only consider binary ops
	      (let ((type1 (car type-tags))
		    (type2 (cadr type-tags)))
		(if (eq? type1 type2) ;; preventing unnecessary coercion lookups
		    (error "No method for same types"
			   (list op type-tags))
		    (let ((a1 (car args))
			  (c2 (cadr args)))
		      (let ((t1->t2 (get-coercion type1 type2))
			    (t2->t1 (get-coercion type2 type1)))
			(cond (t1->t2
			       (apply-generic op (t1->t2 a1) a2))
			      (t2->t1
			       (apply-generic op a1 (t2->t1 a2)))
			      (else
			       (error "No method for these types"
				      (list op type-tags))))))))
	      (error "No method for these types"
		     (list op type-tags)))))))


(exp z1 z2)
;No method for same types (exp (complex complex))

(exp 2 10)
;Value: 1024

