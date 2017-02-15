;; Exercise 2.84

;; Description:
;;
;; Use the `raise` operation to modify `apply-generic` soo that it coerces arguments
;; to have the same type by method of succesive raising. Devise the implementation so
;; that it is compatible with the rest of the system and allows for additions to
;; the tower of types.



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



;; in the 'operation-table' package
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (homogenous? type-tags)
	      (error "No method for these types"
		     (list op type-tags))
	      (let ((rank1 (type-rank (type-tag (car args))))
		    (rank2 (type-rank (type-tag (cadr args)))))
		(if (< rank1 rank2)
		    (apply-generic op (raise (car args)) (cadr args))
		    (apply-generic op (car args) (raise (cadr args))))))))))


;; in the 'generic-arithmetic' package
(define type-tower (list 'scheme-number 'rational 'real 'complex))

(define (type-rank t)
  (define (iter t types n)
    (cond ((null? types) (error "Unkown type -- TYPE-RANK" t))
	  ((eq? t (car types)) n)
	  (else (iter t (cdr types) (+ n 1)))))
  (iter t type-tower 0))



;; Testing:

(define test-n 5)
;Value: test-n

(define test-rat (make-rational 22 7))
;Value: test-rat

(define test-com (make-complex-from-real-imag 1 2))
;Value: test-com

(add test-n test-com)
;Value 18: (complex rectangular 6 . 2)

(add test-n test-rat)
;Value 25: (rational 57 . 7)

(add test-rat test-com)
;Value 26: (complex rectangular 29/7 . 2)

