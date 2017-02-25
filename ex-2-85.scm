;; Exercise 2.85:

;; Description:
;;
;; Write a `drop` procedure that drops an object as far as possible by testing
;; coercions to lower types. Install the required `project` procedure as a
;; generic operation in the 'general-arithmetic' package. Use the `drop`
;; procedure to rewrite `apply-generic` so that it simplifies its answers.


;; Imports:
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

;; in the 'generic-arithmetic' package
(define (project x) (apply-generic 'project x))

;; in the 'operation-table'package
(define (drop number)
  (let ((proj (get 'project (list (type-tag number)))))
    (if proj
	(let ((dropped (proj (contents number))))
	  (let ((raised (raise dropped)))
	    (if (equ? number raised)
		(drop dropped)
		number)))
	number)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (let ((res (apply proc (map contents args))))
	    (cond ((eq? op 'raise) res)
		  ((eq? op 'equ?) res)
		  (else (drop res))))
	  (if (homogenous? type-tags)
	      (error "No method for these types"
		     (list op type-tags))
	      (let ((rank1 (type-rank (type-tag (car args))))
		    (rank2 (type-rank (type-tag (cadr args)))))
		(if (< rank1 rank2)
		    (apply-generic op (raise (car args)) (cadr args))
		    (apply-generic op (car args) (raise (cadr args))))))))))

;; in the 'complex' package

(put 'project '(complex)
     (lambda (z) ((get 'make 'rational) (real-part z) 1)))

;; in the 'rational' package

(put 'project '(rational)
     (lambda (r) ((get 'make 'scheme-number) (integer-round (numer r)
							    (denom r)))))



;; Testing:
(drop (make-complex-from-real-imag 3 4))
;Value 24: (complex rectangular 3 . 4)

(drop (make-complex-from-real-imag 3 0))
;Value: 3

(drop (make-rational 2 1))
;Value: 2

(drop (make-rational 2 3))
;Value 13: (rational 2 . 3)

(add 5 (make-complex-from-real-imag 8 0))
;Value: 13

(add (make-rational 8 1) 5)
;Value: 13

(add (make-rational 3 4) (make-complex-from-real-imag 5 0))
;Value 16: (rational 23 . 4)

(add (make-complex-from-real-imag 4 5) (make-complex-from-real-imag 2 3))
;Value 17: (complex rectangular 6 . 8)

(add (make-complex-from-real-imag 4 0) (make-complex-from-real-imag 2 0))
;Value: 6

(add (make-rational 4 1) (make-rational 5 1))
;Value: 9

(add 10 (make-rational 1 3))
;Value 18: (rational 31 . 3)
