;; operation-table.scm
;;
;; Description:
;;
;; A implementation lookup table for tagged data and their corresponding operations.


;; Definitions:

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


(define operation-table (make-table))

(define get (operation-table 'lookup-proc))

(define put (operation-table 'insert-proc!))

(define coercion-table (make-table))

(define get-coercion (coercion-table 'lookup-proc))

(define put-coercion (coercion-table 'insert-proc!))

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
	((pair? datum) (car datum))
	(else
	 error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (cond ((number? datum) datum)
	((pair? datum) (cdr datum))
	(else
	 (error "Bad tagged datum -- CONTENTS" datum))))

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
