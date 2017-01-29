;; Exercise 2.74
;;
;; Description:
;;
;; Consider a large enterprise software system that features a large number of files
;; all implemented with different data structures. Show how data-directed programming
;; can be used to integrate the files while preserving their structural autonomy. Each
;; file is a set of records keyed by the employees name, and each employee's record is
;; also a set keyed under identifiers `address` and `salary`.
;;
;;    a. Implement a `get-record` procedure that retrieves a specified employee's record
;;       from a specified personnel file.
;;
;;    b. Implement a `get-salary` procedure that returns the salary information from a given
;;       employee's record from any division's personnel file.
;;
;;    c. Implement a `find-employee-record` procedure that searches all divisions' files for
;;       the record of a given employee and return the record.
;;
;;    d. When a new division must be integrated, what changes must be made to incorporate new
;;       personnel information into the central system?



;; Solutions:
;;
;; a.
;;
;; The following solution requires that each departments `file` data structure implement
;; `dept-name` selector which retrieves the type-tag information, and the `dept-data`
;; selector which retrieves the set of employee records operand to the operation found
;; in the generic table lookup.

(define (get-record employee division)
  ((get division 'get-record) employee))
;Value: get-record


;; b.
;;
;; The following solution delegates the record lookup to the previously defined `get-record`
;; procedure. With this record, the procedure does a lookup on the proper implementation
;; for `get-salary` according to the division name, in order to properly obtain the division name
;; the `employee-record` data structure must have this field as at its `car`.
(define (get-salary employee-record)
  ((get (car employee-record) 'get-salary) (cdr employee-record)))
;Value: get-salary


;; c.
;;
;; The following solution travers the divisions list until either the list is empty, no matching
;; record, or a particular division returns a match for the employee. The procedure delegates
;; the implementation lookup of `get-record` the procedure defined above.
(define (find-employee-record employee division-list)
  (if (null? division-list)
      false 
       (let ((record (get-record employee (car division-list))))
	 (if record
	     record
	     (find-employee-record employee (cdr division-list))))))
;Value: find-employee-record


;; d.
;;
;; In order to integrate a new division the division must implement the `dept-data` and
;; `dept-name` selectors for the data structure composing their files. The division is
;; then free to implement record selectors however they choose, as long as the selectors
;; are tagged with the department name registered in the operation lookup table with their
;; concrete implementation.



;; Definitions:


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


;; === Testing Dataset ===
;;
;; Since the text does not provide explicit training data, the following dataset was adapted
;; from Curt Clifton's solution as part of the Omni Group's book club. Of Course, Omni Group FTW.
;; https://github.com/curtclifton/sicp-omni/blob/master/exercise-2-74.rkt

(define (acquire-vine)
  (define (tag record)
    (cons 'vine record))
  (define (has-employee? employee-name)
    (eq? employee-name 'curt))
  (define (get-employee-record employee-name)
    ; Would use the employee-name to look up a record in the division's file, but we'll fake it:
    (if (has-employee? employee-name)
	(tag (list 'seattle employee-name 100000))
        false))
  (define (get-employee-address employee-record)
    (car employee-record))
  (define (get-employee-salary employee-record)
    (caddr employee-record))
  ;; installation
  (put 'vine 'get-record get-employee-record)
  (put 'vine 'get-address get-employee-address)
  (put 'vine 'get-salary get-employee-salary)
  'acquired)
;Value: acquire-vine

(define (acquire-periscope)
  (define (tag record)
    (cons 'periscope record))
  (define (has-employee? employee-name)
    (eq? employee-name 'drew))
  (define (get-employee-record employee-name)
    (if (has-employee? employee-name)
      (tag (list employee-name 120000 'temecula))
      false))
  (define (get-employee-salary employee-record)
    (cadr employee-record))
  (define (get-employee-address employee-record)
    (caddr employee-record))
  ;; installation
  (put 'periscope 'get-record get-employee-record)
  (put 'periscope 'get-salary get-employee-salary)
  (put 'periscope 'get-address get-employee-address)
  'acquired)
;Value: acquire-periscope

(acquire-vine)
;Value: acquired

(acquire-periscope)
;Value: acquired

(define divisions '(vine periscope))
;Value: divisions


;; Testing:

(get-record 'curt 'vine)
;Value 17: (vine seattle curt 100000)

(get-record 'drew 'periscope)
;Value 18: (periscope drew 120000 temecula)

(get-record 'bob 'vine)
;Value: #f

(get-salary (get-record 'drew 'periscope))
;Value: 120000

(get-salary (get-record 'curt 'vine))
;Value: 100000

(find-employee-record 'drew divisions)
;Value 31: (periscope drew 120000 temecula)

(find-employee-record 'alice divisions)
;Value: #f

(find-employee-record 'curt divisions)
;Value 32: (vine seattle curt 100000)

