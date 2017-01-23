;; Exercise 2.66
;;
;; Description:
;;
;; Implement the `lookup` procedure for the case where the set of records is structured
;; as a binary tree, ordered by the numerical value of the keys.


;; Definitions:

(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      false
      (let ((entry-key (key (entry set-of-records))))
	(cond ((equal? given-key entry-key)
	       (entry set-of-records))
	      ((< given-key entry-key)
	       (lookup given-key (left-branch set-of-records)))
	      ((> given-key entry-key)
	       (lookup given-key (right-branch set-of-records)))))))
;Value: lookup


(define (key record) (car record))
;Value: key


(define (data record) (cdr record))
;Value: data


(define (make-record key data) (cons key data))
;Value: make-record


(define (list->tree elements)
  (car (partial-tree elements (length elements))))
;Value: list->tree


(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
	(let ((left-result (partial-tree elts left-size)))
	  (let ((left-tree (car left-result))
		(non-left-elts (cdr left-result))
		(right-size (- n (+ left-size 1))))
	    (let ((this-entry (car non-left-elts))
		  (right-result (partial-tree (cdr non-left-elts)
					      right-size)))
	      (let ((right-tree (car right-result))
		    (remaining-elts (cdr right-result)))
		(cons (make-tree this-entry left-tree right-tree)
		      remaining-elts))))))))
;Value: partial-tree


(define (make-tree entry left right)
  (list entry left right))
;Value: make-tree




(define (entry tree) (car tree))
;Value: entry


(define (left-branch tree) (cadr tree))
;Value: left-branch


(define (right-branch tree) (caddr tree))
;Value: right-branch


;; Testing:

(define database
  (list (make-record 1 'John)
	(make-record 2 'Paul)
	(make-record 3 'George)
	(make-record 4 'Ringo)))
;Value: database

(define tree-db (list->tree database))
;Value: tree-db

tree-db
;Value 13: ((2 . paul) ((1 . john) () ()) ((3 . george) () ((4 . ringo) () ())))


(lookup 1 tree-db)
;Value 14: (1 . john)

(lookup 2 tree-db)
;Value 15: (2 . paul)

(lookup 3 tree-db)
;Value 16: (3 . george)

(lookup 4 tree-db)
;Value 17: (4 . ringo)

(lookup 5 tree-db)
;Value: #f

		     
		 
