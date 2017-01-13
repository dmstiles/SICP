;; Exercise 2.59
;;
;; Description:
;;
;; Implement the `union-set` operation for the unordered-list representation of sets.


;; Definitions:

(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	((element-of-set (car set1) set2)
	 (union-set (cdr set1) set2))
	(else
	 (cons (car set1) (union-set (cdr set1) set2)))))
;Value: union-set


(define (element-of-set e set)
  (cond ((null? set) false)
	((equal? e (car set)) true)
	(else (element-of-set e (cdr set)))))
;Value: element-of-set


;; Testing:

(element-of-set '3 '(5 8 2 3 4))
;Value: #t

(element-of-set 'a '(b c d e f))
;Value: #f

(union-set '(a b c) '(d e f))
;Value 18: (a b c d e f)

(union-set '(a b c d) '(c d e f))
;Value 19: (a b c d e f)

(union-set '(a b c) '())
;Value 20: (a b c)

(union-set '() '(d e f))
;Value 22: (d e f)

(union-set '() '())
;Value: ()
