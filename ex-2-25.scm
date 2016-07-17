;; Exercise 2.25
;;
;; Description:
;;
;; Give the combination of `car`s and `cdr`s that will select 7 from
;; each of the following lists.


;; Definitions:

(define list1 (list 1 3 (list 5 7)))
;Value: list1

(define list2 (list (list 7)))
;Value: list2

(define list3 (list 1
		    (list 2
			  (list 3
				(list 4
				      (list 5
					    (list 6 7)))))))
;Value: list3



;; Solutions:

(car (cdr (car (cdr (cdr list1)))))
;Value: 7

(car (car list2))
;Value: 7

;;        (6 7)     (5 *)     (4 *)     (3 *)     (2 *)
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr list3))))))))))))
;Value: 7

