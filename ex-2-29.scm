;; Exercise 2.29
;;
;; Description:
;;
;; A binary mobile consists of two branches, a left branch and a right branch.
;; Each branch is a rod of a certain length, from which hangs either a weight
;; or another binary mobile.
;;
;; A. Write the corresponding selectors `left-branch` and `right-branch`, which
;;    return the branches of a mobile, and `branch-length` and `branch-structure`
;;    which return the components of a branch.
;;    
;; B. Define a procedure `total-weight` that returns the total weight of a mobile.
;;
;; C.

;; Definitions:

(define (make-mobile left right)
  (list left right))
;Value: make-mobile

(define (make-branch length structure)
  (list length structure))
;Value: make-branch

(define (left-branch mobile)
  (car mobile))
;Value: left-branch

(define (right-branch mobile)
  (car (cdr mobile)))
;Value: right-branch

(define (branch-length branch)
  (car branch))
;Value: branch-length

(define (branch-structure branch)
  (car (cdr branch)))
;Value: branch-structure

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))
;Value: total-weight

(define (branch-weight branch)
  (let ((struct (branch-structure branch)))
    (if (not (pair? struct))
	struct
	(total-weight branch))))
;Value: branch-weight

;; Testing:

(define m0 (make-mobile (make-branch 1 10)
			(make-branch 2 20)))
;Value: m0

(define m1 (make-mobile (make-branch 3 30)
			m0))
;Value: m1

(define m2 (make-mobile m1 m0))
;Value: m2


(left-branch m0)
;Value 13: (1 10)

(right-branch m0)
;Value 14: (2 20)

(branch-length (left-branch m0))
;Value: 1

(branch-structure (left-branch m0))
;Value: 10

(total-weight m0)
;Value: 30

(total-weight m1)
;Value: 60

(total-weight m2)
;Value: 90

