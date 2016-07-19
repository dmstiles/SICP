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
;; C. Design a predicate that tests whether a binary mobile is balanced.
;;
;; D. How much would the preceding programs need to be changed if mobiles were
;;    represented in terms of `cons` and not `list`?


;; Definitions:

(define (make-mobile left right)
  (cons left right))
;Value: make-mobile

(define (make-branch length structure)
  (cons length structure))
;Value: make-branch

(define (left-branch mobile)
  (car mobile))
;Value: left-branch

(define (right-branch mobile)
  (cdr mobile))
;Value: right-branch

(define (branch-length branch)
  (car branch))
;Value: branch-length

(define (branch-structure branch)
  (cdr branch))
;Value: branch-structure

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))
;Value: total-weight

(define (branch-weight branch)
  (let ((struct (branch-structure branch)))
    (if (not (pair? struct))
	struct
	(total-weight struct))))
;Value: branch-weight

(define (balanced? mobile)
  (let ((lb (left-branch mobile))
	(rb (right-branch mobile)))
    (and (= (* (branch-length lb) (branch-weight lb))
	    (* (branch-length rb) (branch-weight rb)))
	 (balanced-branch? lb)
	 (balanced-branch? rb))))
;Value: balanced?

(define (balanced-branch? b)
  (if (not (pair? (branch-structure b)))
      true
      (balanced b)))
;Value: balanced-branch?


;; Testing:

(define m0 (make-mobile (make-branch 1 10)
			(make-branch 2 20)))
;Value: m0

(define m1 (make-mobile (make-branch 3 30)
			(make-branch 4 m0)))
;Value: m1

(define m2 (make-mobile (make-branch 5 m0)
			(make-branch 6 m1)))
;Value: m2

(define m3 (make-mobile (make-branch 2 10)
			(make-branch 1 20)))
;Value: m3

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

(total-weight m3)
;Value: 30

(balanced? m3)
;Value: #t

(balanced? m2)
;Value: #f


;; Solution.
;;
;; By changing the constructors of `make-mobile` and `make-branch` to use `cons`
;; instead of `list` only the preceding definitions for selectors on mobiles and
;; branches require change. Since the representations for these object now use a 
;; simple pair as opposed to a list the forms `car` and `cdr` address the target
;; item without requiring the additional `car` filtering required when an object
;; is not stored at the head of a list. Since these implementation details are 
;; abstracted behind the respective selectors, procedures operating through the
;; selector interface do not require any modification.
