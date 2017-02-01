;; ex-2-78.scm


;; Description:
;;
;; Modify the definitions of `type-tag`, `contents`, and `attach-tag` so that the
;; `scheme-number`package takes advantage of Scheme's internal type system. After
;; these changes the system should work as before except that ordinary numbers
;; will exist as Scheme numbers rather than explicity typed pairs whose car
;; is the symbol 'scheme-number'.


;; Imports:

(load "operation-table.scm")
;Loading "operation-table.scm"... done

(load "scheme-number.scm")
;Loading "scheme-number.scm"... done

;; Definitions:

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))
;Value: make-scheme-number

(define (add x y) (apply-generic 'add x y))
;Value: add

(define (sub x y) (apply-generic 'sub x y))
;Value: sub

;; Generic arithmetic with explicityly tagged ordinary numbers.

(define a (make-scheme-number 1))
;Value: a

a
;Value 13: (scheme-number . 1)

(define b (make-scheme-number 3))
;Value: b

b
;Value 14: (scheme-number . 3)

(add a b)
;Value 15: (scheme-number . 4)

(sub b a)
;Value 16: (scheme-number . 2)

;; Apply changes to represent oridinary numbers without explicit tags.

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))
;Value: attach-tag

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
	((pair? datum) (car datum))
	(else
	 error "Bad tagged datum -- TYPE-TAG" datum)))
;Value: type-tag

(define (contents datum)
  (cond ((number? datum) datum)
	((pair? datum) (cdr datum))
	(else
	 (error "Bad tagged datum -- CONTENTS" datum))))
;Value: contents

;; Generic arithmetic with implicitly tagged ordinary numbers.

(define a (make-scheme-number 1))
;Value: a

a
;Value: 1

(define b (make-scheme-number 3))
;Value: b

b
;Value: 3

(add a b)
;Value: 4

(sub b a)
;Value: 2


