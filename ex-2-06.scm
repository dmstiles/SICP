;; Exercise 2.6
;;
;; Description:
;;
;; The definitions below formulate a representation for natural numbers
;; using Church numerals. Define `one` and `two` directly and give a direct
;; definition of the `+` procedure using Church numerals.



;; Definitions:

(define zero (lambda (f) (lambda (x) x)))
;Value: zero


(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
;Value: add-1


(define one (lambda (f) (lambda (x) (f x))))
;Value: one


(define two (lambda (f) (lambda (x) (f (f x)))))
;Value: two


(define (add-church a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))
;Value: add-church

;; Used in testing the number of applications of a procedure.
(define (inc x) (+ x 1))
;Value: inc


;; Testing:

((zero inc) 0)
;Value: 0

((zero inc) 1)
;Value: 1

((one inc) 0)
;Value: 1

((two inc) 0)
;Value: 2

(((add-church one two) inc) 0)
;Value: 3

(((add-church (add-church one two) (add-church two two)) inc) 0)
;Value: 7

