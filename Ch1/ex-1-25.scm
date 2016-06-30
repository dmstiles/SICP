;; Exercise 1.25
;;
;; One could argue that a simpler implementation for the
;; `expmod` procedure of exercise 1.26 would be define as
;; follows. Is this argument correct? Would this definition
;; serve as well for the fast prime tester?

;; Definitions.

(define (expmod base exp m)
 (remainder (fast-expt base exp) m))
;Value: expmod


(define (fast-expt base exp)
  (cond ((= exp 0) 1)
	((even? exp) (square (fast-expt base (/ exp 2))))
	(else (* base (fast-expt base (- exp 1))))))
;Value: fast-expt


(define (even? n)
  (= (remainder n 2) 0))
;Value: even?


(define (square n)
  (* n n))
;Value: square


;; Testing.
(square 3)
;Value: 9

(even? 100)
;Value: #t

(fast-expt 2 4)
;Value: 16

(expmod 2 4 3)
;Value: 1


;; Solution.
;;
;; While the `expmod` implementation using the `fast-expt` procedure
;; is correct in that it returns a proper solution, and also exhibits
;; the same log(n) order of growth as that of the original `expmod`
;; implementation. The original `expmod` procedure is preferred since
;; limits the size of operands to an upper bound equal to (m - 1) since
;; the `remainder` procedure is applied before returning a value from each
;; call to `expmod`. In contrast the implementation using the `fast-expt`
;; procedure delays the `remainder` operation until the end, resulting
;; in an much larger operand since the base is raised completely to the
;; given exp before performing the `remainder` procedure.




