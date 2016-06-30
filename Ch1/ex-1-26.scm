;; Exercise 1.26
;;
;; Question.
;;
;; Explain how the following definition of `expmod` transforms
;; the original O(log(n)) order of growth to an O(n) order of
;; growth.

;; Definitions.

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (* (expmod base (/ exp 2) m)
		       (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))
;Value: expmod


;; Testing.

(expmod 2 4 3)
;Value: 1


;; Solution.
;;
;; Instead of generating a linear recursive process which reduces
;; the size of the exponent being computed by a factor of 2 with
;; each call to expmod, thus giving a log(exp) number of calls to
;; reach the base case. The process above generates a tree recursive
;; process that increaes the amount of work by a factor of 2 for 
;; each subsequent stack frame, thus offsetting the reduced work
;; achieved intially by reducing the number of stack frames required
;; to reach the base case. This results in an order of growth linear
;; to the size of exp.

		       