;; Exercise 1.13
;; Prove that Fib(n) is the closest integer to (phi^n)/5
;; where phi = (1 + (5)^(1/2))/2
;;
;; Solved with assistance from:
;; http://www.billthelizard.com/2009/12/sicp-exercise-113-fibonacci-and-golden.html

(define phi
  (/ (+ 1 (sqrt 5)) 2))
;Value: phi

(define psi 
  (/ (- 1 (sqrt 5)) 2))
;Value: psi


;; Testing.
(+ phi 0)
;Value: 1.618033988749895

(+ psi 0)
;Value: -.6180339887498949


;; Creating a procedure for calculating exponents.
(define (^ base exponent)
  (define (*^ exponent acc)
    (if (= exponent 0)
	acc
	(*^ (- exponent 1) (* acc base))))
  (*^ exponent 1))
;Value: ^

;; Testing.
(^ 2 10)
;Value: 1024

;; Quantifying (phi^n)/(5^(1/2)).
(define (f n)
  (/ (^ phi n) (sqrt 5)))
;Value: f


;; Testing how closely the procedure `f` tracks with the 
;; Fibonacci sequence.
(f 0)
;Value: .4472135954999579

(f 1)
;Value: .7236067977499789

(f 2)
;Value: 1.1708203932499368

(f 3)
;Value: 1.8944271909999157

(f 4)
;Value: 3.0652475842498528

(f 5)
;Value: 4.959674775249769

(f 6)
;Value: 8.024922359499621

;; Rounding the preceding values reveals the Fibonacci sequence.
;; 0, 1, 1, 2, 3, 5, 8, ...


;; Using induction and the definition of Fibonacci numbers
;; to prove that Fib(n) = ((phi^n) - (psi^n))/(5^(1/2))

;; The base cases.

;; Defining a procdure to compute a Fibonacci number based
;; upon the definition of the Fibonacci sequence.
(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1))
		 (fib (- n 2))))))
;Value: fib

;; Testing.
(fib 1)
;Value: 1

(fib 2)
;Value: 1

(fib 6)
;Value: 8


;; Defining a procedure to compute the value of the function containing
;; phi and psi which returns a value related to the Fibonacci sequence at
;; the position defined by argument `n`.
(define (fib-alt n)
  (/ (- (^ phi n) (^ psi n))
     (sqrt 5)))
;Value: fib-alt

;; Testing.
(fib-alt 1)
;Value: 1.

(fib-alt 2)
;Value: 1.

(fib-alt 6)
;Value: 8.


;; It has been shown that (fib n ) = (fib-alt n).
;; As well as that (fib n-1) = (fib-alt n-1).
;; Through algebraic manipulation it can be shown that
;; (fib n+1) = (fib-alt n+1). Which concludes the
;; inductive proof.


;; Now, proving that Fib(n) is the closest integer to (phi^n)/(sqrt 5).

;; Fib(n) = (phi^n - psi^n)/(sqrt 5)
;; => Fib(n) = (phi^n)/(sqrt 5) - (psi^n)/(sqrt 5)
;; => Fib(n) - (phi^n)/(sqrt 5) = - (psi^n)/(sqrt 5)

;; In order to show that the next closest integer resulting from
;; the funciton (phi^n)/(sqrt 5) is equivalent to the Fib(n) it would
;; follow that the difference between Fib(n) and (phi^2)/(sqrt 5)
;; would be less than 1/2.

;; Therefore we want to show that (psi^n)/(sqrt 5) <= 1/2
;; Or, alternatively, that (psi^n) <= (sqrt 5)/2

;; Since
psi
;Value: -.6180339887498949

;; And the value of n will always be a positive integer.
;; Then psi^n will always be <= 1 in this context.

;; Also since
(/ (sqrt 5) 2)
;Value: 1.118033988749895

;; And psi is always less then 1, it has been proven that
;; (psi^n)/(sqrt 5) is always less than 1/2

;; Therefore since the difference between Fib(n) and (phi^n)/(sqrt 5)
;; is equal to (psi^n)/(sqrt 5)
;; And (psi^n)/(sqrt 5) is always less than 1/2
;; It has been proven that (phi^n)/(sqrt 5) will always be within
;; 1/2 of the Fib(n), otherwise regarded as the closest integer.