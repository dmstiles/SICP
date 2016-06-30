;; Exercise 1.15

;; Definitions.

(define (cube x) (* x x x))
;Value: cube

(define (p x) (- (* 3) (* 4 (cube x))))
;Value: p

(define (sine angle)
  (if (< (abs angle) 0.1)
      angle
      (p (sine (/ angle 3.0)))))
;Value: sine


;; Testing.

(sine 0)
;Value: 0

(sine 90)
;Value: #[+inf]


;; Question.

;; A. How many times is the procedure p appllied when (sine 12.15) is evaluated?

;; B. What is the order of growth in space and computation (as function of a) used
;; by the process when (sine a) is evaluated?


;; Solution.

;; A. 5

;; B. 
;;
;; number of procedure calls = (ceil (/ (log (/ angle / 0.1)) (log 3)))
;; 
;; The spatial order of growth for the `sine` procedure is logrithmic in the sense that 
;; the amount of state information required at any point in the computation is equivalent
;; to the depth of the procedure call stack at that point. Since the depth of the procedure
;; call stack will be equal to the log base 3 of the argument divided by 0.1 one will
;; observe a number of stack frames denoted by the equation above for any given `angle`.
;;
;; Like the spatial growth, the computational growth for the `sine` procedure grows logrithmicly
;; with the size of the input. This is due to the fact that as the argument grows in magnitude, 
;; the number of subsequent procedure calls required to reduce the argument within the 0.1 
;; magnitude base case will also grow. However each subsequent call only produces one extra unit
;; of computation (stack frame) and the number of stack frames will be given by a logrithmic
;; function. Therefrore requiring just O(log(n)) computational step to reach the base case.