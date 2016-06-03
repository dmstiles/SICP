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

;; B. Wht is the order of growth in space and computation (as funciton of a) used
;; by the process when (sine a) is evaluated?


;; Solution.

;; A. 5