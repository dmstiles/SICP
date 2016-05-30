;; Exercise 1-11
;; Write procedures that compute the function defined in the text by
;; both a recursive process and an iterative process.

;; As a recursive process.
(define (f-rec n)
  (if (< n 3) 
      n
      (+ (f-rec (- n 1))
	 (* 2 (f-rec (- n 2)))
	 (* 3 (f-rec (- n 3))))))
;Value: f-rec


;; As an iterative process.
(define (f-iter n)
  (f-iter-helper 0 1 2 n))
;Value: f-iter

(define (f-iter-helper a b c count)
  (if (= count 0)
      a
      (f-iter-helper b c (+ c (* 2 b) (* 3 a)) (- count 1))))
;Value: f-iter-helper


;; Testing.
(f-rec 2)
;Value: 2

(f-iter 2)
;Value: 2

(f-rec 5)
;Value: 25

(f-iter 5)
;Value: 25

