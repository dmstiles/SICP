;; Exercise 1.3
;; Defining a procedure that takes three arguments and returns
;; the sum of squares for the two larger numbers.

(define (larger x y)
  (if (> x y) x y))
;Value: larger

(define (square x) (* x x))
;Value: square

(define (sum-of-squares x y)
  (+ (square x)
     (square y)))
;Value: sum-of-squares

(define (larger-sum-of-squares x y z)
  (cond ((and (> x y) (> x z)) (sum-of-squares x (larger y z)))
	((and (> y x) (> y z)) (sum-of-squares y (larger x z)))
	(else (sum-of-squares z (larger x y)))))
;Value: larger-sum-of-squares


;; Testing.

(larger-sum-of-squares 1 2 3)
;Value: 13

(larger-sum-of-squares 10 9 8)
;Value: 181

(larger-sum-of-squares -3 -4 5)
;Value: 34
