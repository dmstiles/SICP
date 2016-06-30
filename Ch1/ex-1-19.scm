;; Exercise 1.19
;; Consider the family of transformations Tpq where Tpq transforms pair
;; (a,b) according to a = bq + aq + ap and b = bp + aq.
;;
;; Show that if we apply such a transformation Tpq twice, the effect is
;; the same as using a single transformation Tp'q' of the same form, and
;; compute p' and q' in terms of p and q.
;;
;; This transformation gives an explicit way to square these transformations
;; and thus allows one to compute T^n using successive squaring, which runs
;; in a logarithmic number of steps.


;; Definitions.
(define (square x)
  (* x x))
;Value: square

(define (fib n)
  (fib-iter 1 0 0 1 n))
;Value: fib

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
	((even? count)
	 (fib-iter a
		   b
		   (+ (square p) (square q))
		   (+ (* 2 p q) (square q))
		   (/ count 2)))
	(else
	 (fib-iter (+ (* b q) (* a q) (* a p))
		   (+ (* b p) (* a q))
		   p
		   q
		   (- count 1)))))
;Value: fib-iter


;; Testing.

(fib 0)
;Value: 0

(fib 1)
;Value: 1

(fib 2)
;Value: 1

(fib 3)
;Value: 2

(fib 10)
;Value: 55



;; TODO formalize reasoning (algebra)