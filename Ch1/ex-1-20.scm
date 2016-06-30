;; Exercise 1.20
;;
;; Using the substitution method, illustrate the process generated in
;; evaluating (gcd 206 40) and indicate the `remainder` operations that
;; actually performed.
;;
;; How many `remainder` operations are actually performed in the
;; normal-order evaluation of (gcd 206 40)?
;;
;; In the applicative-order evaluation?


;; Definitions.

(define (r a b)
  (remainder a b))
;Value: r

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (r a b))))
;Value: gcd


;; Solutions.

;; Illustrating the process using substitution (applicative-order) evaluation.
;;
;; (gcd 206 40)
;; => (gcd 40 (r 206 40)) => (gcd 40 6)
;; => (gcd 6 (r 40 6)) => (gcd 6 4)
;; => (gcd 4 (r 6 4)) => (gcd 4 2)
;; => (gcd 2 (r 4 2)) => (gcd 2 0)
;; => return 2
;; 
;; In total, 4 r operations are applied.
;;
;; Illustrating the process using normal-order evaluation.
;;
;; (gcd 206 40)
;; => if (= 40 0) #f => (gcd 40 (r A206 B40))
;; => if (= (r 206 40) 0) #f => (gcd (r 206 40) (r A40 B(r (206 40))))                                      ;; 1 eval in if
;; => if (= (r 40 (r 206 40))) 0) #f => (gcd (r 40 (r (206 40))) (r A(r 206 40) B(r 40 (r 206 40))))        ;; 2 eval in if
;; => if (= (r (r 206 40) (r 40 (r 206 40))) 0) #f    ;; 4 eval in if
;;    => (gcd (r (r 206 40) (r 40 (r 206 40))) (r A(r 40 (r (206 40))) B(r (r 206 40) (r 40 (r 206 40)))))
;; => if (= (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))) 0) #t                                    ;; 7 eval in if
;;    => return (r (r 206 40) (r 40 (r 206 40)))                                                            ;; 4 eval in return
;; 
;; In total 18 remainder `r` operations are performed. 14 to evaluate the if condition in the process,
;; and 4 in evaluating the return value once the if condition is satisfied.
