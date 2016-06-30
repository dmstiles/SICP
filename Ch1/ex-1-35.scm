;; Exercise 1.35
;;
;; Definitions:
;;
;; Show that the golden ration is a fixed point of the transformation:
;;
;;    x => 1 + (1/x) 
;;
;; Use this fact to compute phi by means of the `fixed-point` procedure.


;; Definitions:

(define tolerance 0.00001)
;Value: tolerance


(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))
;Value: fixed-point


;; Testing:

(fixed-point cos 1.0)
;Value: .7390822985224023

(fixed-point (lambda (y) (+ (sin y) (cos y)))
	     1.0)
;Value: 1.2587315962971173

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)


;; Applying y = 1 + 1/x at phi. 
((lambda (x) (+ 1 (/ 1 x))) 1.61803)
;Value: 1.6180355123205379


;; Solutions:
;;
;; Proving that phi is x satisifying fixed point equation x = 1 + 1/x
;;
;;     Multiply both sides by x: x^2 = x + 1
;;
;;     Shape as a quadratic: x^2 - x - 1 = 0
;;
;;     Solve for positive root using quadratic formula: root = (1 + sqrt(5))/2
;;
;;     Since root is solution to x^2 - x - 1 = 0
;;     
;;     And x^2 - x - 1 = 0 is a transformation of x = 1 + 1/x by factor x
;;
;;     Then root satisifying the quadratic also satisfies x = 1 + 1/x
;;
;;     Since the equation for root denotes phi
;;
;;     Then phi satisfies x = 1 + 1/x


(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
;Value: 1.6180327868852458

