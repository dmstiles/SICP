;; Exercise 1.41
;;
;; Description:
;;
;; A. Define a procedure `double` that takes a procedure of one argument
;;    as an argument and applies the original procedure twice.
;;
;; B. What is the value returned by the following statement?
;;
;;    (((double (double double))) inc 5)



;; Definitions:

(define (double f)
  (lambda (x) (f (f x))))
;Value: double


(define (inc x)
  (+ x 1))
;Value: inc


;; Testing;

((double inc) 1)
;Value: 3

(((double (double double)) inc) 5)
;Value: 21



;; Solutions:
;;
;; B. The nested calls to double produce an output that is incremened by 16.
;;    This is because each call to `double` in which `double` is passed as the
;;    argument increases the number of times the `inc` function will be applied by 2.
;;    The initial doubling of `double` results in 2 applications of `double`
;;    the subsequent third application of `double` results in 4 applications
;;    of `double`. Therefore when `inc` is applied it will be doubled 4 times
;;    resulting in 2^4 (2 -> 4 -> 8 -> 16) applications to its argument. 

