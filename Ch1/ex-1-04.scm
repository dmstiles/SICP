;; Exercise 1.4
;; Describe the behavior of the following procedure.

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
;Value: a-plus-abs-b


;; Testing.

(a-plus-abs-b 2 2)
;Value: 4

(a-plus-abs-b 6 -9)
;Value: 15


#|
The `a-plus-abs-b` procedure here returns an operator dependent upon
the sign of argument b. That is, if b is positive then an addition operator
is returned, else if b is negative a subtraction operator is returned.
This operator's value can be regarded as the set of machine instructions
required to carry out the corresponding combination of operands a and b.
|#