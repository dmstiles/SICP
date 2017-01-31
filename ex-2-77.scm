;; Exercise 2.77
;;
;; Description:
;;
;; Describe why the following expressions are required in order to use selectors
;; with complex numbers.
;;
;;    (put 'real-part '(complex) real-part)
;;    (put 'imag-part '(complex) imag-part)
;;    (put 'magnitude '(complex) magnitude)
;;    (put 'angle '(complex) angle)
;;
;; In particular how many times is `apply-generic` invoked and what procedure is
;; dispatched to?



;; Imports:

(load "operation-table.scm")
;    Loading "operation-table.scm"... done

(load "complex.scm")
;    Loading "complex.scm"...
;      Loading "rectangular.scm"... done
;      Loading "polar.scm"... done
;    ... done


;; Generic Interface:

(define (real-part z) (apply-generic 'real-part z))
;Value: real-part

(define (imag-part z) (apply-generic 'imag-part z))
;Value: imag-part

(define (magnitude z) (apply-generic 'magnitude z))
;Value: magnitude

(define (angle z) (apply-generic 'angle z))
;Value: angle

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
;Value: make-complex-from-real-imag


;; Solution:

(define z (make-complex-from-real-imag 3 4))
;Value: z

z
;Value 13: (complex rectangular 3 . 4)

(magnitude z)
;The object "No method for these types -- APPLY-GENERIC" is not applicable.

;; Associate the `magnitude` operation with the `complex` type.
(put 'magnitude '(complex) magnitude)
;Value: ok

(magnitude z)
;Value: 5

;; In order to make the procedure `magnitude` work with `complex` types, the two must
;; be associated (put into the `operation-table`). When this occurs the following
;; sequence causes the selector to retrieve the correct data.
;;
;; 1. `apply-generic` performs a lookup on the `magnitude` procedure for the `complex` type.
;;
;; 2. The `complex` tag is stripped when `apply-generic` applies `contents` to `z`.
;;
;; 3. The `complex` type implementation of `magnitude` is applied to `contents z`.
;;
;; 4. Since the `complex` type implementation of `magnitude` is the generic implementation
;;    of `magnitude`, `apply-generic` performs a second lookup on `magnitude` and `z`.
;;
;; 5. Since `z` has been stripeed of the `complex` tag, it is now type `rectangular`
;;    (because it was constructed from real and imaginary components), the `rectangular`
;;    tag is stripped by `contents z`.
;;
;; 6. The `rectangular` implementation of `magnitude` is applied to `z` by `apply-generic`.
;;
;; 7. The `rectangular` implementation of `magnitude` executes and returns 5.



