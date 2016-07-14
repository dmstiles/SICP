;; Exercise 2.17
;;
;; Description:
;;
;; Define a procedure `last-pair` that returns the list that contains only
;; the last element of a given nonempty list as follows.
;;
;; (last-pair (list 23 72 149 34))
;; (34)



;; Definitions:

(define (last-pair items)
  (define (iter last-item current-items)
    (if (null? current-items)
      (list last-item)
      (iter (car current-items) (cdr current-items))))
  (iter '() items))
;Value: last-pair


;; Testing:

(last-pair (list 23 72 149 34))
;; (34)




      