;; Exercise 2.68
;;
;; Description:
;;
;; Design the `encode-symbol` procedure in the given definition for `encode`. Use
;; the `encode` procedure to encode the message from exercise 2.67 to ensure the
;; the encoding is the same as the original encoded message.


;; Definitions:

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
;Value: make-leaf


(define (leaf? object)
  (eq? (car object) 'leaf))
;Value: leaf?


(define (symbol-leaf x) (cadr x))
;Value: symbol-leaf


(define (weight-leaf x) (caddr x))
;Value: weight-leaf


(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))
;Value: make-code-tree


(define (left-branch tree) (car tree))
;Value: left-branch


(define (right-branch tree) (cadr tree))
;Value: right-branch


(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
;Value: symbols


(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))
;Value: weight


(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else (error "bad bit -- CHOOSE-BRANCH" bit))))
;Value: choose-branch


(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
	    (append (cdr list1) list2))))
;Value: append


(define (contains? x list)
  (cond ((null? list) false)
	((eq? (car list) x) true)
	(else (contains? x (cdr list)))))
;Value: contains?


(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
	'()
	(let ((next-branch
	       (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
;Value: decode


(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))
;Value: encode


(define (encode-symbol s tree)
  (cond ((leaf? tree)
	 (if (eq? s (symbol-leaf tree))
	     '()
	     (error "unknown symbol -- ENCODE-SYMBOL" s)))
	((contains? s (left-branch tree))
	 (cons 0 (encode-symbol s (left-branch tree))))
	(else (cons 1 (encode-symbol s (right-branch tree))))))
;Value: encode-symbol



;; Testing:

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree (make-leaf 'D 1)
				   (make-leaf 'C 1)))))
;Value: sample-tree

;;     *
;;    / \
;;   A   *
;;      / \
;;     B   *
;;        / \
;;       D   C

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;Value: sample-message

(define decoded (decode sample-message sample-tree))
;Value: decoded

decoded
;Value 13: (a d a b b c a)



;; Solution:

(encode decoded sample-tree)
;Value 14: (0 1 1 0 0 1 0 1 0 1 1 1 0)

(encode '(a b d) sample-tree)
;Value 17: (0 1 0 1 1 0)

(encode '(a b c) sample-tree)
;Value 18: (0 1 0 1 1 1)

(encode '(a b c d) sample-tree)
;Value 19: (0 1 0 1 1 1 1 1 0)

(encode '(a b c z) sample-tree)
;unknown symbol -- ENCODE-SYMBOL z
