;; Exercise 2.69
;;
;; Description:
;;
;; Implement the `succesive-merge` procedure for the given `generate-huffman-tree`
;; procedure. The `succesive-merge` procedure merges the smallest-weight elements
;; until there is only one element left, which is the desired Huffman tree.


;; Definitions:

(define (generate-huffman-tree pairs)
  (succesive-merge (make-leaf-set pairs)))
;Value: generate-huffman-tree


(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair) ; symbol
			       (cadr pair)) ; frequency
		    (make-leaf-set (cdr pairs))))))
;Value: make-leaf-set


 (define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set)
		    (adjoin-set x (cdr set))))))
;Value: adjoin-set


(define (succesive-merge set)
  (if (null? (cdr set))
      (car set)
      (succesive-merge
       (adjoin-set
	(make-code-tree (car set) (cadr set))
	(cddr set)))))
;Value: succesive-merge


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
	((contains? s (symbols (left-branch tree)))
	 (cons 0 (encode-symbol s (left-branch tree))))
	(else (cons 1 (encode-symbol s (right-branch tree))))))
;Value: encode-symbol



;; Testing:


(define small-test-tree (generate-huffman-tree '((A 3) (B 5) (C 6) (D 6))))
;Value: small-test-tree

test-tree
;Value 20: (((leaf a 3) (leaf b 5) (a b) 8) ((leaf d 6) (leaf c 6) (d c) 12) (a b d c) 20)

;;         (20)
;;         /  \
;;        /    \
;;      (8)   (12)
;;      / \    / \
;;     A3 B5  D6 C6

(define small-encode (encode '(A B C D) small-test-tree))
;Value: small-encode

small-encode
;Value 25: (0 0 0 1 1 1 1 0)
;;          --| --| --| --|
;;            A   B   C   D


(decode small-encode small-test-tree)
;Value 26: (a b c d)


(define pairs '((A 8)
		(B 3)
		(C 1)
		(D 1)
		(E 1)
		(F 1)
		(G 1)
		(H 1)))

(define large-test-tree (generate-huffman-tree pairs))
;Value: large-test-tree

large-test-tree
;Value 21: ((leaf a 8) ((((leaf h 1) (leaf g 1) (h g) 2) ((leaf f 1) (leaf e 1) (f e) 2) (h g f e) 4) (((leaf d 1) (leaf c 1) (d c) 2) (leaf b 3) (d c b) 5) (h g f e d c b) 9) (a h g f e d c b) 17)

;;           (17)
;;           /  \
;;          A8   \
;;               (9)
;;               / \
;;              /   \
;;             /     \
;;           (4)      \
;;           / \       \ 
;;          /   \       \
;;         /     \       \
;;       (2)     (2)      \
;;       / \     / \       \
;;      H1 G1   F1 E1      (5)
;;                         / \
;;                        /   \
;;                      (2)    B3
;;                      / \
;;                     D1 C1

(define large-encode (encode '(B A G H D A D) large-test-tree))
;Value: large-encode

large-encode
;Value 22: (1 1 1 0 1 0 0 1 1 0 0 0 1 1 0 0 0 1 1 0 0)					
;;          ----| | ------| ------| ------| | ------|
;;              B A       G       H       D A       D

(decode large-encode large-test-tree)
;Value 24: (b a g h d a d)


