;; Exercise 2.72
;;
;; Description:
;;
;; Consider the encoding procedure below. What is the order of growth in number of steps needed
;; to encode a symbol where the symbol frequencies are given by the pattern 1,2,...,2^(n-1)?


;; Definitions:

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
	((contains? s (symbols left-branch tree))
	 (cons 0 (encode-symbol s (left-branch tree))))
	(else (cons 1 (encode-symbol s (right-branch ))))))
;Value: encode-symbol


;; Solution:
;;
;; Encoding the least frequent symbol is most expensive, since it requires an O(n^2) uppper bound.
;; This is becuase, in reaching the leaf of the Huffman tree, the procedure must perform a search on
;; the current symbols remaining in the tree. The formula for precisely how may search are performed
;; arises from the sum of all searches, where the first search takes a maximum (n) operations. The
;; subsequent search takes (n-1) operations, and so on down to 1 search operation where the element.
;; is found in its leaf node. This summation reduces to (n * (n + 1))/2, and gives an O(n^2) growth
;; in number of steps.
;;
;; Conversely, the most frequent character will be found in the first search operation which requires,
;; worst case, (n) steps, therefore the order of growth in encoding the most frequent character is
;; simply O(n).
