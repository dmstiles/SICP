;; Exercise 2.71
;;
;; Description:
;;
;; Consider the Huffman tree for an alphabet of n symbols, with relative frequencies
;; 1,2,4,...,2^(n-1). Sketch the trees for n=5 and n=10. In general how many bits are
;; required to encode the most frequent and least frequent symbols?



;; Testing:

(load "ex-2-69.scm")

;Loading "ex-2-69.scm"... done

(define alphabet '((A 1)
		   (B 2)
		   (C 4)
		   (D 8)
		   (E 16)))
;Value: alphabet

alphabet
;Value 16: ((a 1) (b 2) (c 4) (d 8) (e 16))


(define small-tree (generate-huffman-tree alphabet))
;Value: small-tree

(encode '(E) small-tree)
;Value 19: (1)

(encode '(A) small-tree)
;Value 20: (0 0 0 0)


(define alphabet-full '((A 1)
			(B 2)
			(C 4)
			(D 8)
			(E 16)
			(F 32)
			(G 64)
			(H 128)
			(I 256)
			(J 512)))
;Value: alphabet-full

(define large-tree (generate-huffman-tree alphabet-full))
;Value: large-tree

(encode '(J) large-tree)
;Value 21: (1)

(encode '(A) large-tree)
;Value 22: (0 0 0 0 0 0 0 0 0)

;; Solution:
;;
;; n = 5
;; S = { (A,1), (B,2), (C,4), (D,8), (E,16) }
;;
;;    *
;;   / \
;;  E   *
;;     / \
;;    D   *
;;       / \
;;      C   *
;;         / \
;;        B   A
;;
;; E = 1
;; A = 0000
;;
;; 
;; n = 10
;; S = { (A,1), (B,2), (C,4), (D,8), (E,16), (F,32), (G,64), (H,128), (I,256), (J,512) }
;;
;;    *
;;   / \
;;  J   *
;;     / \
;;    I   *
;;       / \
;;      H   *
;;         / \
;;        G   *
;;           / \
;;          F   *
;;             / \
;;            E   *
;;               / \
;;              D   *
;;                 / \
;;                C   *
;;                   / \
;;                  B   A
;;
;; J = 1
;; A = 000000000
;;                        

;; As can be seen above, in general a tree with n symbols and relative frequencies 1,2,...,2^(n-1)
;; will require 1 bit to encode the most frequent symbol and n-1 bits encode the least frequent symbol.
