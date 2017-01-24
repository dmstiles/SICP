;; Exercise 2.70
;;
;; Description:
;;
;; Use the `generate-huffman-tree` and `encode` procedures to encode the given message
;; with the the following eight symbol alphabet.
;;
;;    Alphabet:
;;
;;    A 2     NA 16   BOOM 1   SHA 3
;;
;;    GET 2   YIP 9   JOB 2    WAH 1
;;
;;    Message:
;;
;;    Get a job
;;    Sha na na na na na na na na
;;    Get a job
;;    Sha na na na na na na na na
;;    Wah yip yip yip yip yip yip yip yip yip
;;    Sha boom
;;
;; How many bits are required for the encoding?
;;
;; What is the smallest number of bits needed to encode the song if a fixed-length
;; code was used for the eight-symbol alphabet?


;; Definitions:

(load "ex-2-69.scm")
;  Loading "ex-2-69.scm"... done


(define alphabet '((A 2)
		   (BOOM 1)
		   (GET 2)
		   (JOB 2)
		   (NA 16)
		   (SHA 3)
		   (YIP 9)
		   (WAH 1)))
;Value: alphabet

(define message '(GET A JOB
		      SHA NA NA NA NA NA NA NA NA
		      GET A JOB
		      SHA NA NA NA NA NA NA NA NA
		      WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
		      SHA BOOM))
;Value: message

(length message)
;Value: 36

		      
(define huff-tree (generate-huffman-tree alphabet))
;Value: huff-tree

huff-tree
;Value 14: ((leaf na 16) ((leaf yip 9) (((leaf a 2) ((leaf wah 1) (leaf boom 1) (wah boom) 2) (a wah boom) 4) ((leaf sha 3) ((leaf job 2) (leaf get 2) (job get) 4) (sha job get) 7) (a wah boom sha job get) 11) (yip a wah boom sha job get) 20) (na yip a wah boom sha job get) 36)

(define encoded (encode message huff-tree))
;Value: encoded

encoded
;Value 16: (1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)


;; Solution:

;; The encoding requires:

(length encoded)
;Value: 84

;; bits to transmit the message. Considerinig the fixed length transmission, since there are
;; 8 symbols in the alphabet, and the number of bits required to represent an alphabet
;; is given by log(n) where n is the number of symbols. This alphabet would require
;; 3 bits to represent each symbol, therefore a message of 36 symbols would require:

(* 3 36)
;Value: 108

;; bits to transmit. In compressing the message with a Huffman tree ordered by
;; relative frequencies, one is able to achieve a:

(- 1.0 (/ 84.0 108.0))
;Value: .2222222222222222

;; reduction in the size of the transmitted message.

