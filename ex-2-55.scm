;; Exercise 2.55
;;
;; Description:
;;
;; Consider the following expression.
;;
;;    (car ''abracadabra)
;;
;; Why does the interpreter print `quote` upon evaluation?



;; Definitions:
(car ''abracadabra)
;Value: quote

;; Taken literally the `'` symbol evalautes to the procedure `quote` which tells the
;; interpreter to regard the subsequent characters as symbols and not values.
;; Something of the form 'a => (quote a), therefore the evaluation of ''abracadabra
;; is interpreted as (quote (quote abracadabra)), or similarily, '(quote abracadabra).
;; From this form it is evident that the interpreter sees a list of two symbols, quote
;; and abracadabra, where quote is the `car` of that list.
