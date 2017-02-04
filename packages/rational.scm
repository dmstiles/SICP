

(define (install-rational-package)
  ;; internal procedures

  (define (pos? x) (> x 0))
  (define (gcd a b)
    (if (= b 0)
	a
	(gcd b (remainder a b))))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (if (pos? (/ n d))
	  (cons (/ n g) (/ d g))
	  (cons (* -1 (/ (abs n) g)) 
		(/ (abs d) g)))))
  (define (add x y)
    (make-rat (+ (* (numer x) (denom y))
		 (* (numer y) (denom x)))))
  (define (sub x y)
    (make-rat (- (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y))))
  (define (mul x y)
    (make-rat (* (numer x) (numer y))
	      (* (denom x) (denom y))))
  (define (div x y)
    (make-rat (* (numer x) (denom y))
	      (* (numer y) (denom x))))
  (define (equ? x y)
    (= (* (numer x) (denom y))
       (* (denom x) (numer y))))

  ;; interface
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div x y))))
  (put 'equ? '(rational rational)
       (lambda (x y) (equ? x y)))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'rational-package)

(install-rational-package)

(define (make-rational n d)
  ((get 'make 'rational) n d))
