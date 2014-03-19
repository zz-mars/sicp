; mult-representation of complex number
(define (square z) (* z z))

(define nil '())
(define (attach-tag tag content)
 (cons tag content))

(define (type-tag datum)
 (if (pair? datum)
  (car datum)
  nil))

(define (contents datum)
 (if (pair? datum)
  (cdr datum)
  nil))

(define (rectangular? z)
 (eq? (type-tag z) 'rectangular))
(define (polar? z)
 (eq? (type-tag z) 'polar))

; rectangular package
(define (install-rectangular-package)
 (define (real-part z) (car z))
 (define (img-part z) (cdr z))
 (define (make-from-real-img r i)
  (cons r i))
 (define (magnitude z)
  (sqrt (+ (square (real-part z))
		 (square (img-part z)))))
 (define (angle z)
  (atan (img-part z) (real-part z)))
 (define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))
 (define (tag-it z)
  (cons 'rectangular z))
 (put 'real-part '(rectangular) real-part)
 (put 'img-part '(rectangular) img-part)
 (put 'magnitude '(rectangular) magnitude)
 (put 'angle '(rectangular) angle)
 (put 'make-from-real-img '(rectangular)
  (lambda (x y) (tag-it (make-from-real-img x y))))
 (put 'make-from-mag-ang '(rectangular)
  (lambda (r a) (tag-it (make-from-mag-ang r a))))
 'done)

; polar package
(define (install-polar-package)
 (define (magnitude z) (car z))
 (define (angle z) (cdr z))
 (define (make-from-mag-ang r a)
  (cons r a))
 (define (real-part z)
  (* (magnitude z) (cos (angle z))))
 (define (img-part z)
  (* (magnitude z) (sin (angle z))))
 (define (make-from-real-img x y)
  (cons (sqrt (+ (square x) (square y)))
   (atan y x)))
 (define (tag-it z)
  (cons 'polar z))
 (put 'real-part '(polar) real-part)
 (put 'img-part '(polar) img-part)
 (put 'magnitude '(polar) magnitude)
 (put 'angle '(polar) angle)
 (put 'make-from-real-img '(polar)
  (lambda (x y)
   (tag-it (make-from-real-img x y))))
 (put 'make-from-mag-ang '(polar)
  (lambda (r a)
   (tag-it (make-from-mag-ang r a))))
 'done)

(define (apply-generic op . args)
 (let ((type-tags (map type-tag args)))
  (let ((proc (get op type-tags)))
   (if proc
	(apply proc (map contents args))
	nil))))

(define (real-part z)
 (apply-generic 'real-part z))
(define (img-part z)
 (apply-generic 'img-part z))
(define (magnitude z)
 (apply-generic 'img-part z))
(define (angle z)
 (apply-generic 'angle z))

(define (make-from-real-img r i)
 ((get 'make-from-real-img '(rectangular)) r i))
(define (make-from-mag-ang r a)
 ((get 'make-from-mag-ang '(polar)) r a))


; message passing
(define (make-from-real-img x y)
 (lambda (op)
  (cond ((eq? op 'real-part) x)
   ((eq? op 'img-part) y)
   ((eq? op 'magnitude)
	(sqrt (+ (square x) (square y))))
   ((eq? op 'angle) 
	(atan y x))
   (else 
	(display "unrecognized operation")
	(newline)))))

(define (make-from-mag-ang r a)
 (lambda (op)
  (cond ((eq? op 'real-part)
		 (* r (cos a)))
   ((eq? op 'img-part)
	(* r (sin a)))
   ((eq? op 'magnitude) r)
   ((eq? op 'angle) a)
   (else 
	(display "unrecognized operation")
	(newline)))))

(define ri (make-from-real-img 2 3))
(define ma (make-from-mag-ang 2 0.7))

(define (apply-generic1 op arg)
 (arg op))

(define (real-part z)
 (apply-generic1 'real-part z))
(define (img-part z)
 (apply-generic1 'img-part z))
(define (magnitude z)
 (apply-generic1 'magnitude z))
(define (angle z)
 (apply-generic1 'angle z))

(display "------- make-from-real-img test ----------")
(newline)
(display (real-part ri))
(newline)
(display (img-part ri))
(newline)
(display (magnitude ri))
(newline)
(display (angle ri))
(newline)

(display "------- make-from-mag-ang test ----------")
(newline)
(display (real-part ma))
(newline)
(display (img-part ma))
(newline)
(display (magnitude ma))
(newline)
(display (angle ma))
(newline)

; generic operation
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
 (define (tag x)
  (cons 'scheme-number x))
 (put 'equ? '(scheme-number scheme-number)
  (lambda (x y) (= x y)))
 (put 'add '(scheme-number scheme-number)
  (lambda (x y)
   (tag (+ x y))))
 (put 'sub '(scheme-number scheme-number)
  (lambda (x y)
   (tag (- x y))))
 (put 'mul '(scheme-number scheme-number)
  (lambda (x y)
   (tag (* x y))))
 (put 'div '(scheme-number scheme-number)
  (lambda (x y)
   (tag (/ x y))))
 (put 'make 'scheme-number
  (lambda (x) (tag x)))
 'done)

(define (make-scheme-number n)
 ((get 'make 'scheme-number) n))

; rational package
(define (install-rational-package)
 (define (numer x) (car x))
 (define (denom x) (cdr x))
 (define (make-rat n d)
  (let ((gcd-nd (gcd n d)))
   (cons (/ n gcd-nd) (/ d gcd-nd))))
 (define (add-rat x y)
  (make-rat 
   (+ (* (denom x) (numer y))
	(* (numer x) (denom y)))
   (* (denom x) (denom y))))
 (define (sub-rat x y)
  (make-rat 
   (- (* (denom x) (numer y))
	(* (numer x) (denom y)))
   (* (denom x) (denom y))))
 (define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
   (* (denom x) (denom y))))
 (define (div-rat x y)
  (make-rat (* (numer x) (denom y))
   (* (numer y) (denom x))))
 (define (tag x)
  (cons 'rational x))
 (put 'equ? '(rational rational)
  (lambda (x y)
   (and (= (numer x) (numer y))
	(= (denom x) (denom y)))))
 (put 'add '(rational rational)
  (lambda (x y) (tag (add-rat x y))))
 (put 'sub '(rational rational)
  (lambda (x y) (tag (sub-rat x y))))
 (put 'mul '(rational rational)
  (lambda (x y) (tag (mul-rat x y))))
 (put 'div '(rational rational)
  (lambda (x y) (tag (div-rat x y))))
 (put 'make 'rational 
  (lambda (x y)
   (tag (make-rat x y))))
 'done)

(define (make-rational n d)
 ((get 'make 'rational) n d))

; complex number package
(define (install-complex-package)
 (define (make-from-real-img x y)
  ((get 'make-from-real-img 'rectangular) x y))
 (define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))
 ; internal procedure
 (define (add-complex z1 z2)
  (make-from-real-img 
   (+ (real-part z1) (real-part z2))
   (+ (img-part z1) (img-part z2))))
  (define (sub-complex z1 z2)
   (make-from-real-img
    (- (real-part z1) (real-part z2))
	(- (img-part z1) (img-part z2))))
  (define (mul-complex z1 z2)
   (make-from-mag-ang 
	(* (magnitude z1) (magnitude z2))
	(+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
   (make-from-mag-ang 
	(/ (magnitude z1) (magnitude z2))
	(- (angle z1) (angle z2))))
  ; interfaces
  (define (tag x)
   (cons 'complex x))
  (put 'equ? '(complex complex)
   (lambda (x y) 
	(and (= (real-part x) (real-part y))
	 (= (img-part x) (img-part y)))))
  (put 'add '(complex complex)
   (lambda (x y)
	(tag (add-complex x y))))
  (put 'sub '(complex complex)
   (lambda (x y)
	(tag (sub-complex x y))))
  (put 'mul '(complex complex)
   (lambda (x y) 
	(tag (mul-complex x y))))
  (put 'div '(complex complex)
   (lambda (x y)
	(tag (div-complex x y))))
  (put 'make-from-real-img 'complex
   (lambda (x y)
	(tag (make-from-real-img x y))))
  (put 'make-from-mag-ang 'complex
   (lambda (r a)
	(tag (make-from-mag-ang r a))))
  'done)
   
(define (make-from-real-img x y)
 ((get 'make-from-real-img 'complex) x y))
(define (make-from-mag-ang r a)
 ((get 'make-from-mag-ang 'complex) r a))
	
