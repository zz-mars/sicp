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

(display (make-from-real-img 2 3))
(newline)

