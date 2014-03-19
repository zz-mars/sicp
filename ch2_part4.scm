(define (square z) (* z z))

(define nil '())
(define (add-tag tag content)
 (cons tag content))
(define (type-tag datum)
 (if (pair? datum)
  (car datum)
  nil))
(define (contents datum)
 (if (pair? datum)
  (cadr datum)
  nil))
(define (rectangular? z)
 (eq? (type-tag z) 'rectangular))
(define (polar? z)
 (eq? (type-tag z) 'polar))

(define (make-from-real-img r i)
 (list 'rectangular r i))
(define (make-from-polar a r)
 (list 'polar a r))

(define (rl-part z)
 (cond ((rectangular? z) (cadr z))
  ((polar? z) (* (cadr z) (cos (caddr z))))
  (else nil)))

(define (img-part z)
 (cond ((rectangular? z) (caddr z))
  ((polar? z) (* (cadr z) (sin (caddr z))))
  (else nil)))

(define (magnitude-part z)
 (cond ((rectangular? z) 
		(sqrt (+ (square (cadr z)) (square (caddr z)))))
  ((polar? z) (cadr z))
  (else nil)))

(define (angle-part z)
 (cond ((rectangular? z) 
		(atan (caddr z) (cadr z)))
  ((polar? z) (caddr z))
  (else nil)))

(define (add-complex z1 z2)
 (make-from-real-img
  (+ (rl-part z1) (rl-part z2))
  (+ (img-part z1) (img-part z2))))
(define (sub-complex z1 z2)
 (make-from-real-img
  (- (rl-part z1) (rl-part z2))
  (- (img-part z1) (img-part z2))))

(define (mul-complex z1 z2)
 (make-from-polar 
  (* (magnitude-part z1) (magnitude-part z2))
  (+ (angle-part z1) (angle-part z2))))

(define (divide z1 z2)
 (make-from-polar
  (/ (magnitude-part z1) (magnitude-part z2))
  (- (angle-part z1) (angle-part z2))))

(define z1 (make-from-real-img 1 2))
(define z2 (make-from-real-img 1 (- 2)))

(display (add-complex z1 z2))
(newline)
(display (sub-complex z1 z2))
(newline)
(display (mul-complex z1 z2))
(newline)
(display (divide z1 z2))
(newline)

; polar
(define (real-part-polar z)
 (* (car z) (cos (cdr z))))
(define (img-part-polar z)
 (* (car z) (sin (cdr z))))
(define (magnitude-polar z)
 (car z))
(define (angle-polar z)
 (cdr z))
(define (make-complex-polar A R)
 (add-tag 'polar (cons A R)))
; real-img

; data directed programming
(define (install-rectangluar-package)
  (define (real-part z) (car z))
  (define (img-part z) (cdr z))
  (define (magnitude z)
	(sqrt (+ (square (real-part z))
			 (square (img-part z)))))
  (define (angle z)
	(atan (/ (img-part z)
			 (real-part z))))
  (define (make-from-ri r i) (cons r i))
  (define (make-from-ar a r)
   (cons (* a (cos r)) (* a (sin r))))
  ;; interface to the rest of the system
  (define (tag-it x) (add-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'img-part '(rectangular) img-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-ri '(rectangular) 
   (lambda (r i) (tag-it (make-from-ri r i))))
  (put 'make-from-ar '(rectangular) 
   (lambda (a r) (tag-it (make-from-ar a r))))
  'done)

(define (install-polar-package)
 (define (magnitude z) (car z))
 (define (angle z) (cdr z))
 (define (make-from-ar a r) (cons a r))
 (define (real-part z)
  (* (magnitude z) (cos (angle z))))
 (define (img-part z)
  (* (magnitude z) (sin (angle z))))
 (define (make-from-ri x y)
  (cons (sqrt (+ (square x) (square y)))
   (atan y x)))
 ;; extern interface
  (define (tag-it x) (add-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'img-part '(polar) img-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-ri '(polar) 
   (lambda (r i) (tag-it (make-from-ri r i))))
  (put 'make-from-ar '(polar) 
   (lambda (a r) (tag-it (make-from-ar a r))))
  'done)

(define (apply-generic op . args)
 (let ((type-tags (map type-tag args)))
  (let ((proc (get op type-tags)))
   (if proc
	(apply proc (map contents args))
	(display "error")))))
; generic 
(define (real-part z) (apply-generic 'real-part z))
(define (img-part z) (apply-generic 'img-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-img x y)
 ((get 'make-from-ri 'rectangular) x y))
(define (make-from-polar a r)
 ((get 'make-from-ar 'polar) a r))

; exercise 2.73
