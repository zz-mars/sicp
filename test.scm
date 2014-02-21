(define abs
 (lambda (x) 
  (cond 
   ((or (> x 0) (= x 0)) x)
   ((< x 0) (- x)))))

(define abs 
 (lambda (x)
  (cond ((< x 0) (- x))
   (else x))))

(define (abs x)
 (if (< x 0)
  (- x)
  x))

(display (abs (- 3)))
(newline)

(define square 
 (lambda (x)
  (* x x)))

(display (square 9))
(newline)

(define (<= x y)
 (not (> x y)))

(define (>= x y)
 (not (< x y)))

(define (sum-of-square x y)
 (+ (square x) (square y)))

(define (exercise1_3 x y z)
 (cond 
  ((and (<= x y) (<= x z)) (sum-of-square y z))
  ((and (<= y x) (<= y z)) (sum-of-square x z))
  ((and (<= z y) (<= z x)) (sum-of-square x y))))

(display (exercise1_3 2 3 4))
(newline)

(define (a-plus-abs-b a b)
 ((if (< b 0) - +) a b))

(display (a-plus-abs-b 1 4))
(newline)
(display (a-plus-abs-b 1 (- 2)))
(newline)

(define (p) (p))

(define (test x y)
 (if (= x 0) 
  0
  y))

(define (sqrt-iter x guess)
 (define (good-enough? z)
   (< (abs (- x (square z))) 0.001))
 (if (good-enough? guess)
  guess
  (sqrt-iter x (average guess (/ x guess)))))

(define (sqrt-iter x guess)
 (define (next-guess z) (average z (/ x z)))
 (define (good-enough? z)
   (< (abs (- z (next-guess z))) 0.001))
 (if (good-enough? guess)
  guess
  (sqrt-iter x (next-guess guess))))

;(define (sqrt-iter x guess)
; (define (good-enough? z)
;   (< (abs (- x (square z))) 0.00000001))
; (define (new-if predicate then-clause else-clause)
;  (cond (predicate then-clause)
;		(else else-clause)))
;(new-if (good-enough? guess)
;  guess
;  (sqrt-iter x (average guess (/ x guess)))))

(define average
 (lambda (x y)
  (/ (+ x y) 2.0)))


(define (ssqrt x)
 (sqrt-iter x 1))

(display (ssqrt 5))
(newline)

(define tt1 12999999999999398776456789)
(define tt2 0.001)

(define t1 (ssqrt tt1))
(define t2 (ssqrt tt2))

(display tt1)
(newline)
(display t1)
(newline)
(display (square t1))
(newline)

(display tt2)
(newline)
(display t2)
(newline)
(display (square t2))
(newline)

