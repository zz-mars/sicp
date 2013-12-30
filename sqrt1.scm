#lang racket
(define (sqrt x)
  (define (try guess)
    (define (goodenough guess)
      (define (square guess)
        (* guess guess))
      (define abs
        (lambda (x)
          (cond ((< x 0)(- x))
                ((= x 0) 0)
                ((> x 0) x))))
      (< (abs (- (square guess) x)) 0.0000001))
    (define (improve guess)
      (define average
        (lambda (a b)
          (/ (+ a b) 2.0)))
      (average guess (/ x guess)))
    (if (goodenough guess)
        guess
        (try (improve guess))))
  (try 1))

(sqrt 3)