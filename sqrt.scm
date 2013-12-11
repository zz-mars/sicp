#! /usr/bin/guile -s 
!#

; my first scheme program
; calculate the square root of a given number

(define (square x) (* x x))

(define (averageof x y) (/ (+ x y) 2.0))

(define (zabs x)
 (if (< x 0) (- x) x))

(define (good_enough guess x)
 (< (zabs (- (square guess) x)) 0.001))

(define (improve guess x) (averageof guess (/ x guess)))

(define (sqrt_iter guess x)
 (if (good_enough guess x) guess 
  (sqrt_iter (improve guess x) x)))

(define (zsqrt x) 
 (if (< x 0) (- 1) (sqrt_iter 1.0 x)))

(display (zsqrt 8.9))
(display "\n")
