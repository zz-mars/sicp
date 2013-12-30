#! /usr/bin/guile -s 
!#

(define (try guess x)
 (if (goodenough? guess x)
	 guess
	 (try (improveguess guess x) x)))

(define improveguess
 (lambda (guess x) 
  (average guess (/ x guess))))

(define average 
 (lambda (x y)
  (/ (+ x y) 2.0)))

(define goodenough?
 (lambda (guess x)
  (< (abs (- (square guess) x)) 0.00001)))

(define square 
 (lambda (x) 
  (* x x)))

(define sqrtz
 (lambda (x) (try 1 x)))

(display (sqrtz 2))
(display "\n")

