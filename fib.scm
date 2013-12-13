#! /usr/bin/guile -s
!#

; add comment
(define (fib n)
 (cond ((= n 0) 0)
       ((= n 1) 1)
	   (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (fibb n)
 (cond ((or (= n 0) (= n 1)) n)
       (else (+ (fibb (- n 1)) (fibb (- n 2))))))

; fib index starts from 0
; find the element with index of 'n' 
(define (fibbb n)
 (if (or (= n 0) (= n 1))
  n 
  (fib_iter 0 1 1 n)))

; counter is the index of value 'b'
(define (fib_iter a b counter n)
 (if (= counter n) 
  b
  (fib_iter b (+ a b) (+ counter 1) n)))

(display (fib 10))
(display "\n")
(display (fibb 10))
(display "\n")
(display (fibbb 10))
(display "\n")
