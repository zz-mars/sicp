#! /usr/bin/guile -s
!#
(define (fac n)
 (if (= n 1) 
  1 
  (* n (fac (- n 1)))))

(define (fac_iter1 n fvalue) 
 (if (= n 1) 
  fvalue
  (fac_iter1 (- n 1) (* fvalue n))))

(define (fac1 n)
 (fac_iter1 n 1))

(define (fac_iter2 product counter n)
 (if (> counter n) product 
  (fac_iter2 (* product counter) (+ counter 1) n)))

(define (fac2 n)
 (fac_iter2 1 1 n))

(display (fac 6))
(display "\n")
(display (fac1 7))
(display "\n")
(display (fac2 7))
(display "\n")
