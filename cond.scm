#! /usr/bin/guile -s
!#

(define (zabs x)
 (cond ((< x 0) (- x))
       ((= x 0) 0)
	   (else x)))

(display (zabs -9))
(display "\n")
