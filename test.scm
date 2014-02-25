; chapter two : data abstraction
(define (>= a b) (not (< a b)))
(define (<= a b) (not (> a b)))
(define (average x y) (/ (+ a b) 2.0))

(define (gcd a b)
 (if (= b 0) a (gcd b (remainder a b))))

;(display (gcd 12 16))
;(newline)

;(define (make-rat n d)
; (cond ((= d 0) 
;		(display "denominator cannot be zero!")
;		(newline)
;		(cons 1 0))
;  (else (let ((the-gcd (gcd n d)))
;		 (cons (/ n the-gcd) (/ d the-gcd))))))

; better version of make-rat
(define (make-rat n d)
 (cond ((= d 0) 
		(display "denominator cannot be zero!")
		(newline)
		(cons 1 0))
  (else (let ((the-gcd (gcd n d)))
		 (let ((a (/ n the-gcd))
			   (b (/ d the-gcd)))
		  (cond ((< b 0) (cons (- a) (- b)))
		   (else (cons a b))))))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
 (display (numer x))
 (display "/")
 (display (denom x))
 (newline))

;(print-rat (make-rat 12 16))

;(let ((x (make-rat 12 16)))
; (display (numer x))
; (newline)
; (display (denom x))
; (newline))

(define (add-rat a b)
 (let ((na (numer a))
	   (da (denom a))
	   (nb (numer b))
	   (db (denom b)))
  (make-rat 
   (+ (* na db) (* nb da))
   (* da db))))

(print-rat (add-rat (make-rat 1 3) (make-rat 1 3)))

(define (sub-rat a b)
 (add-rat a (make-rat (numer b) (- (denom b)))))

(print-rat (sub-rat (make-rat 1 3) (make-rat 1 3)))
(print-rat (sub-rat (make-rat 1 2) (make-rat 1 3)))

(define (mul-rat a b)
 (make-rat (* (numer a) (numer b)) (* (denom a) (denom b))))

(print-rat (mul-rat (make-rat 4 6) (make-rat 1 2)))

(define (div-rat a b)
 (make-rat (* (numer a) (denom b)) (* (numer b) (denom a))))

(define (equal-rat? a b)
 (= (* (numer a) (denom b)) (* (numer b) (denom a))))

; reciprocal of x
(define (rec-rat x)
 (make-rat (denom x) (numer x)))

(print-rat (rec-rat (make-rat 0 1)))

(print-rat (rec-rat (make-rat 2 (- 6))))

(define (square x) (* x x))

;exercise 2.2
(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (segment-len seg)
 (sqrt 
  (+ (square (- (x-point (start-segment seg)) (x-point (end-segment seg))))
   (square (- (y-point (start-segment seg)) (y-point (end-segment seg)))))))

; check if two segments are orthogonal
; (define (segments-orthogonal? seg1 seg2))

(define (mid-point-segment seg)
 (make-point 
  (average (x-point (start-segment seg)) (x-point (end-segment seg)))
  (average (y-point (start-segment seg)) (y-point (end-segment seg)))))

; exercise 2.3
; Assume seg1 and seg2 are orthogonal
(define (make-rectangle seg1 seg2) (cons seg1 seg2))
(define (rec-perimeter rec)
 (let ((seg1 (car rec))
	   (seg2 (cdr rec)))
  (* 2 (+ (segment-len seg1) (segment-len seg2)))))

(define (rec-area rec)
 (let ((seg1 (car rec))
	   (seg2 (cdr rec)))
  (* (segment-len seg1) (segment-len seg2))))

