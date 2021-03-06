; chapter two : data abstraction
(define (>= a b) (not (< a b)))
(define (<= a b) (not (> a b)))
(define (average x y) (/ (+ a b) 2.0))

(define (gcd a b)
 (if (= b 0) a (gcd b (remainder a b))))

;(define (cons x y)
; (lambda (nr)
;  (cond ((= nr 0) x)
;   ((= nr 1) y)
;   (else (display "invalid dispatch argument!")
;	(newline)))))
;
;(define (car x) (x 0))
;(define (cdr x) (x 1))
;
;; exercise 2.4
;(define (cons x y)
; (lambda (m) (m x y)))
;(define (car z)
; (z (lambda (x y) x)))
;(define (cdr z)
; (z (lambda (x y) y)))

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
(define (segments-orthogonal? seg1 seg2)
 (let ((s1 (start-segment seg1))
	   (e1 (end-segment seg1))
	   (s2 (start-segment seg2))
	   (e2 (end-segment seg2)))
  (= (+ (* (- (x-point e1) (x-point s1)) (- (x-point e2) (x-point s2)))
	  (* (- (y-point e1) (y-point s1)) (- (y-point e2) (y-point s2)))) 0)))

(define A (make-point 1 4))
(define B (make-point 2 1))
(define C (make-point 5 2))
(define D (make-point 1 1))

(define seg1 (make-segment B A))
(define seg2 (make-segment B C))
(define seg3 (make-segment D A))
(define seg4 (make-segment D C))

; test segments-orthogonal? 
(if (segments-orthogonal? seg1 seg2)
 (display "1 2 orthogonal")
 (display "1 2 not orthogonal"))
(newline)
(if (segments-orthogonal? seg3 seg4)
 (display "3 4 orthogonal")
 (display "3 4 not orthogonal"))
(newline)

(define (mid-point-segment seg)
 (make-point 
  (average (x-point (start-segment seg)) (x-point (end-segment seg)))
  (average (y-point (start-segment seg)) (y-point (end-segment seg)))))

; exercise 2.3
; Assume seg1 and seg2 are orthogonal
(define (make-rectangle seg1 seg2) 
 (cond ((segments-orthogonal? seg1 seg2) (cons seg1 seg2))
  (else 
   (display "segment not orthogonal!")
   (newline))))

(define (rec-perimeter rec)
 (let ((seg1 (car rec))
	   (seg2 (cdr rec)))
  (* 2 (+ (segment-len seg1) (segment-len seg2)))))

(define (rec-area rec)
 (let ((seg1 (car rec))
	   (seg2 (cdr rec)))
  (* (segment-len seg1) (segment-len seg2))))

(define rec1 (make-rectangle seg1 seg2))

(display (rec-perimeter rec1))
(newline)
(display (rec-area rec1))
(newline)

(define (even? x) (= (remainder x 2) 0))
(define (my-exp x n)
 (define (iter base i r)
  (cond ((= i 0) r)
   ((even? i) (iter (square base) (/ i 2) r))
   (else (iter base (- i 1) (* r base)))))
 (iter x n 1))
;(display (my-exp 2 13))
;(newline)

; exercise 2.5
;(define (cons a b)
; (let ((repr (* (my-exp 2 a) (my-exp 3 b))))
;  (lambda (f) (f repr))))
;
;(define (cad z k)
; (z (lambda (x)
;	 (define (iter y r)
;	  (if (= (remainder y k) 0)
;	   (iter (/ y k) (+ r 1))
;	   r))
;	 (iter x 0))))
;
;(define (car z) (cad z 2))
;(define (cdr z) (cad z 3))
;
;(define z (cons 4 3))
;(display (car z))
;(newline)
;(display (cdr z))
;(newline)
	
; exercise 2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
 (lambda (f) (lambda (x) (f ((n f) x)))))

; (add-1 zero) 
(define one
 (lambda (f) (lambda (x) (f x))))
; (add-1 one)
(define two
 (lambda (f) (lambda (x) (f (f x)))))
; (add-1 two)
(define three
 (lambda (f) (lambda (x) (f (f (f x))))))

(define (+ a b) 
 (lambda (f) (lambda (x) ((b f) ((a f) x)))))

; sequences
(define l (list 1 2 3 4 5))
(display (car l))
(newline)
(display (cdr l))
(newline)

(define (list-ref items n)
 (if (= n 0)
  (car items)
  (list-ref (cdr items) (- n 1))))

(display (list-ref l 3))
(newline)

(define nil (cdr (list 1)))

(define (list-reverse lst)
 (define (iter origin res)
  (if (null? origin)
   res
   (iter (cdr origin) (cons (car origin) res))))
 (iter lst nil))

(display (list-reverse l))
(newline)

(define (list-map lst f)
 (let ((nil (cdr (list 1))))
  (define (iter origin res)
   (if (null? origin) res
	(iter (cdr origin) (cons (f (car origin)) res))))
  (list-reverse (iter lst nil))))

(define l2 (list-map l (lambda (x) (square x))))

(define (append list1 list2)
 (if (null? list1)
  list2
  (cons (car list1) (append (cdr list1) list2))))

(display (append l l2))
(newline)

; exercise 2.17
(define (last-pair lst)
 (if (null? (cdr lst))
  lst
  (last-pair (cdr lst))))

(display (last-pair l2))
(newline)

; exercise 2.20
; an implementation of filter
;(define (list-filter lst f)
; (define (iter origin-lst res-lst)
;  (cond ((null? origin-lst) res-lst)
;   ((f (car origin-lst))
;	(iter (cdr origin-lst) (cons (car origin-lst) res-lst)))
;   (else (iter (cdr origin-lst) res-lst))))
; (list-reverse (iter lst nil)))

; another filter implementation
(define (list-filter lst f)
 (define (iter origin-lst res-lst)
  (if (null? origin-lst) res-lst
   (let ((to-test (car origin-lst))
		 (lst-left (cdr origin-lst)))
	(if (f to-test) (iter lst-left (cons to-test res-lst))
	 (iter lst-left res-lst)))))
 (list-reverse (iter lst nil)))

(define l (list 1 2 3 4 5 6 7 8 9))
(display "==============filter test=============")
(newline)
(display (list-filter l (lambda (x) (= (remainder x 2) 0))))
(newline)

(define (same-parity first-arg . l)
 (let ((first-remainder (remainder first-arg 2)))
  (cons first-arg 
   (list-filter l 
	(lambda (x) (= first-remainder (remainder x 2)))))))

(display "============filter============")
(newline)
(display (same-parity 1 2 3 4 5 6 7 8 9 ))
(newline)

; exercise 2.21
; 1st implementation of square-list
(define (square-list lst)
 (if (null? lst)
  nil
  (cons (square (car lst)) (square-list (cdr lst)))))

(display (square-list l))
(newline)

(define (square-list lst) (map square lst))
(display (square-list l))
(newline)

; exercise 2.22 pass
; exercise 2.23
(define (list-for-each f lst)
 (cond ((not (null? lst))
		(f (car lst))
		(list-for-each f (cdr lst)))))
(list-for-each (lambda (x) (newline) (display x)) (list 57 321 88))
(newline)

