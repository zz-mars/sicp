(define (add-rat x y)
  (make-rat 
   (+ (* (numer x) (denom y)) (* (numer y) (denom x)))
   (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat
   (- (* (numer x) (denom y)) (* (numer y) (denom x)))
   (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat 
   (* (numer x) (numer y))
   (* (denom x) (denom y))))     

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat x y)
  (let ((g (gcd x y)))
  (cons (/ x g) (/ y g))))

(define (rat-sign x y)
  (if (or (and (< x 0) (> y 0)) (and (> x 0) (< y 0)))
             (- 1)
             1))

(define (new-make-rat x y)
  (let ((thesign (rat-sign x y))
        (p-rat (make-rat (abs x) (abs y))))
  (cons (* thesign (numer p-rat)) (denom p-rat))))

(define abs
  (lambda (x)
    (if (< x 0)
        (- x)
        x)))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (print-rat x)
  (display (numer x))
    (display "/")
    (display (denom x)))

(print-rat (new-make-rat (- 1) (- 3)))
(newline)
(print-rat (new-make-rat 1 (- 3)))
(newline)
(print-rat 
 (add-rat
  (new-make-rat 1 (- 3))
  (new-make-rat (- 1) (- 3))))
(newline)
