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

(define average
 (lambda (x y)
  (/ (+ x y) 2.0)))

(define (sqrt-iter x guess)
 (define (good-enough? z)
   (< (abs (- x (square z))) 0.001))
 (if (good-enough? guess)
  guess
  (sqrt-iter x (average guess (/ x guess)))))

(define (ssqrt x)
 (sqrt-iter x 1))

(define tt1 1299999999398776456789)
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
(display "----------------------")
(newline)

; exercise 1.7
(define (sqrt-iter x guess)
 (define (next-guess z)
  (average z (/ x z)))
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


(define (ssqrt x)
 (sqrt-iter x 1))

(define tt1 1299999999398776456789)
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

(define (cube x)
 (* x x x))

(define (cube-root x)
 (cube-root-iter x 1.0))

(define (cube-root-iter x guess)
; (define (improve-cube-root-guess z)
;   (average z (/ (+ (/ x (square z)) (* 2 z)) 3)))
 (define (improve-cube-root-guess z)
   (/ (+ (/ x (square z)) (* 2 z)) 3))
 (define (cube-root-good-enough? z)
  (< (abs (- x (cube z))) 0.001))
 (if (cube-root-good-enough? guess)
  guess
  (cube-root-iter x (improve-cube-root-guess guess))))

(define t3 87)

(display t3)
(newline)
(define tt3 (cube-root t3))
(display tt3)
(newline)
(display (cube tt3))
(newline)

; factorial with continuation passing style
(define (fact n)
 (define (fact-iter n f)
  (if (= n 1)
   (f 1)
   (fact-iter (- n 1) (lambda (x) (f (* n x))))))
 (fact-iter n (lambda (x) (display x))))

(fact 6)
(newline)

; exercise 1.10
(define (A x y)
 (cond ((= y 0) 0)
  ((= x 0) (* 2 y))
  ((= y 1) 2)
  (else (A (- x 1) (A x (- y 1))))))

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))

(display "-----------")
(newline)
(display (f 1))
(newline)
(display (f 2))
(newline)
(display (f 3))
(newline)
(display (f 4))
(newline)

(display "-----------")
(newline)
(display (g 1))
(newline)
(display (g 2))
(newline)
(display (g 3))
(newline)
(display (g 4))
(newline)

(display "-----------")
(newline)
(display (h 1))
(newline)
(display (h 2))
(newline)
(display (h 3))
(newline)
(display (h 4))
(newline)

; fib with continuation passing style
(define (fib n)
  (define (fib-iter i a b f)
	(cond ((= i n) (f a))
		  (else (fib-iter (+ i 1) b (+ a b) f))))
  (fib-iter 1 0 1 (lambda (x) (display x))))

;(display (fib 10))
(fib 10)
(newline)

(define (count-change amount)
 (cc amount 5))

(define (cc amount kinds-of-coin)
 (cond 
  ((= amount 0) 1)
  ((or (< amount 0) (= kinds-of-coin 0)) 0)
  (else (+ (cc amount (- kinds-of-coin 1))
		 (cc (- amount (first-denominator kinds-of-coin)) kinds-of-coin)))))
(define (first-denominator kinds-of-coin)
 (cond 
  ((= kinds-of-coin 1) 1)
  ((= kinds-of-coin 2) 5)
  ((= kinds-of-coin 3) 10)
  ((= kinds-of-coin 4) 25)
  ((= kinds-of-coin 5) 50)))

(display (count-change 100))
(newline)

; exercise 1.11
(define (recursf n)
 (cond 
  ((< n 0) (display "invalid argument"))
  ((< n 3) n)
  (else (+ (recursf (- n 1)) (* 2 (recursf (- n 2))) (* 3 (recursf (- n 3)))))))

(display (recursf 10))
(newline)

(define (iterf n)
 (define (iterf-iter i a b c)
  (cond ((= i n) c)
		(else (iterf-iter (+ i 1) b c (+ c (* 2 b) (* 3 a))))))
 (cond 
  ((< n 0) (display "invalid argument"))
  ((< n 3) n)
  (else (iterf-iter 2 0 1 2))))

(display (iterf 10))
(newline)

; exercise 1.12 
(define (pascal-triangle i j)
 (cond ((or (< i 0) (< j 0) (< i j)) 0)
  ((and (= i 1) (= j 1)) 1)
  (else (+ (pascal-triangle (- i 1) (- j 1)) (pascal-triangle (- i 1) j)))))

(display (pascal-triangle 5 3))
(newline)

(define (even? x)
 (= (remainder x 2) 0))

(define (fast-exp x n)
 (cond ((= n 0) 1)
  ((even? n) (fast-exp (square x) (/ n 2)))
  (else (* x (fast-exp x (- n 1))))))

(define (fast-exp x n)
 (define (fast-exp-iter x n r)
  (cond ((= n 0) r)
		((even? n) (fast-exp-iter (square x) (/ n 2) r))
		(else (fast-exp-iter x (- n 1) (* x r)))))
 (fast-exp-iter x n 1))

(display (fast-exp 2 9))
(newline)

; exercise 1.18
(define (mul a b)
 (define (double x) (* x 2))
 (define (halve x) (/ x 2))
 (define (mul-iter a b r)
  (cond ((= b 0) r)
   ((even? b) (mul-iter (double a) (halve b) r))
   (else (mul-iter a (- b 1) (+ r a)))))
 (mul-iter a b 0))

(display (mul 45 8))
(newline)

; exercise 1.19
;(define (mktrans p q) (cons p q))
;(define (trans-p t) (car t))
;(define (trans-q t) (cdr t))
;
;(define (fib n)
; (define (trans-exp t n)	
;  (define p (trans-p t))
;  (define q (trans-q t))
;  (cond ((= n 0) t)
;   ((even? n) (trans-exp (mktrans (+ (square p) (square q)) (+ (square q) (* 2 p q))) (/ n 2)))
;   (else (trans-exp
; (define (fib-iter n p q)
;  (define single-step-trans-p 0)
;  (define single-step-trans-q 1)

(define (fib n)
 ; initial state : (a,b)
 ; initial transformation : (p,q)
 ; do the transformation times : count
 ;
 ; When count is even, we can iter with a new transformation 
 ; which is square of the original one and halve the count
 ; When count is odd, do the transformation for once, 
 ; EAT THE 'SINGLE' STEP TRANSFORMATION!
 (define (fib-iter a b p q count)
  (cond ((= count 0) b)
   ((even? count)
	(fib-iter a b
	 (+ (square p) (square q))
	 (+ (square q) (* 2 p q))
	 (/ count 2)))
   (else (fib-iter (+ (* q (+ a b)) (* a p))
		  (+ (* b p) (* a q))
		  p q (- count 1)))))
 (fib-iter 1 0 0 1 n))

(display (fib 10))
(newline)

(define (zgcd a b)
 (if (= b 0) 
  a
  (zgcd b (remainder a b))))

; exercise 1.21
(define (smallest-divisor x)
 (define (divided? i)
  (= (remainder x i) 0))
 (define (find-smallest-divisor i)
  (cond ((> (square i) x) x)
   ((divided? i) i)
   (else (find-smallest-divisor (+ i 1)))))
 (find-smallest-divisor 2))

(display (smallest-divisor 199))
(newline)
(display (smallest-divisor 1999))
(newline)
(display (smallest-divisor 19999))
(newline)

; runtime error?
;(define (timed-prime-test n)
; (newline)
; (display n)
; (start-prime-test n (runtime)))
;
;(define (start-prime-test n start-time)
; (define (report-prime elapsed-time)
;  (display " *** ")
;  (display elapsed-time)
;  (newline))
; (if (prime? n)
;  (report-prime (- (runtime) start-time))
;  (timed-prime-test (+ n 1))))
;
;(timed-prime-test 1000)
;(timed-prime-test 10000)
;(timed-prime-test 100000)
;(timed-prime-test 1000000)

;exercise 1.23
(define (smallest-divisor n)
 (define (next i) (if (= i 2) 3 (+ i 2)))
 (define (divided? i)
  (= (remainder n i) 0))
 (define (find-divisor i)
  (cond ((> (square i) n) n)
   ((divided? i) i)
   (else (find-divisor (next i)))))
 (find-divisor 2))

(define (prime? x)
 (= (smallest-divisor x) x))

(display (smallest-divisor 199))
(newline)
(display (smallest-divisor 1999))
(newline)
(display (smallest-divisor 19999))
(newline)

(define (sigma from to next-func term-func)
 (define (iter from r)
  (if (> from to)
  r
  (iter (next-func from) (+ r (term-func from)))))
 (iter from 0))

(define (next-func1 x) (+ x 1))
(define (next-func2 x) (+ x 4))
(define (term1 x) x)
(define (term2 x) (* x x x))
(define (term3 x) (/ 1.0 (* x (+ x 2))))

(display (sigma 1 100 next-func1 term1))
(newline)
(display (sigma 1 100 next-func1 term2))
(newline)
(display (* 8 (sigma 1 10000 next-func2 term3)))
(newline)

(define (definite-integral f a b dx)
 (define (next x) (+ x dx))
 (* dx (sigma (+ a (/ dx 2.0)) b next f)))

(display (definite-integral cube 0 1 0.01))
(newline)
(display (definite-integral cube 0 1 0.001))
(newline)

;exercise 1.29
; integral with simon's rule
(define (integral-with-simon-rule f a b n)
 (define (even-odd k)
  (if (= (remainder k 2) 0) 0 1))
 (define (inc x) (+ x 1))
 (define (new-func k)
  (* (+ 2 (* 2 (even-odd k))) (f (+ a (* k (/ (- b a) n))))))
 (* (/ (- b a) (* 3 n)) (- (sigma 0 n inc new-func) (+ (f a) (f b)))))

(display (integral-with-simon-rule cube 0 1.0 100))
(newline)
(display (integral-with-simon-rule cube 0 1.0 1000))
(newline)

; exercise 1.31
(define (mul-sigma f from next to)
 (define (iter a r)
  (if (> a to)
   r
   (iter (next a) (* r (f a)))))
 (iter from 1.0))

; recursive version of mul-sigma
; This leads to stack overflow
; LOL
;(define (mul-sigma f from next to)
; (if (> from to)
;  1
;  (* (f from) (mul-sigma f (next from) next to))))

(define (test-func k)
 (/ (* (- k 1) (+ k 1)) (square k)))
(define (next k) (+ k 2))

(display (* 4 (mul-sigma test-func 3 next 10000)))
(newline)

; exercise 1.32 : more general accumulate
(define (accumulator combiner base-value term-func from next to)
 (define (iter k r)
  (if (> k to)
   r
   (iter (next k) (combiner (term-func k) r))))
 (iter from base-value))

(define (add-sigma f a next b)
 (accumulator + 0.0 f a next b))
(define (mul-sigma f a next b)
 (accumulator * 1.0 f a next b))

; accumulator as return value
(define (accumulator combiner base-value)
 (lambda (term-func from next to) 
  (define (iter k r)
   (if (> k to)
	r
	(iter (next k) (combiner (term-func k) r))))
  (iter from base-value)))

(define add-sigma (accumulator + 0.0))
(define mul-sigma (accumulator * 1.0))

(display "---------------------")
(newline)

(define (inc x) (+ x 1))

(display (add-sigma square 3 inc 1000))
(newline)

(display (* 4 (mul-sigma test-func 3 next 10000)))
(newline)

(define (filtered-accumulaor-generator combiner base-value)
 (lambda (term-func from next to the-filter)
  (define (iter k r)
   (cond ((> k to) r)
	((the-filter k) (iter (next k) (combiner r (term-func k))))
	(else (iter (next k) r))))
  (iter from base-value)))

(define filter-add-sigma (filtered-accumulaor-generator + 0))
(define filter-mul-sigma (filtered-accumulaor-generator * 1))

(display "sum of square of the prime numbers between 3 to 1000")
(newline)
(display (filter-add-sigma square 3 inc 1000 prime?))
(newline)

(display ((lambda (x) (+ x 3)) 4))
(newline)

; using let
(define (f x y)
 (let ((a (+ 1 (* x y)))
	   (b (- 1 y)))
  (+ (* x (square a)) (* y b) (* a b))))

(display ((lambda (x) (+ (let ((x 3)) (+ x (* x 10))) x)) 5))
(newline)

(define (<= a b) (not (> a b)))
(define (>= a b) (not (< a b)))

(define (half-interval-method f a b)
 (define (search-root f negative-point positive-point)
  (define (close-enough? a b) (< (abs (- a b)) 0.0001))
  (let ((mid-point (average negative-point positive-point)))
	(if (close-enough? negative-point positive-point) 
	  mid-point
	  (let ((mid-value (f mid-point)))
		(cond ((> mid-value 0) (search-root f negative-point mid-point))
			  ((< mid-value 0) (search-root f mid-point positive-point))
			  (else mid-point))))))
 ; check if the end points have different sign
 (let ((fa (f a))
	  (fb (f b)))
  (cond ((and (<= fa 0) (>= fb 0)) (search-root f a b))
   ((and (>= fa 0) (<= fb 0)) (search-root f b a))
   (else (display "end points have the same sign!")
	(newline)))))

(display (half-interval-method sin 4.0 3.2))
(newline)
(display (half-interval-method sin 4.0 2.0))
(newline)
(display (half-interval-method (lambda (x) (- (cube x) (* 2 x) 3)) 1.0 2.0))
(newline)

(define (fix-point f)
 (define (find-fix-point guess)
  (define (close-enough? a b)
   (< (abs (- a b)) 0.00001))
  (let ((fguess (f guess)))
   (if (close-enough? fguess guess)
	guess
	(find-fix-point fguess))))
 (find-fix-point 1.0))

(display (fix-point cos))
(newline)
(display (fix-point (lambda (x) (+ (sin x) (cos x)))))
(newline)
; exercise 1.35
(display (fix-point (lambda (x) (+ 1 (/ 1 x)))))
(newline)

(define (ssqrt x)
 (fix-point (lambda (y) (average y (/ x y)))))

(display (ssqrt 87))
(newline)

; exercise 1.36
(define (fixed-point f first-guess)
 (define (find-fixed-point step guess)
  (define (close-enough? a b) (< (abs (- a b)) 0.0001))
  (define (show-verbos-msg)
   (display step)
   (display " : ")
   (display guess)
   (newline))
  (show-verbos-msg)
  (let ((fguess (f guess)))
   (if (close-enough? fguess guess)
	guess
	(find-fixed-point (+ step 1) fguess))))
 (display "--------------------start---------------------")
 (newline)
 (find-fixed-point 1 first-guess))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)

(fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 2.0)

; exercise 1.37
; This recusive process will lead to stack overflow
; Top-Down approach
(define (cont-frac n d k)
 (define (iter i)
  (if (= i k)
   (/ (n i) (d i))
   (/ (n i) (+ (d i) (iter (+ i 1))))))
 (iter 1))

; The iterative version
; Buttom-Up approach
(define (cont-frac-iterative n d k)
 (define (iter i r)
  (cond ((= i 0) r)
   ((= i k) (iter (- i 1) (/ (n k) (d k))))
   (else (iter (- i 1) (/ (n i) (+ (d i) r))))))
 (iter k 0))

; iterative version, no stack overflow will happen.
(display (cont-frac-iterative (lambda (i) 1.0) (lambda (i) 1.0) 4000))
(newline)

(display (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 400))
(newline)

(define (n i) 1.0)
(define (d i) 
 (if (= (remainder i 3) 2)
  (/ (* 2 (+ i 1)) 3) 1))
; exercise 1.38
(display (+ (cont-frac-iterative n d 2000) 2))
(newline)
(display (+ (cont-frac n d 200) 2))
(newline)
; stack overflow
;(display (+ (cont-frac n d 2000) 2))
;(newline)

(define (tan-cf x k)
 (define (n i) 
  (if (= i 1) x (- (square x))))
 (define (d i) (- (* 2 i) 1))
 (cont-frac-iterative n d k))

(display (tan (/ 3.14 4)))
(newline)

(display "----------------")
(newline)
(display (tan-cf (/ 3.14 4) 10))
(newline)

