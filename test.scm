; exercise 2.23
(define (list-length lst)
 (define (iter llst r)
  (if (null? llst) r
   (iter (cdr llst) (+ r 1))))
 (iter lst 0))

(define l (list 1 2 3 4 5 6 7 8 9))

(display (list-length l))
(newline)

(define x (cons (list 1 2) (list 3 4)))
(display (list-length x))
(newline)

(define (count-leaves lst)
 (cond ((null? lst) 0)
  ((pair? lst) 
   (+ (count-leaves (car lst))
	(count-leaves (cdr lst))))
  (else 1)))

; This implementation will still generate recursive processing
;(define (count-leaves lst)
; (define (iter items-left res)
;   (if (null? items-left) res
;	(let ((now-checking (car items-left))
;		   (to-check (cdr items-left)))
;	  (iter to-check (+ res 
;		(if (pair? now-checking) (count-leaves now-checking) 1))))))
; (iter lst 0))

(display "-------- count-leaves test --------")
(newline)
(display (count-leaves x))
(newline)

(define l (list 1 (list 2 (list 3 4))))
(display l)
(newline)

(define l1 (list 1 3 (list 5 7) 9))
(define l2 (list (list 7)))
(define l3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(display l1)
(newline)
(display l2)
(newline)
(display l3)
(newline)

(display "=====================")
(newline)
(display (car (cdaddr l1)))
(newline)
(display (caar l2))
(newline)
(display (cadadr (cadadr (cadadr l3))))
(newline)
(display (car (cdr (car (cdr (cdr l1))))))
(newline)
(display (car (car l2)))
(newline)
(display (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l3)))))))))))))
(newline)

(define nil (cdr (list 1)))
; exercise 2.27
(define (list-reverse lst)
 (define (iter llst res)
  (if (null? llst)
   res
   (iter (cdr llst) (cons (car llst) res))))
 (iter lst nil))

(define x (list (list 1 2) (list 3 4)))
(display (list-reverse x))
(newline)

; iterative implementation with append
(define (list-map lst f)
 (define (iter origin-lst res-lst)
  (if (null? origin-lst) res-lst
   (iter (cdr origin-lst) (append res-lst (list (f (car origin-lst)))))))
 (iter lst '()))

; iterative implementation with cons & list-reverse
(define (list-map lst f)
 (define (iter origin-lst res-lst)
  (if (null? origin-lst) res-lst
   (iter (cdr origin-lst) (cons (f (car origin-lst)) res-lst))))
 (list-reverse (iter lst nil)))

; recursive implementation of list-map
(define (list-map lst f)
 (if (null? lst) nil
  (cons (f (car lst)) (list-map (cdr lst) f))))

(display "list-map test ==========")
(newline)
(display (list-map (list 1 2 3 4) (lambda (x) (* x x))))
(newline)

; map of tree
; recursive implementation of deep-list-map
(define (deep-list-map lst f)
 (define (iter origin-lst res-lst)
  (if (null? origin-lst) res-lst
   (let ((ca (car origin-lst))
		 (cd (cdr origin-lst)))
	(iter cd (cons 
			  (if (pair? ca) (deep-list-map ca f) (f ca)) res-lst)))))
 (list-reverse (iter lst nil)))
; recursive implementation of deep-list-map with cons
(define (deep-list-map lst f)
 (cond ((null? lst) nil)
  ((pair? lst)
   (cons (deep-list-map (car lst) f) (deep-list-map (cdr lst) f)))
  (else (f lst))))

(define x (list (list 1 (list 5 6 (list 8 9) 7)  2) (list 3 4)))
(display "deep-list-map test ==========")
(newline)
(display (deep-list-map x (lambda (x) (* x x))))
(newline)

; 1st deep-list-reverse implementation
;(define (deep-list-reverse lst)
; (define (iter remained-items result)
;  (if (null? remained-items) result
;   (let ((to-reverse (car remained-items)))
;	(iter (cdr remained-items)
;	 (cons (if (pair? to-reverse) (deep-list-reverse to-reverse) to-reverse)
;	 result)))))
; (iter lst nil))

; 2nd deep-list-reverse implementation
(define (deep-list-reverse lst)
 (if (pair? lst)
  (list-reverse (list-map lst deep-list-reverse))
  lst))

(display "========= deep-list-reverse =========")
(newline)
(display (deep-list-reverse l3))
(newline)

(define x (list 1 2 (list 3 4) 5))
(display x)
(newline)
(display (deep-list-reverse x))
(newline)

;exercise 2.28
(define (fringe tree)
 (define (iter remained-items result)
  (cond ((null? remained-items) result)
   (else (let ((to-test (car remained-items))
			   (next-loop-items (cdr remained-items)))
		  (iter next-loop-items
		   (append result (if (pair? to-test) (fringe to-test) (list to-test))))))))
 (iter tree nil))

(display "========== fringe ===========")
(define x (list (list 1 2 (list (list 0 9) 5 6)) (list 3 4)))
(display x)
(newline)
(display (fringe x))
(newline)

; exercise 2.29
;(define (make-mobile left right) (list left right))
;(define (make-branch length structure) (list length structure))
;(define (left-branch mobile) (car mobile))
;(define (right-branch mobile) (cadr mobile))
;(define (branch-length branch) (car branch))
;(define (branch-structure branch) (cadr branch))

; another implementation of mobile and branch
(define (make-mobile left right) (cons left right))
(define (make-branch length structure) (cons length structure))
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cdr mobile))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cdr branch))

(define (total-weight mobile)
 (define (branch-weight br)
  (let ((br-stru (branch-structure br)))
   (if (pair? br-stru) (total-weight br-stru) br-stru)))
 (+ (branch-weight (left-branch mobile))
  (branch-weight (right-branch mobile))))

(define (branch-torque br)
 (let ((br-len (branch-length br))
	   (br-stru (branch-structure br)))
  (* br-len (if (pair? br-stru) (total-weight br-stru) br-stru))))

(define (mobile-balanced mobile)
 (let ((left-br (left-branch mobile))
	   (right-br (right-branch mobile)))
  (and (= (branch-torque left-br) (branch-torque right-br))
   (if (pair? (branch-structure left-br)) 
	(mobile-balanced (branch-structure left-br)) 1)
   (if (pair? (branch-structure right-br)) 
	(mobile-balanced (branch-structure right-br)) 1))))

(define b1 (make-branch 10 20))
(define b2 (make-branch 10 20))
(define m1 (make-mobile b1 b2))

(define b3 (make-branch 10 20))
(define b4 (make-branch 10 20))
(define m2 (make-mobile b3 b4))

(define b5 (make-branch 10 m1))
(define b6 (make-branch 10 m2))

(define m (make-mobile b5 b6))

(display "total weight -->  ")
(display (total-weight m))
(newline)

(display "left branch toque -->  ")
(display (branch-torque (left-branch m)))
(newline)
(display "right branch toque -->  ")
(display (branch-torque (right-branch m)))
(newline)

(if (mobile-balanced m)
 (display "balanced")
 (display "not balanced"))
(newline)

; exercise 2.32
; My own implementation of (subsets s) 
; return the set of all the subset of set s
;(define (subsets s)
; ; The set ss after all its elements are appended by elem
; ; the new appended set is returned
; (define (helper elem ss)
;  (define (iter items res)
;   (if (null? items) res
;	(iter (cdr items) (append res (list (append (car items) (list elem)))))))
;  (iter ss nil))
; (if (null? s) (list nil)
;  (let ((sscds (subsets (cdr s))))
;   (append sscds (helper (car s) sscds)))))

; from sicp
; just perfect!
(define (subsets s)
 (if (null? s) (list nil)
  (let ((sscds (subsets (cdr s)))
		(sca (car s)))
   (append sscds (list-map sscds 
				  (lambda (x) (append x (list sca))))))))

(display "======== subsets test ========")
(newline)
(display (subsets (list 1 2 3)))
(newline)

(define (list-filter lst f)
 (if (null? lst) nil
  (let ((ca (car lst))
		(cd (cdr lst)))
   (if (f ca)
	(cons ca (list-filter cd f))
	(list-filter cd f)))))

(display "======== list-filter ========")
(newline)
(display (list-filter (list 1 2 3 4 5 6)
		  (lambda (x) (= (remainder x 2) 0))))
(newline)

;(define (list-accumulator lst f init-value)
; (define (iter L res)
;  (if (null? L) res
;   (iter (cdr L) (f (car L) res))))
; (iter lst init-value))

(define (list-accumulator lst f init-value)
 (if (null? lst) init-value
  (f (car lst) (list-accumulator (cdr lst) f init-value))))

(display "======== list-accumulator ========")
(newline)
(display (list-accumulator (list 1 2 3 4 5 6) + 0))
(newline)


(define (enumerate-interval low high)
 (define (iter llow res)
  (if (> llow high) res
   (iter (+ llow 1) (append res (list llow)))))
 (iter low nil))

(display "======== enumerate-interval ========")
(newline)
(display (enumerate-interval 2 7))
(newline)

(define (enumerate-tree tree)
 (if (null? tree) nil
  (append (if (pair? (car tree)) 
		   (enumerate-tree (car tree)) (list (car tree)))
   (enumerate-tree (cdr tree)))))

(display "======== enumerate-tree ========")
(newline)
(display (enumerate-tree (list 1 (list 2 (list 3 (list 4 (list 5)))))))
(newline)

(define (sum-odd-square tree)
 (list-accumulator (list-map (list-filter (enumerate-tree tree) odd?)
					(lambda (x) (* x x))) + 0))

(display "======== sum-odd-square ========")
(newline)
(display (sum-odd-square (list 1 (list 2 (list 3 (list 4 (list 5)))))))
(newline)

(define (fib n)
 (define (iter a b i)
  (if (= i n) a
   (iter b (+ a b) (+ i 1))))
 (iter 0 1 0))

(define (even-fibs n)
 (list-accumulator
  (list-filter (list-map (enumerate-interval 0 n) fib) even?)
  cons nil))

(display "======== even-fibs ========")
(newline)
(display (even-fibs 20))
(newline)

; exercise 2.33
; (1)
(define (l-map p lst)
 (list-accumulator lst (lambda (x y) (cons (p x) (l-map p (cdr lst)))) nil))

(display "======== l-map ========")
(newline)
(display (l-map (lambda (x) (* x x)) (list 1 2 3 4)))
(newline)

; (2)
(define (l-append l1 l2)
 (list-accumulator l1 cons l2))

(display "======== l-append ========")
(newline)
(define l1 (list 1 2 3))
(define l2 (list 4 5 6))
(display (l-append l2 l1))
(newline)

; (3)
(define (l-length lst)
 (list-accumulator lst (lambda (x y) (+ 1 y)) 0))

(display "======== l-length ========")
(newline)
(display (l-length (list 1 2 3 4 5 6 7)))
(newline)

; exercise 2.34
(define (horner-eval x lst)
 (list-accumulator lst (lambda (a0 a1) (+ a0 (* x a1))) 0))


(display "======== horner-eval ========")
(newline)
(display (horner-eval 2 (list 1 3 0 5 0 1)))
(newline)

; exercise 2.35
;(define (new-count-leaves t)
; (list-accumulator (fringe t) (lambda (x y) (+ 1 y)) 0))
(define (new-count-leaves t)
 (list-accumulator (list-map t 
					(lambda (x) (if (pair? x) 
								 (new-count-leaves x) 1))) + 0))

(display "======== new-count-leaves ========")
(newline)
(display (new-count-leaves (list (list 1 3) (list 0 (list 5 0) ) 1)))
(newline)

(define (make-list elem n)
 (if (> n 0) (cons elem (make-list elem (- n 1))) nil))
(display (make-list 1 10))
(newline)
; exercise 2.36
; my implementation
;(define (list-accumulator-n lst f init-value)
; (define (vector-f x y)
;  (if (null? y) nil
;   (cons (f (car x) (car y)) (vector-f (cdr x) (cdr y)))))
; (list-accumulator lst vector-f (make-list init-value (list-length (car x)))))

(define (list-accumulator-n lst f init-value)
 (if (null? (car lst)) nil
  (cons (list-accumulator (list-map lst car) f init-value)
   (list-accumulator-n (list-map lst cdr) f init-value))))

(display "======== list-accumulator-n ========")
(newline)
(display (list-accumulator-n (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) + 0))
(newline)

; exercise 2.37
(define (dot-product v w)
 (list-accumulator (list-accumulator-n (list v w) * 1) + 0))
;(define (dot-product v w)
; (list-accumulator (map * v w) + 0))
(display (dot-product (list 1 2 3) (list 4 5 6)))
(newline)

;(define (matrix-*-vector m v)
; (if (null? m) nil
;  (cons (dot-product (car m) v)
;   (matrix-*-vector (cdr m) v))))

(define (matrix-*-vector m v)
 (list-map m (lambda (x) (dot-product x v))))

(define M (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define V (list 1 2 3 4))

(display "========== matrix-*-vector ===========")
(newline)
(display (matrix-*-vector M V))
(newline)

;(define (transpose m)
; (if (null? (car m)) nil
;  (cons (list-map m car) (transpose (list-map m cdr)))))

(define (transpose m)
 (list-accumulator-n m cons nil))

(display (transpose M))
(newline)

;(define (matrix-*-matrix m n)
; (define (helper o)
;  (if (null? (car o)) nil
;   (cons (matrix-*-vector m (list-map o car))
;	(helper (list-map o cdr)))))
; (transpose (helper n)))

;(define (matrix-*-matrix m n)
; (transpose (list-map (transpose n) (lambda (x) (matrix-*-vector m x)))))

(define (matrix-*-matrix m n)
 (let ((n-tranz (transpose n)))
  (list-map m (lambda (x) (matrix-*-vector n-tranz x)))))

(define N (list (list 2 7 1 5 7) (list 3 8 2 6 8)
		   (list 4 9 3 7 9) (list 5 0 4 8 0)))

(display "========== matrix-*-matrix ===========")
(newline)
(display (matrix-*-matrix M N))
(newline)


; exercise 2.38
(define (fold-left lst f init-val)
 (define (iter rest res)
  (if (null? rest) res
   (iter (cdr rest) (f res (car rest)))))
 (iter lst init-val))

(define fold-right list-accumulator)

(display (fold-left (list 1 2 3) / 1))
(newline)
(display (fold-right (list 1 2 3) / 1))
(newline)

(display (fold-left (list 1 2 3) list nil))
(newline)
(display (fold-right (list 1 2 3) list nil))
(newline)

; exercise 2.39
(define (l-reverse lst)
 (fold-right lst
  (lambda (x y) (append y (list x))) nil))

(display (l-reverse (list 1 2 3 4)))
(newline)

(define (l-reverse lst)
 (fold-left lst
  (lambda (x y) (cons y x)) nil))

(display (l-reverse (list 1 2 3 4)))
(newline)

