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
  ((not (pair? lst)) 1)
  (else (+ (count-leaves (car lst))
		 (count-leaves (cdr lst))))))

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

;(define (deep-list-reverse lst)
; (display "now reverse list ====== ")
; (display lst)
; (newline)
; (cond ((null? lst) ())
;  ((not (pair? lst)) lst)
;  (else (list-reverse 
;		 (list 
;		  (deep-list-reverse (car lst))
;		  (deep-list-reverse (cadr lst)))))))

;(define (deep-list-reverse lst)
; (cond ((or (null? lst) (not (pair? lst))) lst)
;  ((null? (cdr lst)) (deep-list-reverse (car lst)))
;  (else (list-reverse (list (deep-list-reverse (car lst))
;					   (deep-list-reverse (cdr lst)))))))

;(define (deep-list-reverse lst)
; (define (iter origin-lst res-lst)
;  (cond ((null? origin-lst) res-lst)
;   ((not (pair? origin-lst)) (cons origin-lst res-lst))
;   (else (iter (cdr origin-lst) (append (deep-list-reverse (car lst)) res-lst)))))
; (display "now reverse list ====== ")
; (display lst)
; (newline)
; (iter lst nil))

;(define (list-map lst f)
; (define (iter origin-lst res-lst)
;  (if (null? origin-lst) res-lst
;   (iter (cdr origin-lst) (cons (f (car origin-lst)) res-lst))))
; (list-reverse (iter lst '())))

(define (list-map lst f)
 (define (iter origin-lst res-lst)
  (if (null? origin-lst) res-lst
   (iter (cdr origin-lst) (append res-lst (list (f (car origin-lst)))))))
 (iter lst '()))

(display "list-map test ==========")
(newline)
(display (list-map (list 1 2 3 4) (lambda (x) (* x x))))
(newline)

; 1st deep-list-reverse implementation
(define (deep-list-reverse lst)
 (define (iter remained-items result)
  (if (null? remained-items) result
   (let ((to-reverse (car remained-items)))
	(iter (cdr remained-items)
	 (cons (if (pair? to-reverse) (deep-list-reverse to-reverse) to-reverse)
	 result)))))
 (iter lst nil))

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

