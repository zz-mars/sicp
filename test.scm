(define true (= 0 0))
(define false (= 1 0))
(define (<= x y) (not (> x y)))
(define (>= x y) (not (< x y)))
(define nil '())
(define (n-pair x) (not (pair? x)))
(define (n-null x) (not (null? x)))
(define (list-len lst)
 (define (iter L len)
  (if (null? L) len
   (iter (cdr L) (+ len 1))))
 (iter lst 0))

; (list-split lst idx)
; Split 'lst' from index 'idx'
; return a list like this 
; (part1 pivot part2)
(define (list-split lst idx)
 (define (iter pt1 pt2 i)
  (cond ((< i idx)
		 (iter (append pt1 (list (car pt2)))
		  (cdr pt2) (+ i 1)))
   ((= i idx) (list pt1 (car pt2) (cdr pt2)))
   (else (list nil nil lst))))
 (if (> idx (list-len lst))
  (list lst nil nil)
  (iter nil lst 1)))

(display "+++++++++++ list split ++++++++++")
(newline)
(display (list-split (list 1 2 3 4 5 6 7 8) 0))
(newline)

(define (list-filter lst f)
 (if (null? lst) nil
  (append 
   (if (f (car lst)) (list (car lst)) nil) 
   (list-filter (cdr lst) f))))

;(display (list-filter (list 1 2 3 4 5 6) (lambda (x) (= (remainder x 2) 0))))
;(newline)
(define (memq item x)
 (cond ((null? x) false)
  ((eq? item (car x)) x)
  (else (memq item (cdr x)))))

(display (memq 'apple '(pear banana apple orange)))
(newline)

(define (equal? la lb)
 (cond ((and (null? la) (null? lb)) true)
  ((and (n-pair la) (n-pair lb))
		  (eq? la lb))
  ((and (pair? la) (pair? lb))
	 (and (equal? (car la) (car lb))
	  (equal? (cdr la) (cdr lb))))
  (else false)))
   
(display (if (equal? '(this is a list) '(this is a list)) 
		  "equal" "not equal"))
(newline)

(display (if (equal? '(this is a list) '(this '(is a) list)) 
		  "equal" "not equal"))
(newline)

(define (variable? x) (symbol? x))
(define (same-variable? x y)
 (and (variable? x) (variable? y) (eq? x y)))
(define (=number? expr n)
 (and (number? expr) (= expr n)))

; 1st implementation
;(define (make-sum a b) 
; (cond ((=number? a 0) b)
;  ((=number? b 0) a)
;  ((and (number? a) (number? b)) (+ a b))
;  (else (list '+ a b))))
;
;(define (make-product a b)
; (cond ((or (=number? a 0) (=number? b 0)) 0)
;  ((eq? a '1) b)
;  ((eq? b '1) a)
;  ((and (number? a) (number? b)) (* a b))
;  (else (list '* a b))))
;(define (sum? x)
; (and (pair? x) (eq? (car x) '+)))
;(define (addend s) (cadr s))
;(define (addgend s) 
; (if (> (list-len s) 3)
;  (append (list '+) (cddr s))
;  (caddr s)))
;(define (product? x)
; (and (pair? x) (eq? (car x) '*)))
;(define (multiplier p) (cadr p))
;(define (multiplicand p)
; (if (> (list-len p) 3)
;  (append (list '*) (cddr p))
;  (caddr p)))
;
;(define (exponentiation? expr)
; (and (pair? expr) (eq? (car expr) '**)))
;(define (base expr) (cadr expr))
;(define (exponent expr) (caddr expr))
;(define (make-exponentiation bs expn)
; (cond ((=number? expn 0) 1)
;  ((=number? expn 1) bs)
;  (else (list '** bs expn))))

; 2nd implementation
; exercise 2.58 (a)
; This works for fully parenthesized expressions
; and some not fully parenthesized ones
(define (sum? expr)
 (and (pair? expr) (eq? (cadr expr) '+)))
(define (addend expr) (car expr))
(define (addgend expr) 
 (if (> (list-len expr) 3)
  (cddr expr)
  (caddr expr)))
(define (make-sum a b)
 (cond ((=number? a 0) b)
  ((=number? b 0) a)
  ((and (number? a) (number? b)) (+ a b))
  (else (list a '+ b))))

(define (product? expr)
 (and (pair? expr) (eq? (cadr expr) '*)))
(define (multiplier expr) (car expr))
(define (multiplicand expr)
 (if (> (list-len expr) 3)
  (cddr expr)
  (caddr expr)))
(define (make-product a b)
 (cond ((or (=number? a 0) (=number? b 0)) 0)
  ((=number? a 1) b)
  ((=number? b 1) a)
  (else (list a '* b))))

(define (exponentiation? expr)
 (and (pair? expr) (eq? (cadr expr) '**)))
(define (base expr) (car expr))
(define (exponent expr) (caddr expr))
(define (make-exponentiation bs expo)
 (cond ((=number? expo 0) 1)
  ((=number? expo 1) bs)
  (else (list bs '** expo))))

; differentiation procedure
(define (deriv expr var)
 (cond ((number? expr) 0)
  ((variable? expr)
   (if (same-variable? expr var) 1 0))
  ((sum? expr) 
   (make-sum (deriv (addend expr) var)
	(deriv (addgend expr) var)))
  ((product? expr)
   (make-sum (make-product (multiplier expr)
			  (deriv (multiplicand expr) var))
	(make-product (deriv (multiplier expr) var)
	 (multiplicand expr))))
  ((exponentiation? expr)
   (let ((bs (base expr))
		 (expn (exponent expr)))
	(make-product 
	 (make-product expn (make-exponentiation bs (make-sum expn (- 1))))
	 (deriv bs var))))
  (else (display "unknown expression type!")
   (newline))))

(display "======== deriv test =========")
(newline)

; test for 1st implementation
;(display (deriv '(+ x 3) 'x))
;(newline)
;(display (deriv '(* x y) 'x))
;(newline)
;(display (deriv '(* (* x y) (+ x 3)) 'x ))
;(newline)
;(display (deriv '(+ (** (+ (** x 3) (* x y)) 2) (* x y)) 'x))
;(newline)
;(display (deriv '(* x y (+ x 3)) 'x))
;(newline)

; test for 2nd implementation
(display (deriv '(x + 3 * (x + y + 2)) 'x))
(newline)
(display (deriv '(x + 3) 'x))
(newline)
(display (deriv '(x * y) 'x))
(newline)
(display (deriv '(x * y * (x + 3)) 'x))
(newline)
(display (deriv '((((x ** 3) + (x * y)) ** 2) + (x * y)) 'x))
(newline)

; ===================== sets (list representation) ======================
(define (element-of-set? x set)
 (cond ((null? set) false)
  ((equal? x (car set)) true)
  (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
 (if (element-of-set? x set)
  set
  (cons x set)))

(define (union-set s1 s2)
 (if (null? s2) s1
  (union-set (adjoin-set (car s2) s1) (cdr s2))))

(define (intersection-set s1 s2)
 (list-filter s1 (lambda (elem) (element-of-set? elem s2))))

; =============== set (ordered list representation)===============
(define (ol-element-of-set? x set)
 (cond ((null? set) false)
  ((< x (car set)) flase)
  ((= x (car set)) true)
  (else (ol-element-of-set? x (cdr set)))))

(define (ol-intersection-set s1 s2)
 (if (or (null? s1) (null? s2)) nil
  (let ((x1 (car s1))
		(y1 (car s2)))
   (cond ((< x1 y1) (ol-intersection-set (cdr s1) s2))
	((= x1 y1) (cons x1 (ol-intersection-set (cdr s1) (cdr s2))))
	((> x1 y1) (ol-intersection-set s1 (cdr s2)))))))

(define (ol-adjoin-set x set)
 (cond ((null? set) (list x))
  ((< x (car set)) (cons x set))
  ((= x (car set)) set)
  (else (cons (car set) (ol-adjoin-set x (cdr set))))))

(define (ol-union-set s1 s2)
 (define (iter res-part1 res-part2 items-left)
  (cond ((null? items-left) (append res-part1 res-part2))
   ((null? res-part2) (append res-part1 items-left))
   (else (let ((x (car items-left))
			   (y (car res-part2)))
		  (cond ((< x y)
				 (iter (append res-part1 (list x)) res-part2 (cdr items-left)))
		   ((= x y)
			(iter (append res-part1 (list x)) (cdr res-part2) (cdr items-left)))
		   ((> x y)
			(iter (append res-part1 (list y)) (cdr res-part2) items-left)))))))
 (iter nil s1 s2))

(define S1 (list 1 2 3 4 5))
(define S2 (list 1 3 5 7 9))
(display "=========== ordered-list representation ============")
(newline)
(display S1)
(newline)
(display S2)
(newline)
(if (ol-element-of-set? 3 S1)
 (display "yes")
 (display "no"))
(newline)
(if (ol-element-of-set? 7 S1)
 (display "yes")
 (display "no"))
(newline)
(newline)
(display (ol-adjoin-set 6 S2))
(newline)
(display (ol-union-set S1 S2))
(newline)
(display (ol-intersection-set S1 S2))
(newline)

; ============ tree representation ===========
(define (entry tree-node) (car tree-node))
(define (left-branch tree-node)
 (cadr tree-node))
(define (right-branch tree-node)
 (caddr tree-node))
(define (make-entry entry-val left right)
 (list entry-val left right))

(define (t-element-of-set? x set)
 (if (null? set) false
  (let ((ent-val (entry set)))
   (cond 
	((< x ent-val) (t-element-of-set? x (left-branch set)))
	((= x ent-val) true)
	((> x ent-val) (t-element-of-set? x (right-branch set)))))))

(define (t-adjoin-set x set)
 (if (null? set) (make-entry x nil nil)
  (let ((ent-val (entry set))
		(l-b (left-branch set))
		(r-b (right-branch set)))
   (cond ((< x ent-val) (make-entry ent-val
						 (t-adjoin-set x l-b) r-b))
	((= x ent-val) set)
	((> x ent-val) 
	 (make-entry ent-val
	  l-b (t-adjoin-set x r-b)))))))

; exercise 2.63
(define (tree->list tree)
 (if (null? tree) nil
  (append (tree->list (left-branch tree))
   (list (entry tree))
   (tree->list (right-branch tree)))))

(define t1 (make-entry 1 nil nil))
(define t2 (make-entry 3 nil nil))
(define t3 (make-entry 5 nil nil))
(define t4 (make-entry 7 nil nil))

(define t5 (make-entry 2 t1 t2))
(define t6 (make-entry 6 t3 t4))
(define t7 (make-entry 4 t5 t6))

(display "================ binary tree ==============")
(newline)
(display (tree->list t7))
(newline)

; exercise 2.63
(define tn1 (make-entry 1 nil nil))
(define tn3 (make-entry 3 nil nil))
(define tn5 (make-entry 5 nil nil))
(define tn7 (make-entry 7 nil nil))
(define tn9 (make-entry 9 nil nil))
(define tn11 (make-entry 11 nil nil))

(define T1 (make-entry 7
			(make-entry 3
			 tn1 tn5)
			(make-entry 9
			 nil tn11)))
(define T2 (make-entry 3
			tn1
			(make-entry 7
			 tn5
			 (make-entry 9
			  nil tn11))))
(define T3 (make-entry 5
			(make-entry 3
			 tn1 nil)
			(make-entry 9
			 tn7 tn11)))

(display (tree->list T1))
(newline)
(display (tree->list T2))
(newline)
(display (tree->list T3))
(newline)

(define (tree->list-1 tree)
 (if (null? tree) nil
  (append (tree->list-1 (left-branch tree))
		   (cons (entry tree)
			(tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
 (define (copy-to-list tree res)
  (if (null? tree) res
   (copy-to-list (left-branch tree)
	(cons (entry tree)
	 (copy-to-list (right-branch tree) res)))))
 (copy-to-list tree nil))

(display "+++++++ tree->list-1 ++++++++")
(newline)
(display (tree->list-1 T1))
(newline)
(display (tree->list-1 T2))
(newline)
(display (tree->list-1 T3))
(newline)
(display (tree->list-1 t7))
(newline)

(display "+++++++ tree->list-2 ++++++++")
(newline)
(display (tree->list-2 T1))
(newline)
(display (tree->list-2 T2))
(newline)
(display (tree->list-2 T3))
(newline)
(display (tree->list-2 t7))
(newline)

; exercise 2.64
; (1) list is null 
;	--> nil
; (2) length of list is 1 
;	--> make a tree of only one node
; (3) length of list > 1
;	--> split the list into three parts
;	1) left branch
;	2) root node
;	3) right branch
;	make-entry with this three parts
(define (list->tree lst)
 (if (null? lst) nil
  (let ((l-len (list-len lst)))
   (if (= l-len 1)
	(make-entry (car lst) nil nil)
	(let ((splt (list-split lst (quotient (+ l-len 1) 2))))
	 (make-entry (cadr splt) 
	  (list->tree (car splt))
	  (list->tree (caddr splt))))))))

(define T (list->tree (list 1 3 5 7 9 11)))

(display "+++++++++++++ list->tree +++++++++++")
(newline)
(display T)
(newline)
(display (tree->list T))
(newline)

; exercise 2.64 : sicp implementation
(define (partial-tree lst n)
 (if (= n 0) (cons nil lst)
  (let ((left-sz (quotient n 2)))
   (let ((left-part (partial-tree lst left-sz)))
	(let ((left-tree (car left-part))
		  (non-left (cdr left-part))
		  (right-sz (- n (+ left-sz 1))))
	 (let ((entry-val (car non-left))
		   (right-part (partial-tree (cdr non-left) right-sz)))
	  (cons (make-entry entry-val left-tree (car right-part))
	   (cdr right-part))))))))

(define (list->tree1 lst)
 (car (partial-tree lst (list-len lst))))

(display "+++++++++++++ list->tree sicp implementation +++++++++++")
(newline)
(define T1 (list->tree1 (list 1 3 5 7 9 11)))
(display T1)
(newline)
(display (tree->list T1))
(newline)

; exercise 2.65
(define (t-union-set s1 s2)
 (list->tree1 
  (ol-union-set
   (tree->list-1 s1)
   (tree->list-2 s2))))

(define (t-intersection-set s1 s2)
 (list->tree
  (ol-intersection-set
   (tree->list-1 s1)
   (tree->list-2 s2))))

(display "++++++++++++ exercise 2.65 +++++++++++")
(newline)
(display (t-intersection-set T1 T2))
(newline)
(display (t-union-set T1 T2))
(newline)

; huffman tree
; leaf : ('leaf symbol weight)
(define (make-leaf symbol weight)
 (list 'leaf symbol weight))
(define (leaf? obj)
 (eq? (car obj) 'leaf))
(define (symbol-leaf leaf)
 (cadr leaf))
(define (weight-leaf leaf)
 (caddr leaf))

; general tree node : 
; (left-branch right-branch symbol-list total-weight)
(define (left-branch t) (car t))
(define (right-branch t) (cadr t))
(define (symbols t)
 (if (leaf? t)
  (list (symbol-leaf t))
  (caddr t)))
(define (weight t)
 (if (leaf? t)
  (weight-leaf t)
  (cadddr t)))
(define (make-code-tree left right)
 (list left right
  (append (symbols left) (symbols right))
  (+ (weight left) (weight right))))

(define (decode bits tree)
 (define (next-branch i current-branch)
  (cond ((= i 0) (left-branch current-branch))
   ((= i 1) (right-branch current-branch))
   (else (display "error encoding!")
	(newline))))
 (define (iter bts res current-branch)
  (if (null? bts) res
   (let ((next-b (next-branch (car bts) current-branch)))
	(if (leaf? next-b)
	 (iter (cdr bts)
	  (append res (list (symbol-leaf next-b))) tree)
	 (iter (cdr bts) res next-b)))))
 (iter bits nil tree))

(define (hufm-adjoin-set x set)
 (cond ((null? set) (list x))
  ((< (weight x) (weight (car set)))
   (cons x set))
  (else (cons (car set) (hufm-adjoin-set x (cdr set))))))

; make-leaf-set :
; input : (('A 3) ('B 2) ('C 1))
(define (make-leaf-set pairs)
 (define (iter p res)
  (if (null? p) res
   (let ((pair (car p)))
	(iter (cdr p)
	 (hufm-adjoin-set (make-leaf (car pair)
					   (cadr pair)) res)))))
 (iter pairs nil))
(display "00000000 huffman make-leaf-set test 0000000000")
(newline)
(display (make-leaf-set '((A 3) (B 2) (C 1))))
(newline)

(define (make-huffman-tree-helper leaf-set)
 (let ((set-len (list-len leaf-set)))
  (cond ((= set-len 0) nil)
   ((= set-len 1) (car leaf-set))
   (else (make-huffman-tree-helper
		  (hufm-adjoin-set 
		   (make-code-tree 
			(car leaf-set) (cadr leaf-set))
		   (cddr leaf-set)))))))

(define (make-huffman-tree pairs)
 (make-huffman-tree-helper (make-leaf-set pairs)))

(display "=========== make-huffman-tree test ==============")
(newline)
(display (make-huffman-tree '((A 3) (B 2) (C 1))))
(newline)

; exercise 2.67
;(define sample-tree
; (make-code-tree (make-leaf 'A 4)
;  (make-code-tree
;   (make-leaf 'B 2)
;   (make-code-tree (make-leaf 'D 1)
;	(make-leaf 'C 1)))))

(define sample-tree
 (make-huffman-tree '((A 4) (B 2) (C 1) (D 1))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(display "-------------- decode test --------------")
(newline)
(display (decode sample-message sample-tree))
(newline)

; exercise 2.68 encode
;(define (encode lst tree)
; (define (iter items res current-branch)
;  (if (null? items) res
;   (let ((cur-ch (car items))
;		 (br-items (symbols current-branch))
;		 (l-br (left-branch current-branch))
;		 (r-br (right-branch current-branch)))
;	(let ((next-iter-arg 
;		   (cond ((element-of-set? cur-ch (symbols l-br))
;				  (list l-br 0))
;			((element-of-set? cur-ch (symbols r-br))
;			 (list r-br 1))
;			(else nil))))
;	 (let ((next-br (car next-iter-arg))
;		   (next-enc (cdr next-iter-arg)))
;	  (let ((next-items-br
;			 (if (leaf? next-br)
;			  (cons (cdr items) tree)
;			  (cons items next-br))))
;	   (iter (car next-items-br)
;		(append res next-enc) (cdr next-items-br))))))))
; (iter lst nil tree))

(define (encode msg tree)
 (define (encode-symbol cur-sym cur-br)
  (if (leaf? cur-br) nil
   (let ((l-br (left-branch cur-br))
		 (r-br (right-branch cur-br)))
	(cond ((element-of-set? cur-sym (symbols l-br))
		   (cons 0 (encode-symbol cur-sym l-br)))
	 ((element-of-set? cur-sym (symbols r-br))
	  (cons 1 (encode-symbol cur-sym r-br)))
	 (else (display "invalid --> ")
	  (display cur-sym)
	  (newline))))))
 (if (null? msg) nil
  (append (encode-symbol (car msg) tree)
   (encode (cdr msg) tree))))

(display "'''''''''''' encode test ''''''''''''''")
(newline)
(display (encode '(A D A B B C A) sample-tree))
(newline)

; exercise 2.70
(define hufm-tree
 (make-huffman-tree 
  '((a 2) (na 16) (boom 1) (sha 3) (get 2) (yip 9) (job 2) (wah 1))))

(display "=============== exercise 2.70 encode ================")
(newline)
(display hufm-tree)
(newline)
(display (encode '(get a job) hufm-tree))
(newline)
(display (encode '(sha na na na na na na na na) hufm-tree))
(newline)
(display (encode '(wah yip yip yip yip yip yip yip yip yip) hufm-tree))
(newline)
(display (encode '(sha boom) hufm-tree))
(newline)

