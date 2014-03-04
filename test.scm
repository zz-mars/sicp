(define true (= 0 0))
(define false (= 1 0))

(define (memq item x)
 (cond ((null? x) false)
  ((eq? item (car x)) x)
  (else (memq item (cdr x)))))

(display (memq 'apple '(pear banana apple orange)))
(newline)

(define (equal? la lb)
 (cond ((and (null? la) (null? lb)) true)
  ((and (not (null? la)) (not (null? lb)))
   (let ((la-a (car la))
	   (lb-a (car lb)))
	(and ((if (pair? la-a) equal? eq?) la-a lb-a) 
	 (equal? (cdr la) (cdr lb)))))
  (else false)))
   
(display (if (equal? '(this is a list) '(this is a list)) 
		  "equal" "not equal"))
(newline)

(display (if (equal? '(this is a list) '(this '(is a) list)) 
		  "equal" "not equal"))
(newline)

