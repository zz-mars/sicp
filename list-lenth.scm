(define (list-length lst)
 (define (iter llst r)
  (if (null? llst) r
   (iter (cdr llst) (+ r 1))))
 (iter lst 0))

(define l (list 1 2 3 4 5 6 7 8 9))

(display (list-length l))
(newline)

