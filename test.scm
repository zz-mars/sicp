(define (make-accumulator init-val)
 (lambda (inc-val)
  (begin (set! init-val (+ init-val inc-val))
   init-val)))

(define A (make-accumulator 5))
(display (A 10))
(newline)
(display (A 10))
(newline)

(define (make-monitored f)
 (let ((counter 0))
  (lambda (input)
   (if (eq? input 'how-many-calls?) counter
	(begin (set! counter (+ counter 1)) (f input))))))

(define s (make-monitored sqrt))
(display (s 100))
(newline)
(display (s 120))
(newline)
(display (s 'how-many-calls?))
(newline)

;(define (make-account balance pswd)
; (define (withdraw money)
;  (if (< balance money)
;   (display "in-sufficient money!")
;   (begin (set! balance (- balance money))
;	balance)))
; (define (diposite money)
;  (begin (set! balance (+ balance money))
;   balance))
; (lambda (psswd op)
;  (if (eq? psswd pswd)
;   (cond ((eq? op 'withdraw) withdraw)
;	((eq? op 'deposite) diposite)
;	(else (display "undefined operation!")))
;   (display "incorrect password!"))))
;
;(define acc (make-account 100 'zz))
;(display ((acc 'zz 'withdraw) 10))
;(newline)
;(display ((acc 'zz 'deposite) 20))
;(newline)
;((acc 'zzz 'deposite) 20)
;(newline)

(define (make-account balance pswd)
 (define (withdraw money)
  (if (< balance money)
   (display "in-sufficient money!")
   (begin (set! balance (- balance money))
	balance)))
 (define (diposite money)
  (begin (set! balance (+ balance money))
   balance))
 (let ((pwsdc 0))
  (lambda (psswd op)
   (if (eq? psswd pswd)
	(begin (set! pwsdc 0)
	 (cond ((eq? op 'withdraw) withdraw)
	  ((eq? op 'deposite) diposite)
	  (else (display "undefined operation!"))))
   (begin (set! pwsdc (+ pwsdc 1))
	(display pwsdc)
	(newline)
	(if (> pwsdc 3)
	 (display "call the cop!")
	 (display "incorrect password!")))))))

(define acc (make-account 100 'zz))
(display ((acc 'zz 'withdraw) 10))
(newline)
(display ((acc 'zz 'deposite) 20))
(newline)
(acc 'zzz 'deposite)
(newline)
(acc 'zzz 'deposite)
(newline)
(acc 'zzz 'deposite)
(newline)
(acc 'zzz 'deposite)
(newline)
(acc 'zzz 'deposite)
(newline)

