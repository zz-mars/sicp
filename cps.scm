(define (factCPS n r)
  (if (= n 0)
      (r 1)
      (factCPS 
       (- n 1) 
       (lambda (x) (r (* n x))))))

(factCPS 6 (lambda (x) (display x)))
