(define (filtered-accumulator-generator combiner base-value)
 (lambda (f a next b the-filter)
  (define (iter k r)
   (cond ((> to k) r)
	((the-filter k) (iter (next k) (combiner r (f k))))
	(else (iter (next k) r))))
  (iter from base-value)))

