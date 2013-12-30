(call/cc (lambda (k)
		  (display "before call k")
		  (k "i'm back")
		  (display "after call k")))

(display "i'm the point of control flow k ")
