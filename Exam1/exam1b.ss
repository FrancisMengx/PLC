;Francis Meng exam 1 b
;c5 
(define (rotate ls)
  (append (cdr ls) (cons (car ls) '())))

;c6
(define (compose-with-list ls-fn)
	(if (null? ls-fn)
	    (lambda (x) x)
	    (lambda (x) (compose-helper ls-fn x)))
)

;c7

(define compose
	(lambda list-of-functions 
		(if (null? list-of-functions)
		    (lambda (x) x)
		    (lambda (x) (compose-helper list-of-functions x)))
))

(define (compose-helper list-of-functions x)
  (cond 
		      [(null? (cdr list-of-functions)) ((car list-of-functions) x)]
		      [else ((car list-of-functions) (compose-helper (cdr list-of-functions) x))]))
