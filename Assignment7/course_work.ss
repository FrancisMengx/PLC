(define list-sum
	(letrec ([helper (lambda (ls) (if (null? ls)
	    0
	    (+ (car ls) (helper (cdr ls)))))]) helper
)
)

(define (list-recur base-value list-proc)
(letrec ([helper (lambda (ls) (if (null? ls)
    base-value
    (list-proc (car ls) (helper (cdr ls)))))]) helper))