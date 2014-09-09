;Francis Meng Assignment 6
;Question1
(define (curry2 proc)
	(lambda (x)
		(lambda (y) (proc x y))))

;Question2 
(define (curried-compose proc1)
	(lambda (proc2) 
		(lambda (x) (proc1 (proc2 x)))))

;Question3
(define compose
	(lambda list-of-functions 
		(lambda (x) (compose-helper list-of-functions x))
		
))

(define (compose-helper list-of-functions x)
  (cond 
		      [(null? (cdr list-of-functions)) ((car list-of-functions) x)]
		      [else ((car list-of-functions) (compose-helper (cdr list-of-functions) x))]))
;Question4
(define (make-list-c n)
	(lambda (arg) (let loop ([x n])
  	(if (equal? x 0)
  	    '()
  	    (cons arg (loop (- x 1)))))
)
)
;Question5 
(define (let->application code)
	(cons (list 'lambda (get-variable-name (car (cdr code))) (car (cddr code))) (get-variable-value (car (cdr code))))
)
(define (get-variable-name list)
(if (null? list)
    '()
    (cons (car (car list)) (get-variable-name (cdr list)))))

(define (get-variable-value list)
(if (null? list)
    '()
    (cons (car (cdr (car list))) (get-variable-value (cdr list)))))

;Question 6
(define (let*->let  code)
  (let parse ([varlist (car (cdr code))])
  	(if (null? varlist)
  	    (car (cddr code))
  	    (list 'let (cons (car varlist) '()) (parse (cdr varlist))))))

;Question7
(define (filter-in op list)
(if (null? list)
    '()
    (if (op (car list))
        (cons (car list) (filter-in op (cdr list)))
        (filter-in op (cdr list))))
)

;Question8
(define (filter-out op list)
  (if (null? list)
    '()
    (if (op (car list))
        (filter-out op (cdr list))
        (cons (car list) (filter-out op (cdr list))))))

;Question9
(define (sort-list-of-symbols list)
	(map (lambda (y) (string->symbol y)) 
	 (sort string<=? (map (lambda (x) (if (not (string? x)) (symbol->string x))) list)))
)