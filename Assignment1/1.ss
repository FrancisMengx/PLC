(define Fahrenheit->Celsius 
	(lambda (temp) 
		(* (- temp 32) 5/9)))

(define interval-contains? 
	(lambda (interval number) 
		(and (>= number (car interval)) (<= number (car (cdr interval))))))

(define interval-intersects? 
	(lambda (i1 i2) 
		(if (>= (car i1) (car i2)) 
			(if (>= (car (cdr i2)) (car i1)) #t #f)
			(if (<= (car i2) (car (cdr i1))) #t #f)
		)
	)
)

(define interval-union 
	(lambda (i1 i2) 
		(if (>= (car i2) (car i1)) 
			(if (<= (car i2) (car (cdr i1))) 
				(if (>= (car (cdr i2)) (car (cdr i1))) 
					(cons (cons (car i1) (cons (car (cdr i2)) '())) '()) 
					(cons i1 '())
				)
				(cons i1 (cons i2 '()))
			) 
			(if (< (car (cdr i2)) (car i1)) 
				(cons i2 (cons i1 '())) 
				(if (< (car (cdr i2)) (car (cdr i1)))
					(cons (cons (car i2) (cons (car (cdr i1)) '())) '()) 
					(cons i2 '())
				)
			)
		)
	)
)


(define divisible-by-7? 
	(lambda (n) 
		(if (= 0 (modulo  n 7)) #t #f)
	)
)

(define divisible-by-10? 
	(lambda (n) 
		(if (= 0 (modulo n 10)) #t #f)
	)
)

(define ends-with-7? 
	(lambda (n) 
		(divisible-by-10? (- n 7))
	)
)


