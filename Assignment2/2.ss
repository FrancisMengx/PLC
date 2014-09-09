;Francis Meng Assignment 2
;Q1a Factorial of a non negative number
(define fact 
	(lambda (n) 
		(if (= n 0) 1 
			(if (= n 1) 1 
				(* n (fact (- n 1)))
			)
		)
	)
)

;Q1b Calculate binomial coefficient
(define choose 
	(lambda (n m) 
		(/ (fact n) (* (fact m) (fact (- n m))))
	)
)

;Q2 Return integers between the first number and the second number 
(define make-range 
	(lambda (n m) 
		(if (>= n m) '() 
			(cons n (make-range (+ n 1) m))
		)
	)
)

;Q3 Determine if the given list is a set
(define set? 
	(lambda (li)
		(if (equal? li '()) #t 
			(if (equal? (cdr li) '()) #t 
				(if (member (car li) (cdr li)) #f (set? (cdr li)))
			)
		)
	)
)

;Q4 Calculate the sum of squares of all the number in the given list
(define sum-of-squares 
	(lambda (lon) 
		(if (equal? lon '()) 0 (+ (* (car lon) (car lon)) (sum-of-squares (cdr lon))))
	)
)

;Q5 Return the vector that is formed by the given points
(define make-vec-from-points 
	(lambda (p1 p2)
		(if (equal? p1 '()) '() 
			(cons (- (car p2) (car p1)) (make-vec-from-points (cdr p1) (cdr p2)))
		)
	)
)

;Q6 Calculate the dot product of two vectors
(define dot-product 
	(lambda (v1 v2) 
		(if (equal? v1 '()) 0
			(+ (* (car v1) (car v2)) (dot-product (cdr v1) (cdr v2)))
		)
	)
)

;Q7 Calculate the vector length
(define vec-length 
	(lambda (v) (inexact->exact (truncate (sqrt (sum-of-squares v)))))
)

;helper function that calculate square root of a number
(define sqrt 
	(lambda (n) (if (= n 0) 0 (exp (* 0.5 (log n))))
)
)

;Q8 Calculate the destance between two vectors
(define distance 
	(lambda (p1 p2) 
		(if (equal? p1 p2) 0 (vec-length (make-vec-from-points p1 p2)))
	)
)