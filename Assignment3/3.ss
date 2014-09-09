;Francis Meng Assignment 3
;Question 1 Calculate the cross product of two vectors
(define cross-product 
	(lambda (v1 v2) 
		(cons (- (* (car (cdr v1)) (car (cddr v2))) 
				(* (car (cdr v2)) (car (cddr v1)))) 
		(cons (- (* (car (cddr v1)) (car v2)) 
				(* (car (cddr v2)) (car v1))) 
		(cons (- (* (car v1) (car (cdr v2))) 
				(* (car v2) (car (cdr v1)))) '()))
		)
	)
)
;Question 2 check if two given vectors are parallel
(define parallel?
	(lambda (v1 v2) 
		(let ((factor (/ (car v2) (car v1)))) 
			(if (equal? (* factor (car (cdr v1))) (car (cdr v2)))
				(if (equal? (* factor (car (cddr v1))) (car (cddr v2))) #t #f) 
				#f
			)
		)
	)
)

;Question 3 check if three given points are collinear
(define (collinear? p1 p2 p3)
  (let ((v1 (make-vec-from-points p1 p2))
  		(v2 (make-vec-from-points p1 p3)))
  	(parallel? v1 v2))
)

(define make-vec-from-points 
	(lambda (p1 p2)
		(if (equal? p1 '()) '() 
			(cons (- (car p2) (car p1)) (make-vec-from-points (cdr p1) (cdr p2)))
		)
	)
)

;Question 4 find the nearest point in the list to the given point
(define (nearest-point p list-of-points)
  (if (equal? (cdr list-of-points) '())
      (car list-of-points)
      (if (> (distance p (car list-of-points)) (distance p (car (cdr list-of-points))))
          (nearest-point p (cdr list-of-points))
          (nearest-point p (cons (car list-of-points) (cddr list-of-points))))))

(define distance 
	(lambda (p1 p2) 
		(if (equal? p1 p2) 0 (vec-length (make-vec-from-points p1 p2)))
	)
)

(define vec-length 
	(lambda (v) (sqrt (sum-of-squares v)))
)

(define sum-of-squares 
	(lambda (lon) 
		(if (equal? lon '()) 0 (+ (* (car lon) (car lon)) (sum-of-squares (cdr lon))))
	)
)

;Question 5 find the intersection of two set
(define (intersection s1 s2)
	(if (equal? s1 '())
	    '()
		(if (member (car s1) s2)
        (cons (car s1) (intersection (cdr s1) s2))
        (intersection (cdr s1) s2)))
 )

;Question 6 determine if the first set is a subset of the second set
(define (subset? s1 s2)
  (if (equal? s1 '())
      #t
      (if (member (car s1) s2)
        (subset? (cdr s1) s2)
        #f)))

;Question 7 determine if the given set is a relation;
(define (relation? obj)
	(if (set? obj)
	    (checkRelation obj)
	    #f)
)

(define (checkRelation obj)
	(if (equal? obj '())
	    #t
	    (if (list? (car obj))
	        (if (equal? (length (car obj)) 2)
	        	(checkRelation (cdr obj))
	            #f)
	        #f)
	)
)

(define set? 
	(lambda (li)
		(if (list? li)
		    (if (equal? li '()) #t 
			(if (equal? (cdr li) '()) #t 
				(if (member (car li) (cdr li)) #f (set? (cdr li)))
			)
		)
		    #f)
		
	)
)

;Question 8 determine the domain of the given relation
(define (domain r)
	(if (equal? r '())
	    '()
	    (if (member (car (car r)) (domain (cdr r)))
	        (domain (cdr r))
	        (cons (car (car r)) (domain (cdr r)))))
)

