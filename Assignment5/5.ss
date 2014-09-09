;Francis Meng Assignment 4
;Question 1 Return a minimized interval of the given set of intervals
(define (minimize-interval-list ls)
	(if (or (equal? ls '()) (equal? (cdr ls) '()))
	    ls
	    (get-min-set '() ls))
)

(define (get-min-set curMinSet li)
	(if (equal? li '())
	    curMinSet
	    (get-min-set (get-single-min-set curMinSet (car li)) (cdr li)))
)

(define (get-single-min-set curMinSet li)
	(if (equal? curMinSet '())
	    (cons li '())
	    (if (equal? (cdr (minimize-two-intervals (car curMinSet) li)) '())
	        (cons (car (minimize-two-intervals (car curMinSet) li)) (cdr curMinSet))
	        (cons (car curMinSet) (get-single-min-set (cdr curMinSet) li))))
)

(define (minimize-two-intervals ls1 ls2)
	(if (< (car ls1) (car ls2))
    	(if (< (car (cdr ls1)) (car ls2))
        	(cons ls1 (cons ls2 '()))
        	(if (< (car (cdr ls1)) (car (cdr ls2)))
        	    (cons (cons (car ls1) (cdr ls2)) '())
        	    (cons ls1 '())))
    	(if (< (car (cdr ls2)) (car ls1))
        	(cons ls2 (cons ls1 '()))
        	(if (< (car (cdr ls2)) (car (cdr ls1)))
        	    (cons (cons (car ls2) (cdr ls1)) '())
        	    (cons ls2 '())))))

;Question 2 Check if there is at list one element in the list is applicable to the pred
(define (exists? pred li)
  (if (equal? li '())
      #f
      (if (pred (car li))
          #t
          (exists pred (cdr li)))))

;Question 3 Give the first index of the element that satisfy the pred
(define (list-index pred li)
	(if (equal? li '())
	    #f
	    (if (pred (car li))
	        '0
	        (if (number? (list-index pred (cdr li)))
	            (+ 1 (list-index pred (cdr li)))
	            #f)))
)

;Question 4 return the pascal-triangle with given layer number
(define (pascal-triangle n)
	(if (< n 0)
	    '()
	    (cons (getButtomLevel n n) (pascal-triangle (- n 1))))
)

(define (getButtomLevel n m)
	(if (equal? m 0)
	    (cons 1 '())
	    (cons (choose n m) (getButtomLevel n (- m 1))))
)

(define choose 
	(lambda (n m) 
		(/ (fact n) (* (fact m) (fact (- n m))))
	)
)

(define fact 
	(lambda (n) 
		(if (= n 0) 1 
			(if (= n 1) 1 
				(* n (fact (- n 1)))
			)
		)
	)
)

;Question 5 return the cartesian product of two sets
(define (product set1 set2) 
	(if (equal? set1 '())
	    '()
	    (union (map (lambda (x) (cons (car set1) (cons x '()))) set2) (product (cdr set1) set2))) 
	
)

(define (union s1 s2)
	(if (equal? s2 '())
	    s1
	    (if (member (car s2) s1)
	      (union s1 (cdr s2))
	      (cons (car s2) (union s1 (cdr s2)))))
)

;Question 6 return the number of edges that given vertices can form
(define (max-edges n)
  (if (< n 2)
      0
      (choose n 2)))

;Question 7 check if the given graph is complete
(define (complete? g)
	(if (equal? g '())
	    #t
		(if (checkComplete (domain g) (car g))
		 	(complete? (cdr g))
		 	#f))
)

(define (checkComplete dom g)
	(if (equal? dom '())
	    #t
	    (if (member (car dom) (car (cdr g)))
	        (checkComplete (cdr dom) g)
	        (if (equal? (car dom) (car g))
	            (checkComplete (cdr dom) g)
	            #f)))
)	

(define (domain r)
	(if (equal? r '())
	    '()
	    (if (member (car (car r)) (domain (cdr r)))
	        (domain (cdr r))
	        (cons (car (car r)) (domain (cdr r)))))
)

;Question 8 construct complete graph with given vertices;
(define (complete ls)
	(if (equal? ls '())
	    '()
	    (constructGraph ls ls))
)

(define (constructGraph curPos ls)
	(if (equal? curPos '())
	    '()
	    (cons (cons (car curPos) 
	    	(cons (constructDestiList (car curPos) ls) '())) (constructGraph (cdr curPos) ls)))
)

(define (constructDestiList curPos ls)
  (if (equal? ls '())
      '()
      (if (equal? curPos (car ls))
          (constructDestiList curPos (cdr ls))
          (cons (car ls) (constructDestiList curPos (cdr ls))))))

;Question 9 replace the first given data with the second given data in the given list
(define (replace old new ls)
(if (equal? ls '())
    '()
    (if (equal? (car ls) old)
        (cons new (replace old new (cdr ls)))
        (cons (car ls) (replace old new (cdr ls)))))
)

;Question 10 delete first appeared element in the list
(define (remove-first element ls)
	(if (equal? ls '())
	    '()
	    (if (equal? element (car ls))
	        (cdr ls)
	        (cons (car ls) (remove-first element (cdr ls)))))
)

;Question 11 delete last appeared element in the list
(define (remove-last element ls)
 (if (equal? ls '())
        '()
      	(if (equal? element (car ls))
      	    (if (last? element (cdr ls))
      	        (cdr ls)
      	        (cons (car ls) (remove-last element (cdr ls))))
      	    (cons (car ls) (remove-last element (cdr ls))))))

(define (last? element ls)
  (if (equal? ls '())
      #t
      (if (equal? element (car ls))
          #f
          (last? element (cdr ls)))))

