;Francis Meng Assignment 14

;Question 2 writing part:
;The difference between the fib-memo we wrote in class is that we store the value calculated at every step. 
;For example, when we are calculating fib 5 we will store fib 1-4 along the way. However the memoized code 
;that we wrote for this assignment will only store the value for the entire call. For example, if we have 
;already calculated fib 5 before, it will just take the time to look up the hash table. However, if we have 
;not calculated the value, it won't save any time during the calculations as lower fib values won't be stored
;when calculated for higher fib values. So it won't save as much time, as the fib-memo we wrote in class.

;Question 1.a
(define (member?-cps item list c)
  (if (null? list)
      (c #f)
      (member?-cps item (cdr list) 
      	(lambda (x) 
      		(if (equal? item (car list))
      			(c #t)
      		(c x))))))

;Question 1.b
(define set?-cps 
	(lambda (ls c) 
		(cond 
			[(null? ls) (c #t)] 
			[(not (pair? ls)) (c #f)]  
			[else (set?-cps (cdr ls) 
				(lambda (x) 
					(member?-cps (car ls) (cdr ls)
						(lambda (memq)
							(c (if memq
							    #f
							    x))
						)
					)
				))])
	)
) 

;Question 1.c
(define (intersection-cps ls1 ls2 c)
	(cond ((null? ls1) (c '()))
	      (else (intersection-cps (cdr ls1) ls2 
	      (lambda (x) 
	      (member?-cps (car ls1) ls2 
	      (lambda (y) 
	      	(c (if y
	          (cons (car ls1) x)
	          x))))))))
)

;Question 1.d
(define (make-cps pred)
	(lambda (x c)
		(c (pred x))
	)
)

;Question 1.e
(define (andmap-cps pred-cps ls c)
	(cond ((null? ls) (pred-cps ls c))
	      ((null? (cdr ls)) (pred-cps (car ls) c))
	      (else (pred-cps (car ls) (lambda (y)
	      	(if y
	      	    (andmap-cps pred-cps (cdr ls) c)
	      	    (c #f))
	      )))
	)
)

;Question 1.f
(define (matrix?-cps m c)
	(list?-cps m 
		(lambda (v) 
			(if (not v)
			    (c #f)
			    (if (or (null? m) (null? (car m)))
			        (c #f)
			        (if (null? (cdr m))
			            (c #t)
			            (andmap-cps list?-cps m 
			        	(lambda (v) 
			        		(if (not v)
			        		    (c #f)
			        		    (andmap-cps (make-cps 
			        		    	(lambda (ls) 
			        		    		(length-cps ls (lambda (v) 
			        		    			(length-cps (car m) (lambda (k) (= k v)))
			        		    		))
			        		    	)) 
			        			(cdr m) c)
			        		)
			        	)
			        )
			        )
			    )
			)
		)
	)
)

(define (list?-cps ls c)
	(cond ((null? ls) (c #t))
	      ((pair? ls) (list?-cps (cdr ls) c))
	      (else (c #f))
	)
)

(define (length-cps ls c)
	(cond ((null? ls) (c 0))
	      (else (length-cps (cdr ls) (lambda (v) (c (+ 1 v))))))
)

;Question 2
(define (memoize pred hash equiv?)
	(let ((calced (make-hashtable hash equiv?)))
		(lambda v 
			(if (hashtable-contains? calced v)
			    (hashtable-ref calced v #f)
			    (let ((result (apply pred v)))
			    	(hashtable-set! calced v result)
			    	result
			    ))
		)
	)
)


;Question 3
(define subst-leftmost
	(letrec ((helper (lambda (new old slist proc)
		(cond 
			((null? slist) (values '() #f))
			((symbol? (car slist))
				(if (proc (car slist) old)
					(values (cons new (cdr slist)) #t)
					(let ((rec-call (call-with-values (lambda () (helper new old (cdr slist) proc)) list)))
						(values (cons (car slist) (car rec-call)) (cadr rec-call)))))
			(else (let ((sub (call-with-values (lambda () (helper new old (car slist) proc)) list)))
				(if (not (cadr sub))
					(let ((rec-call (call-with-values (lambda () (helper new old (cdr slist) proc)) list)))
					(values (cons (car slist) (car rec-call)) (cadr rec-call)))
					(values (cons (car sub) (cdr slist)) #t))))))))
		(lambda (new old slist proc)
			(car (call-with-values (lambda () (helper new old slist proc)) list)))
	)
)




(define (flatten ls)
	(apply append (map 
		(lambda (el) 
			(if (list? el)
		    (flatten el)
		    (list el)))
	ls)
	)
)



