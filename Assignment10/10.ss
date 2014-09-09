;Francis Meng Assignment 10
;Question 1;
(define (make-slist-leaf-iterator list)
	(let ([stack (make-stack)]) (and (stack 'push list)
		(lambda () (if (stack 'empty?)
		    #f
		    (let pop-stack ([top (stack 'pop)])
			(if (null? top) 
			    (if (stack 'empty?)
			        #f
			        (pop-stack (stack 'pop)))
			    (if (list? (car top))
			    (if (null? (car top))
			        (pop-stack (cdr top))
			        (and (stack 'push (cdr top)) (pop-stack (car top))))
			    (and (stack 'push (cdr top)) (car top))))
			)))))
)

(define make-stack
 (lambda ()
  (let ([stk '()])
   (lambda (msg  . args ) 
    (case msg
      [(empty?) (null? stk)]
      [(push)   (set! stk (cons (car args) stk))]
      [(pop)    (let ([top (car stk)])
                   (set! stk (cdr stk))
                   top)]
      [else (errorf 'stack "illegal message to stack object: ~a" msg)])))))

;Question 2;
(define (free-vars e)
	(if (list? e)
	    (cond ((null? (cdr e)) e)
	          ((null? (cddr e)) (union (free-vars (car e)) (free-vars (cadr e))))
	          (else (not-in (cadr e) (free-vars (caddr e))))
	    )
	    (cons e '()))
)

(define (bound-vars e)
	(if (list? e)
	    (cond ((null? (cdr e)) '())
	          ((null? (cddr e)) (union (bound-vars (car e)) (bound-vars (cadr e))))
	          (else (if (list? (caddr e))
	              (union (intersect (cadr e) (bound-vars (caddr e))) (intersect (cadr e) (free-vars (caddr e))))
	              (intersect (cadr e) (cddr e))))
	    )
	    '())
)

(define (union s1 s2)
        (if (equal? s2 '())
            s1
            (if (member (car s2) s1)
              (union s1 (cdr s2))
              (cons (car s2) (union s1 (cdr s2)))))
)

(define (not-in s1 s2)
  (let find-not-in ([result '()] [cur-list s2])
  	(if (null? cur-list)
  	    result
  	    (if (member (car cur-list) s1)
  	    (find-not-in result (cdr cur-list))
  	    (find-not-in (cons (car cur-list) result) (cdr cur-list))))))

(define (intersect s1 s2)
  (let find-intersection ([result '()] [cur-list s1])
  	(if (null? cur-list)
  	    result
  	    (if (member (car cur-list) s2)
  	        (find-intersection (cons (car cur-list) result) (cdr cur-list))
  	        (find-intersection result (cdr cur-list))))))

;Question 3

(define occurs-free?
  (lambda (var exp)
  	(if (equal? var 'set!)
  	    #f
  	    (cond
      ((symbol? exp) (eqv? var exp))
      ((null? (cdr exp)) #t)
      ((eqv? (car exp) 'lambda) 
       (and (not (member var (cadr exp)))
            (occurs-free? var (caddr exp))))
      ((eqv? (car exp) 'if) (or (occurs-free? var (cadr exp)) (occurs-free? var (caddr exp))))
      ((eqv? (car exp) 'let) (or (not (is-list-car var (cadr exp))) (occurs-free? var (caddr exp))))
      ((eqv? (car exp) 'let*) (and (not (is-list-car var (cadr exp))) (or (not (show-up var (caddr exp))) (occurs-free? var (caddr exp)))))
      (else (or (occurs-free? var  (car exp))
                (occurs-free? var (cadr exp))))))
    ))

(define occurs-bound?
  (lambda (var exp)
  	(if (equal? var 'set!)
  	    #f
  	    (cond
      ((symbol? exp) #f)
      ((null? (cdr exp)) #f)
      ((eqv? (car exp) 'lambda)
      (or (occurs-bound? var (caddr exp))
           (and (member var (cadr exp))
                (occurs-free? var (caddr exp)))))
      ((eqv? (car exp) 'if) (or (occurs-bound? var (cadr exp)) (occurs-bound? var (caddr exp))))
      ((eqv? (car exp) 'let) (or (is-list-car var (cadr exp)) (occurs-bound? var (caddr exp))))
      ((eqv? (car exp) 'let*) (or (and (is-list-car var (cadr exp)) (is-list-cadr var (cadr exp)))
      (and (is-list-car var (cadr exp)) 
      (or (show-up var (caddr exp)) (occurs-bound? var (caddr exp))))))
      (else (or (occurs-bound? var  (car exp))
                (occurs-bound? var (cadr exp))))))
    ))

(define (show-up var ls)
  (if (null? ls)
      #f
      (if (symbol? ls)
          (if (equal? var ls)
              #t
              #f)
          (if (list? (car ls))
          (or (show-up var (car ls)) (show-up var (cdr ls)))
          (show-up var (cdr ls))))))

(define (is-list-car var ls)
	(if (null? ls)
	    #f
	    (if (equal? var (car (car ls)))
	        #t
	        (is-list-car var (cdr ls))))
)
(define (is-list-cadr var ls)
	(if (null? ls)
	    #f
	    (if (equal? var (cadr (car ls)))
	        #t
	        (is-list-cadr var (cdr ls))))
)
