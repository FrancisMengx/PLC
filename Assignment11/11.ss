;Francis Meng Assignment 11
;Question1
(define (lexical-address e)
	(let get-lexi ([expr e] [var-list '()] [cur-depth (- 0 1)])
		(cond ((not (list? expr)) (if (equal? 'free (var-list-index var-list expr))
		    (list ': 'free expr)
		    (list (car (var-list-index var-list expr)) (- cur-depth (cadr (var-list-index var-list expr))) (caddr (var-list-index var-list expr)))))
	      ((null? (cddr expr)) (list (get-lexi (car expr) var-list cur-depth) (get-lexi (cadr expr) var-list cur-depth)))
	      ((equal? (car expr) 'lambda) (list 'lambda (cadr expr) (get-lexi (caddr expr) (append var-list (list (cadr expr))) (+ cur-depth 1))))
	      ((equal? (car expr) 'if) (list 'if (get-lexi (cadr expr) var-list cur-depth) (get-lexi (caddr expr) var-list cur-depth) (get-lexi (cadddr expr) var-list cur-depth)))
	      (else (map (lambda (x) (get-lexi x var-list cur-depth)) expr))
	))
)

(define (var-list-index ls var)
	(let get-list-index ([cur-ls-index 0] [cur-list ls])
		(if (null? cur-list)
	    	'free
	    	(if (null? (get-index (car cur-list) var))
	    	    (get-list-index (+ cur-ls-index 1) (cdr cur-list))
	    	    (if (equal? 'free (get-list-index (+ cur-ls-index 1) (cdr cur-list)))
	    	        (list ': cur-ls-index (get-index (car cur-list) var))
	    	        (get-list-index (+ cur-ls-index 1) (cdr cur-list))))
	    ))
)

(define (get-index ls var)
	(let indexing ([index 0] [cur-list ls])
		(if (null? cur-list)
		    '()
		    (if (equal? (car cur-list) var)
		        index
		        (indexing (+ index 1) (cdr cur-list)))))
)

;Question2 
(define (un-lexical-address e)
	(let delexi ([expr e] [var-list '()] [cur-depth (- 0 1)])
		(cond ((equal? ': (car expr)) (if (equal? (cadr expr) 'free)
		    (caddr expr)
		    (get-var var-list (- cur-depth (cadr expr)) (caddr expr))))
		    ((equal? (car expr) 'lambda) (list 'lambda (cadr expr) (delexi (caddr expr) (append var-list (list (cadr expr))) (+ cur-depth 1))))
			((equal? (car expr) 'if) (list 'if (delexi (cadr expr) var-list cur-depth) (delexi (caddr expr) var-list cur-depth) (delexi (cadddr expr) var-list cur-depth)))
			(else (map (lambda (x) (delexi x var-list cur-depth)) expr))))
)			

(define (get-var ls ls-index var-index)
	(if (zero? ls-index)
	    (get-list-var (car ls) var-index)
	    (get-var (cdr ls) (- ls-index 1) var-index))
)

(define (get-list-var ls var-index)
	(if (zero? var-index)
	    (car ls)
	    (get-list-var (cdr ls) (- var-index 1)))
)

;Question 3
(define-syntax my-let
	(syntax-rules ()
		[(_ ((x v) ...) e1 e2 ...)		((lambda (x ...) e1 e2 ...) v ...)]
		[(_ name ((x v) ...) e1 e2 ...)	(letrec ((name (lambda (x ...) e1 e2 ...))) (name v ...))]
	)
)

(define-syntax my-or
	(syntax-rules ()
		[(_) #f]
		[(_ e1) e1]
		[(_ e1 e2 ...) (let ((t e1))
       (if t t (my-or e2 ...)))]
	)
)

(define-syntax +=
	(syntax-rules ()
		[(_ e1 e2) (begin (set! e1 (+ e1 e2)) e1)]
	)
)

(define-syntax return-first
	(syntax-rules ()
		[(_ e1) e1]
		[(_ e1 e2 ...) (let ([r e1])
			e2 ... r)]
	)
)

