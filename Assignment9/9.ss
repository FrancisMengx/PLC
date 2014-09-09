;Francis Meng Assignment 9
(define (sn-list-recur base-value sn-list-proc . sn-list-proc1)
	(letrec ([helper (lambda (ls) 
		(cond ((null? ls) base-value)
		      ((not (list? (car ls))) (sn-list-proc (car ls) (helper (cdr ls))))
		      (else (if (null? sn-list-proc1)
		          (sn-list-proc (helper (car ls)) (helper (cdr ls)))
		          ((car sn-list-proc1) (helper (car ls)) (helper (cdr ls)))))
		      ))])
	helper)
)

;Question 1.a
(define (sn-list-sum snlist)
	((sn-list-recur 0 +) snlist)
)

;Question 1.b
(define (sn-list-map proc snlist)
	((sn-list-recur '() (lambda (x y) (if (list? x)
	    (cons x y)
	    (cons (proc x) y)))) snlist
))


;Question 1.c
(define (sn-list-paren-count snlist)
  ((sn-list-recur 2 (lambda (x y) y) (lambda (x y) (+ x y))) snlist)
 )

;Question 1.d
(define (sn-list-reverse snlist)
  ((sn-list-recur '() (lambda (x y) (append y (cons x '())))) snlist))

;Question 1.e
(define (sn-list-occur s snlist)
	((sn-list-recur 0 (lambda (x y) (if (equal? s x)
	    (+ 1 y)
	    y)) +) snlist)
)

;Question 1.f
(define (sn-list-depth snlist)
	((sn-list-recur 1 (lambda (x y) y) (lambda (x y) 
		(if (> (+ 1 x) y)
	    (+ x 1)
	    y))) snlist)
)

;Question 2
(define (bt-recur base-value proc)
	(letrec ([helper (lambda (t) 
		(cond ((null? t) base-value)
		      ((not (list? t)) (proc '() t 0))
		      (else (proc (car t) (helper (cadr t)) (helper (caddr t))))
		      ))])
	helper)
)

(define (bt-sum t1)
  ((bt-recur 0 (lambda (x y z) (+ y z))) t1))

(define (bt-inorder t1)
  ((bt-recur '() (lambda (x y z) 
  	(cond
  		((null? x) x) 
  		((and (null? y) (null? z)) (cons x '()))
        ((null? z) (append y (cons x '())))
        ((null? y) (append (cons x '()) z))
        (else (append y (append (cons x '()) z)))
        ))) t1))

