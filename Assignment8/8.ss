;Francis Meng Assignment 8
;Question1.a
(define (slist-map proc slist)
  (if (null? slist)
      '()
      (if (list? (car slist))
          (cons (slist-map proc (car slist)) (slist-map proc (cdr slist)))
          (cons (proc (car slist)) (slist-map proc (cdr slist))))))

;Question1.b
(define (slist-reverse ls)
	(let reversels ([rls (reverse ls)])
		(if (null? rls)
		    '()
		    (if (list? (car rls))
		        (cons (slist-reverse (car rls)) (reversels (cdr rls)))
		        (cons (car rls) (reversels (cdr rls))))))
)

;Question1.c
(define (slist-paren-count slist)
  (if (null? slist)
      2
      (if (list? (car slist))
          (+ (slist-paren-count (car slist)) (slist-paren-count (cdr slist)))
          (slist-paren-count (cdr slist)))))

;Question1.d
(define (slist-depth slist)
	(if (null? slist)
	    1
	    (let getDepth ([curlist slist])
	    	(if (null? curlist)
	    	    0
	    	    (if (and (null? (car curlist)) (null? (cdr curlist)))
	    	        2
	    	        (+ 1 (getDepth (concatenate curlist)))))
))
)

(define (concatenate slist)
	(if (null? slist)
	    '()
	    (let concat ([result '()] [curlist slist])
	    	(if (null? curlist)
	    	    result
	    	    (if (list? (car curlist))
	    	    (concat (append result (car curlist)) (cdr curlist))
	    	    (concat result (cdr curlist))))))
)

;Question1.e
(define (slist-symbols-at-depth slist d)
  (if (equal? d 1)
      (get-current-level slist)
      (slist-symbols-at-depth (concatenate slist) (- d 1))))

(define (get-current-level slist)
	(let curlevel ([result '()] [curlist slist])
		(if (null? curlist)
		    result
		    (if (list? (car curlist))
		        (curlevel result (cdr curlist))
		        (curlevel (append result (cons (car curlist) '())) (cdr curlist)))))
)

;Question2
(define (subst-leftmost new old slist equality-pred?)
	(if (null? slist)
	    '()
	    (if (list? (car slist))
	        (let ([cdr-sub (subst-leftmost new old (car slist) equality-pred?)])
	        	(if (null? cdr-sub)
	            (cons (car slist) (subst-leftmost new old (cdr slist) equality-pred?))
	            (cons cdr-sub (cdr slist))))
	        (if (equality-pred? (car slist) old)
	            (cons new (cdr slist))
	            (cons (car slist) (subst-leftmost new old (cdr slist) equality-pred?)))))
)

;Question3.1
(define (bt-leaf-sum t)
	(cond ((not (list? t)) t)
		  ((null? t) 0)
	      ((and (null? (cadr t)) (null? (caddr t))) (car t))
	      (else (+ (bt-leaf-sum (cadr t)) (bt-leaf-sum (caddr t)))
	)
))

;Question3.2
(define (bt-inorder-list t)
	(if (or (null? t) (number? t))
	    '()
	    (if (and (and (null? (cadr t)) (null? (caddr t))) (not (or (list? (cadr t)) (null? (caddr t)))))
	    (cons (car t) '())
	    (append (bt-inorder-list (cadr t)) (append (cons (car t) '()) (bt-inorder-list (caddr t))))
	    ))
	
)

;Question3.3
(define (bt-max t)
	(let getnmlist ([curT t])
		(if (number? curT)
		    curT
		    (max (getnmlist (cadr curT)) (getnmlist (caddr curT))))
	)
)
;Question3.4
(define (bt-max-interior t)
	(car (max-helper t))
)

(define (max-helper t)
  	(cond ((and (number? (cadr t)) (number? (caddr t))) (cons (car t) (cons (+ (cadr t) (caddr t)) '())))
	    ((number? (cadr t)) (if (positive? (cadr t))
	        (list (car t) (+ (cadr t) (cadr (max-helper (caddr t)))))
	        (max-helper (caddr t))))
	    ((number? (caddr t)) (if (positive? (caddr t))
	        (list (car t) (+ (caddr t) (cadr (max-helper (cadr t)))))
	        (max-helper (cadr t))))
	    (else (cond ((and (positive? (cadr (max-helper (cadr t)))) (positive? (cadr (max-helper (caddr t))))) 
	    (if (equal? (car  (cadr t)) (car (max-helper (cadr t))))
	        (list (car t) (+ (cadr (max-helper (cadr t))) (cadr (max-helper (caddr t)))))
	        (max-helper (cadr t))))
	          ((positive? (cadr (max-helper (cadr t)))) (max-helper (cadr t)))
	    	  ((positive? (cadr (max-helper (caddr t)))) (max-helper (caddr t)))
	    	  (else (if (> (cadr (max-helper (cadr t))) (cadr (max-helper (caddr t))))
	    	      (max-helper (cadr t))
	    	      (max-helper (caddr t))))))
	)
)


