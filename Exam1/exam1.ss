;Francis Meng Exam 1 a
;c1
(define (member-n? sym n ls)
(cond ((zero? n) #t)
	((null? ls) #f)
	((equal? (car ls) sym) (member-n? sym (- n 1) (cdr ls)))
	(else (member-n? sym n (cdr ls)))
  )
)

;c2
(define (opposites-attract ls)
	(let cons-opp-list ([cur ls] [len (- (length ls) 1)]  [result '()] [accum 0])
		(if (null? cur)
		    result
		    (cons-opp-list (cdr cur) len (append result (cons (list (list-ref ls accum) (list-ref ls (- len accum))) '())) (+ accum 1))
	)
))

;c3

(define (symmetric? rel)
	(let check-sym ([cur rel])
		(if (null? cur)
			    #t
			(if (member (list (cadr (car cur)) (car(car cur))) rel)
			    (check-sym (cdr cur))
			    #f))	
	)
)

;c4
(define (lower-triangular? m)
	(let check-nz ([nz (non-zero-m m)] [curnz (list 0)] [cur-last 0])
		(if (null? nz)
		    #t
		    (if (equal? (car nz) curnz)
		        (check-nz (cdr nz) (append curnz (cons (+ cur-last 1) '())) (+ cur-last 1))
		        #f)))
)

(define (non-zero-m m)
  (if (null? m)
      '()
      (cons (non-zero-list (car m) 0) (non-zero-m (cdr m)))))

(define (non-zero-list ls accum)
	(if (null? ls)
	    '()
	    (if (zero? (car ls))
	        (non-zero-list (cdr ls) (+ accum 1))
	        (cons accum (non-zero-list (cdr ls) (+ accum 1)))))
)





