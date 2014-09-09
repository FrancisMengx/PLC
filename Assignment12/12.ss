;Francis Meng Assignment 12
;Question 1

(load "chez-init.ss")

(define-datatype bintree bintree?
  (leaf-node
   (datum number?))
  (interior-node
   (key symbol?)
   (left-tree bintree?)
   (right-tree bintree?)))
   

(define BASE 100)
(define (zero)
  '())

(define (succ num)
	(cond ((null? num) '(1))
	      ((>= (+ (car num) 1) BASE) (cons 0 (succ (cdr num))))
		  (else (cons (+ (car num) 1) (cdr num))))
)

(define (pred num)
	(cond
		((zero? (car num)) (cons (- BASE 1) (pred (cdr num))))
		((and (zero? (- (car num) 1)) (null? (cdr num))) '())
		(else (cons (- (car num) 1) (cdr num)))
	)
)

(define (int->bignum integer)
	(if (zero? integer)
	    '()
	    (cons (remainder integer BASE) (int->bignum (quotient integer BASE))))
)

(define (bignum->int bignum)
	(if (null? bignum)
	    0
	    (+(car bignum) (* BASE (bignum->int (cdr bignum)))))
)

(define (plus x y)
  (if (is-zero? x)
			y
			(succ (plus (pred x) y))))

(define (is-zero? num)
  (if (null? num)
      #t
      #f))

(define (multiply x y)
	(if (is-zero? y)
	    '()
	    (plus x (multiply x (pred y))))
)

(define (factorial num)
  (if (is-zero? num)
      '(1)
      (multiply num (factorial (pred num)))))

;Question 3
(define dt?
	(lambda (obj)
		(cond 	[(eqv? (car obj) 'one)	#t]
				[(eqv? (car obj) 'diff)	(if (null? (cadr obj)) #f
										(if (null? (caddr obj)) #f
										(and (null? (cdddr obj)) (dt? (cadr obj)) (dt? (caddr obj)))))]
				[else 					#f])))

(define dt-negate
	(lambda (dt)
		(list 'diff (list 'diff '(one) dt) '(one))))

(define dt+
	(lambda (dt1 dt2)
		(list 'diff (list 'diff dt1 (list 'diff '(one) dt2)) (dt-negate '(one)))))

(define dt-
	(lambda (dt1 dt2)
		(list 'diff dt1 dt2)))

(define dt=
	(lambda (dt1 dt2)
		(= (dt->integer dt1) (dt->integer dt2))))

(define dt->integer
	(lambda (dt)
		(cond 	[(null? dt) 			0]
				[(eqv? (car dt) 'one)	1]
				[(eqv? (car dt) 'diff)	(- (dt->integer (cadr dt)) (dt->integer (caddr dt)))])))

(define integer->dt
	(lambda (n)
		(define helper
			(lambda (i dt)
				(if (= 0 i) dt
					(helper (- i 1) (dt+ dt '(one))))))
		(if (> 0 n) (dt-negate (helper (- 0 n) '(diff (one) (one))))
					(helper n '(diff (one) (one))))))

;Question 4
(define (bintree-to-list tree)
  (cases bintree tree
  	[leaf-node (datum) (list 'leaf-node datum)]
  	[interior-node (key left right) 
  		(list 'interior-node key (bintree-to-list left) (bintree-to-list right))
  	]
  )
)

;Question 5
(define (max-interior tree)
	(car (max-helper tree))
)

(define (max-helper tree)
	(cases bintree tree
		[leaf-node (datum) (list 'leaf datum)]
		[interior-node (key left right)
			(let ([left-max (max-helper left)] [right-max (max-helper right)])
				(cond ((and (equal? 'leaf (car left-max)) (equal? 'leaf (car right-max))) (list key (+ (cadr left-max) (cadr right-max)))) 
					((equal? 'leaf (car left-max)) (if (positive? (cadr left-max))
	        			(list key (+ (cadr left-max) (cadr right-max)))
	        			right-max))
					((equal? 'leaf (car right-max)) (if (positive? (cadr right-max))
	        			(list key (+ (cadr right-max) (cadr left-max)))
	        			left-max))
					(else (cond ((and (positive? (cadr left-max)) (positive? (cadr right-max))) 
	        	(if ( and (equal? (cadr right) (car right-max)) (equal? (cadr left) (car left-max)))
	        	    (list key (+ (cadr left-max) (cadr right-max)))
	        (if (> (cadr left-max) (cadr right-max))
	    	      left-max
	    	      right-max)))
	          ((positive? (cadr left-max)) left-max)
	    	  ((positive? (cadr right-max)) right-max)
	    	  (else (if (> (cadr left-max) (cadr right-max))
	    	      left-max
	    	      right-max))))
				)
			)
		]
	)
)



