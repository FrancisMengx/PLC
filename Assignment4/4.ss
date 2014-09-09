;Francis Meng Assignment 4
;Question1 Get the union of the two given sets
(define (union s1 s2)
	(if (equal? s2 '())
	    s1
	    (if (member (car s2) s1)
	      (union s1 (cdr s2))
	      (cons (car s2) (union s1 (cdr s2)))))
)

;Question2 Determine if a relation is a reflexive relation
(define (reflexive? r)
	(isReflexive? r r)
)

(define (isReflexive? r curPos)
	(if (equal? curPos '())
	    #t
	    (if (and (member (cons (car (car curPos)) (cons (car (car curPos)) '())) r)
	    		(member (cons (car (cdr (car curPos))) (cons (car (cdr (car curPos))) '())) r))
	        (isReflexive? r (cdr curPos))
	        #f))
)

;Question3 Return the number with the given row and col with matrix
(define (matrix-ref m row col)
  (if (equal? row 0)
      (if (equal? col 0)
          (car (car m))
          (matrix-ref (cons (cdr (car m)) '()) 0 (- col 1)))
      (matrix-ref (cdr m) (- row 1) col)))

;Question4 check if given object is a matrix
(define (matrix? obj)
	(if (equal? obj '())
	    #f
	    (if (list? obj)
	    	(if (equal? (car obj) '())
	    	    #f
	    	    (if (equal? (cdr obj) '())
  		    		#t
  		    		(if (and (list? (car obj)) (list? (car (cdr obj))))
  	      				(if (equal? (length (car obj)) (length (car (cdr obj))))
            				(matrix? (cdr obj))
          					#f)
  	      			#f)))
  		#f))
)

;Question5 give the transpose of the matrix
(define (matrix-transpose m)
  (if (equal? (car m) '())
      '()
      (constructTranspose '(() () ()) m))
)

(define (constructTranspose curM m)
  (if (equal? m '())
      curM
      (constructTranspose (constructMatrix curM (car m)) (cdr m))))

(define (constructMatrix curMx list)
  (if (equal? list '())
      '()
      (cons (append (car curMx) (cons (car list) '())) (constructMatrix (cdr curMx) (cdr list))))
)

;Question6 Return the last element of a list
(define (last li)
  (if (equal? (cdr li) '())
      (car li)
      (last (cdr li))))

;Question7 Return all the element except for the last element
(define (all-but-last li)
  (if (equal? (cdr li) '())
      '()
      (cons (car li) (all-but-last (cdr li)))))


