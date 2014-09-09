;; Test code for CSSE 304 Assignment 6

(define (test-curry2)
    (let ([correct '(
		     6
		     (17 . 29)
		     )]
          [answers 
            (list 
	     (((curry2 -) 8) 2)
	     (let([conscurry (curry2 cons)]) ((conscurry 17) 29))	     
	     )])
      (display-results correct answers equal?)))

(define (test-curried-compose)
  (let ([correct '(arg 1)]
        [answers 
          (list  
	   (((curried-compose car) list) 'arg)
	   (((curried-compose car) car)  '((1 7) (2 9)))
	   )])
    (display-results correct answers equal?)))

(define (test-compose)
  (let ([correct '(1 2)]
        [answers 
          (list
	   ((compose car car) '((1 7) (2 9)))
	   ((compose car car cdr) '((1 7) (2 9)))
          )])
    (display-results correct answers equal?)))

(define (test-make-list-c)
  (let ([correct '(
		  (7 7 7 7)
		  (() () () () ())
		  ()
		   )]
        [answers 
	 (list
	   ((make-list-c 4) 7)
	   ((make-list-c 5) '())
	   ((make-list-c 0) 10)
	 )])
    (display-results correct answers equal?)))

            


(define (test-let->application)
  (let ([correct '(
		   ((lambda (a b) (let ((c b)) (+ a b c))) 4 5)
		   ((lambda () (+ 2 3)))
		   ((lambda (a b c) 
		      (let ((d 3)) 
			(+ a b c d))) 
		    2 1 5)
		   )]
        [answers 
          (list
	   (let->application (quote (let ((a 4) (b 5)) (let ((c b)) (+ a b c)))))	
	   (let->application '(let () (+ 2 3)))
	   (let->application
	    '(let ([a 2] [b 1] [c 5])
	       (let ([d 3])
		 (+ a b c d))))
	   )])
    (display-results correct answers equal?)))

(define (test-let*->let  )
  (let ([correct '(
		   (let ((x 0)) x)
		   (let ((x 50)) 
		     (let ((y (+ x 50))) 
		       (let ((z (+ y 50))) 
			 z)))
		   (let ((x (let ((y 1)) y))) 
		     (let ((z x)) 
		       x))
		   )]
        [answers 
          (list
	   (let*->let (quote (let* ((x 0)) x)))
	   (let*->let 
	    (quote (let* ((x 50) 
			  (y (+ x 50)) 
			  (z (+ y 50)))  
		     z)))
	   (let*->let 
	    (quote 
	     (let* ((x (let ((y 1)) y)) 
		    (z x)) 
	       x)))
	  )])
    (display-results correct answers equal?)))

(define (test-filter-in)
  (let ([correct '(
		   (2 3 5)
		   (() () ())
		   (() (1 2))
		   ((1 2) (3 . 4))
		   ()
		   )]
        [answers 
          (list
	   (filter-in positive? '(-1 2 0 3 -6 5))
	   (filter-in null? '(() (1 2) (3 4) () ()))
	   (filter-in list? '(() (1 2) (3 . 4) #2(4 5)))
	   (filter-in pair? '(() (1 2) (3 . 4) #2(4 5)))
	   (filter-in positive? '())	  
	   )])
     (display-results correct answers equal?)))


(define (test-filter-out)
  (let ([correct '( 
		   (-1 0 -6 0)
		   ((1 2) (3 4))
		   ((3 . 4) #(4 5))
		   (() #(4 5))
		   ()
		   )]
        [answers 
          (list
	   (filter-out positive? '(-1 2 0 3 -6 5 0))
	   (filter-out null? '(() (1 2) (3 4) () ()))
	   (filter-out list? '(() (1 2) (3 . 4) #2(4 5)))
	   (filter-out pair? '(() (1 2) (3 . 4) #2(4 5)))
	   (filter-out positive? '())
	  )])
    (display-results correct answers equal?)))


(define (test-sort-list-of-symbols)
  (let ([correct '(
		   (ab b b c d f g m r)
		   (b)
		   ()
		   )]
        [answers 
          (list
	   (sort-list-of-symbols '(b c d g ab f b r m))
	   (sort-list-of-symbols '(b))
	   (sort-list-of-symbols '())
	  )])
    (display-results correct answers equal?)))


;-----------------------------------------------

(define display-results
  (lambda (correct results test-procedure?)
     (display ": ")
     (pretty-print 
      (if (andmap test-procedure? correct results)
          'All-correct
          `(correct: ,correct yours: ,results)))))


(define sequal?-grading
  (lambda (l1 l2)
    (cond
     ((null? l1) (null? l2))
     ((null? l2) (null? l1))
     ((or (not (set?-grading l1))
          (not (set?-grading l2)))
      #f)
     ((member (car l1) l2) (sequal?-grading
                            (cdr l1)
                            (rember-grading
                             (car l1)
                             l2)))
     (else #f))))

(define set?-grading
  (lambda (s)
    (cond [(null? s) #t]
          [(not (list? s)) #f]
          [(member (car s) (cdr s)) #f]
          [else (set?-grading (cdr s))])))

(define rember-grading
  (lambda (a ls)
    (cond
     ((null? ls) ls)
     ((equal? a (car ls)) (cdr ls))
     (else (cons (car ls) (rember-grading a (cdr ls)))))))

(define set-equals? sequal?-grading)

(define find-edges  ; e know that this node is in the graph before we do the call
  (lambda (graph node)
    (let loop ([graph graph])
      (if (eq? (caar graph) node)
	  (cadar graph)
	  (loop (cdr graph))))))

;; Problem 8  graph?
(define set?  ;; Is this list a set?  If not, it is not a graph.
  (lambda (list)
    (if (null? list) ;; it's an empty set.
	#t
	(if (member (car list) (cdr list))
	    #f
	    (set? (cdr list))))))


(define graph?
  (lambda (obj)
    (and (list? obj)
	 (let ([syms (map car obj)])
	   (and (set? syms)
		(andmap symbol? syms)
		(andmap (lambda (x)
			  (andmap (lambda (y) (member y (remove (car x) syms)))
				  (cadr x)))
			obj))))))
    
(define graph-equal?
  (lambda (a b)
    (and
     (graph? a) 
     (graph? b)
     (let ([a-nodes (map car a)]
	   [b-nodes (map car b)])
       (and 
	(set-equals? a-nodes b-nodes)
	    ; Now  See if the edges from each node are equivalent in the two graphs.
	(let loop ([a-nodes a-nodes])
	  (if (null? a-nodes)
	      #t
	      (let ([a-edges (find-edges a (car a-nodes))]
		    [b-edges (find-edges b (car a-nodes))])
		(and (set-equals? a-edges b-edges)
		     (loop (cdr a-nodes)))))))))))

(define (test-graph-equal)
  (list
   (graph-equal? '((a (b)) (b (a))) '((b (a)) (a (b))))
   (graph-equal? '((a (b c d)) (b (a c d)) (c (a b d)) (d (a b c)))
		 '((b (a c d)) (c (a b d)) (a (b d c)) (d (b a c))))
   (graph-equal? '((a ())) '((a ())))
   (graph-equal? '((a (b c)) (b (a c)) (c (a b))) '((a (b c)) (b (a c)) (c (a b))))
   (graph-equal? '() '())
   ))



(define g test-graph-equal)
	   
	  
     



;You can run the tests individually, or run them all
;#by loading this file (and your solution) and typing (r)

(define (run-all)
  (display 'curry2) 
  (test-curry2)
  (display 'curried-compose) 
  (test-curried-compose)
  (display 'compose) 
  (test-compose)
  (display 'make-list-c) 
  (test-make-list-c)    
  (display 'let->application) 
  (test-let->application)
  (display 'let*->let  ) 
  (test-let*->let)  
  (display 'filter-in) 
  (test-filter-in)  
  (display 'filter-out) 
  (test-filter-out)
  (display 'sort-list-of-symbols) 
  (test-sort-list-of-symbols)
)

(define r run-all)
