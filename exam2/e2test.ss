;; Test code for CSSE 304 Exam 2

(define (test-product-cps)
    (let ([correct '(
		     (((4 1) (4 2) (4 3) (3 1) (3 2) (3 3)))
		     (() ((4 1) (3 1)))
		     #f
		     #t
		     )]
          [answers 
            (list 
	     (product-cps '(3 4) '(1 2 3) list)
	     (product-cps '(3 4) '(1) 
			  (lambda (first-prod) 
			    (product-cps '() '(3 6 7) 
					 (lambda (second-prod) 
					   (list second-prod first-prod)))))
	     (product-cps '(3 4) '(1 2 3) (lambda (v) (matrix?-cps v not)))
	     (product-cps '(3 4) '() (lambda (v) (matrix?-cps v not)))
	     )])
      (display-results correct answers equal?)))

(define (test-expand-lets)
    (let ([correct '(
		     ((lambda (a b) (+ a b)) 3 4)
		     (if a ((lambda (a b) (+ a b)) 3 4) 8)
		     ((lambda (b) (+ b 8)) ((lambda (a b) (+ a b)) 3 4))
		     ((lambda (d) ((lambda (b) (+ b c)) 
				   ((lambda (a b) (+ a b)) 3 4))) 10)
		     )]
          [answers 
            (list 
	     (unparse-exp (expand-lets (parse-exp 
		   '(let ([a 3] [b 4]) (+ a b)))))
	     (unparse-exp (expand-lets (parse-exp 
                   '(if a 
			(let ([a 3] [b 4]) (+ a b))
			8))))
	     (unparse-exp (expand-lets (parse-exp 
                   '(let ([b (let ([a 3] [b 4]) 
			       (+ a b))]) 
		      (+ b 8)))))
	     (unparse-exp (expand-lets (parse-exp 
                    '((lambda (d) 
			(let ([b (let ([a 3] [b 4]) 
				   (+ a b))]) 
			  (+ b c))) 
		      10))))
	     )])
      (display-results correct answers equal?)))

(define (test-lyst)
    (let ([correct '(
		     (1 2 4)
		     (1 3)
		     (2 4)
		     (3 7)
		     ((9 6 1) (4 7 3))
		     ((9 0 1) (4 7 3))
		     ((1 3 7 9) (2 4 6 8))
		     )]
          [answers 
            (list 
	     (let ([lyst1 (make-lyst)] 
		   [lyst2 (make-lyst)]) 
	       (lyst1 'insert-at-current-pos 1) 
	       (lyst2 'insert-at-current-pos 3) 
	       (lyst2 'insert-at-current-pos 4) 
	       (list (lyst1 'get-current-item) 
		     (lyst2 'length) 
		     (lyst2 'get-current-item)))
	     (let ([lyst1 (make-lyst)] [lyst2 (make-lyst)]) 
	       (lyst1 'insert-at-current-pos 1) 
	       (lyst2 'insert-at-current-pos 3) 
	       (lyst2 'insert-at-current-pos 4) 
	       (lyst2 'shift 1) 
	       (list (lyst1 'length) 
		     (lyst2 'get-current-item)))
	     (let ([lyst1 (make-lyst)] [lyst2 (make-lyst)]) 
	       (lyst1 'insert-at-current-pos 1) 
	       (lyst2 'insert-at-current-pos 3) 
	       (lyst2 'insert-at-current-pos 4) 
	       (lyst2 'shift 1) 
	       (lyst2 'insert-at-current-pos 7) 
	       (lyst2 'goto 0) 
	       (lyst1 'insert-at-current-pos 6) 
	       (list (lyst1 'length)
		     (lyst2 'get-current-item)))
	     (let ([lyst1 (make-lyst)] [lyst2 (make-lyst)]) 
	       (lyst1 'insert-at-current-pos 1) 
	       (lyst2 'insert-at-current-pos 3) 
	       (lyst2 'insert-at-current-pos 4) 
	       (lyst2 'shift 1) 
	       (lyst2 'insert-at-current-pos 7) 
	       (lyst2 'goto 0) 
	       (lyst1 'insert-at-current-pos 6) 
	       (lyst2 'goto 2)
	       (lyst2 'shift -1) 
	       (lyst1 'insert-at-current-pos 9) 
	       (list (lyst1 'length) 
		     (lyst2 'get-current-item)))
	     (let ([lyst1 (make-lyst)] 
		   [lyst2 (make-lyst)] 
		   [lyst->list (lambda (ly) 
				 (let ([saved-pos (ly 'get-current-pos)]) 
				   (ly 'goto 0) 
				   (let loop () 
				     (if (= (ly 'get-current-pos) 
					    (ly 'length)) 
					 (begin (ly 'goto saved-pos) '()) 
					 (let ([result (ly 'get-current-item)]) 
					   (ly 'shift 1) 
					   (cons result (loop)))))))]) 
	       (lyst1 'insert-at-current-pos 1) 
	       (lyst2 'insert-at-current-pos 3) 
	       (lyst2 'insert-at-current-pos 4) 
	       (lyst2 'shift 1) 
	       (lyst2 'insert-at-current-pos 7) 
	       (lyst2 'goto 0) 
	       (lyst1 'insert-at-current-pos 6) 
	       (lyst2 'goto 2) 
	       (lyst2 'shift -1) 
	       (lyst1 'insert-at-current-pos 9) 
	       (map lyst->list (list lyst1 lyst2)))
	     (let ([lyst1 (make-lyst)] 
		   [lyst2 (make-lyst)] 
		   [lyst->list (lambda (ly) 
				 (let ([saved-pos (ly 'get-current-pos)]) 
				   (ly 'goto 0) 
				   (let loop () 
				     (if (= (ly 'get-current-pos) (ly 'length))
					 (begin (ly 'goto saved-pos) '()) 
					 (let ([result (ly 'get-current-item)])
					   (ly 'shift 1) 
					   (cons result (loop)))))))]) 
	       (lyst1 'insert-at-current-pos 1) 
	       (lyst2 'insert-at-current-pos 3) 
	       (lyst2 'insert-at-current-pos 4) 
	       (lyst2 'shift 1) 
	       (lyst2 'insert-at-current-pos 7) 
	       (lyst2 'goto 0) 
	       (lyst1 'insert-at-current-pos 6) 
	       (lyst2 'goto 2) 
	       (lyst2 'shift -1) 
	       (lyst1 'insert-at-current-pos 9) 
	       (lyst1 'goto 1) 
	       (lyst1 'replace-current-item 0) 
	       (map lyst->list (list lyst1 lyst2)))
	     (let* ([list->lyst (lambda (ls) 
				  (let ([lyst (make-lyst)]) 
				    (let loop ([ls ls]) 
				      (if (null? ls) lyst 
					  (begin 
					    (lyst 'insert-at-current-pos (car ls)) 
					    (lyst 'shift 1) 
					    (loop (cdr ls)))))))] 
		    [lyst1 (list->lyst '(1 3 5 7 9))] 
		    [lyst2 (list->lyst '(2 4 6 8))] 
		    [lyst->list (lambda (ly) 
				  (let ([saved-pos (ly 'get-current-pos)]) 
				    (ly 'goto 0) 
				    (let loop () 
				      (if (= (ly 'get-current-pos) (ly 'length)) 
					  (begin (ly 'goto saved-pos) '()) 
					  (let ([result (ly 'get-current-item)]) 
					    (ly 'shift 1) 
					    (cons result (loop)))))))]) 
	       (lyst1 'goto 2) 
	       (lyst1 'remove-current) 
	       (map lyst->list (list lyst1 lyst2)))
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
  (display 'product-cps ) 
  (test-product-cps )
  (display 'expand-lets) 
  (test-expand-lets)
  (display 'lyst) 
  (test-lyst)
)

(define r run-all)
