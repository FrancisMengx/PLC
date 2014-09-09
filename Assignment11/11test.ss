;; Test code for CSSE 304 Assignment 11

(define (test-lexical-address)
    (let ([correct '(
		     (: free x)
		     ((: free x) (: free y))
		     (lambda (x y) ((: free cons) (: 0 0) (: 0 1)))
		     (lambda (x y z) (if (: 0 1) (: 0 0) (: 0 2)))
		     (lambda (x y) 
		       (lambda () 
			 ((lambda (z) 
			    (lambda (x w) 
			      (lambda () 
				((: 1 0) (: 4 1) (: 2 0) (: 1 1))))) 
			  ((: 1 0) (: 1 1) (: free z) (: free w)))))
		     (lambda (a b c) 
		       (if ((: free eq?)(: 0 1) (: 0 2)) 
			   ((lambda (c) 
			      ((: free cons) (: 1 0) (: 0 0))) 
			    (: 0 0)) 
			   (: 0 1)))
		     ((lambda (x y) 
			(((lambda (z) 
			    (lambda (w y) 
			      ((: free +) (: 2 0) (: 1 0) (: 0 0) (: 0 1)))) 
			  ((: free list) (: free w) (: 0 0) (: 0 1) (: free z))) 
			 ((: free +) (: 0 0) (: 0 1) (: free z)))) 
		      ((: free y) (: free z)))
		     (if ((lambda (x) 
			    ((: free y) (: 0 0))) 
			  (lambda (y) 
			    ((: 0 0) (: free x)))) 
			 (lambda (z) 
			   (if (: free x) 
			       (: free y) 
			       ((: free cons) (: 0 0) (: 0 0)))) 
			 ((: free x) (: free y)))
		     )]
          [answers 
            (list 
	     (lexical-address (quote x))
	     (lexical-address (quote (x y)))
	     (lexical-address (quote (lambda (x y) (cons x y))))
	     (lexical-address (quote (lambda (x y z) (if y x z))))
	     (lexical-address 
	      (quote (lambda (x y) 
		       (lambda () 
			 ((lambda (z) 
			    (lambda (x w) 
			      (lambda () (x y z w)))) 
			  (x y z w))))))
	     (lexical-address 
	      (quote (lambda (a b c) 
		       (if (eq? b c) 
			   ((lambda (c) (cons a c)) a) 
			   b))))
	     (lexical-address 
	      '((lambda (x y) 
		  (((lambda (z) 
		      (lambda (w y) 
			(+ x z w y))) 
		    (list w x y z)) 
		   (+ x y z))) 
		(y z)))
	     (lexical-address 
	      (quote (if ((lambda (x) (y x)) 
			  (lambda (y) (y x))) 
			 (lambda (z) 
			   (if x y (cons z z))) 
			 (x y))))
	     )])
      (display-results correct answers equal?)))

(define (test-un-lexical-address)
    (let ([correct '(
		     (x y)
		     (if ((lambda (x) (y x)) 
			  (lambda (y) 
			    (y x))) 
			 (lambda (z) 
			   (if x y (cons z z))) 
			 (x y))
		     (lambda (x y) 
		       (lambda () 
			 ((lambda (z) 
			    (lambda (x w) 
			      (lambda () 
				(x y z w)))) 
			  (x y z w))))
		     )]
          [answers 
            (list 
	     (un-lexical-address (lexical-address (quote (x y))))
	     (un-lexical-address 
	      (lexical-address 
	       (quote (if ((lambda (x) (y x)) 
			   (lambda (y) (y x))) 
			  (lambda (z) 
			    (if x y (cons z z))) 
			  (x y)))))
	     (un-lexical-address 
	      (lexical-address 
	       (quote (lambda (x y) 
			(lambda () 
			  ((lambda (z) 
			     (lambda (x w) 
			       (lambda () 
				 (x y z w)))) 
			   (x y z w)))))))
	     )])
      (display-results correct answers equal?)))

(define (test-my-let)
    (let ([correct '(
		     1
		     55
		     123
		     3
		     )]
          [answers 
            (list 
	     (my-let ((a 1)) a)

	     (my-let loop 
		     ((L (quote (1 2 3 4 5 6 7 8 9 10))) 
		      (A 0)) 
		     (if (null? L) 
			 A 
			 (loop (cdr L) 
			       (+ (car L) A))))
	     (my-let ((a 5)) 
		     (+ 3 
			(my-let fact ((n a)) 
				(if (zero? n) 
				    1 
				    (* n (fact (- n 1)))))))
	     (my-let ((a (lambda () 3))) 
		     (my-let ((a (lambda () 5)) 
			      (b a)) 
			     (b)))
	     )])
      (display-results correct answers equal?)))

(define (test-my-or)
    (let ([correct '(
		     #t
		     (#(2 s c) 5 2)
		     #t
		     #f
		     1
		     4
		     1
		     6
		     )]
          [answers 
            (list 
	     (begin (set! a #f) 
		    (my-or #f 
			   (begin 
			     (set! a (not a)) 
			     a) 
			   #f))
	     (let loop ((L (quote (a b 2 5 #f (a b c) #(2 s c) foo a))) 
			(A (quote ()))) 
	       (if (null? L) 
		   A 
		   (loop (cdr L) 
			 (if (my-or (number? (car L)) 
				    (vector? (car L)) 
				    (char? (car L))) 
			     (cons (car L) A) 
			     A))))
	     (let loop ((L (quote (1 2 3 4 5 a 6)))) 
	       (if (null? L) 
		   #f 
		   (my-or (symbol? (car L)) 
			  (loop (cdr L)))))
	     (my-or)
	     (let ([x 0]) 
	       (if (my-or 
		    #f 
		    4 
		    (begin (set! x 12) 
			   #t)) 
		   (set! x (+ x 1)) 
		   (set! x (+ x 3))) 
	       x)
	     (my-or #f 4 3)
	     (let ([x 0]) 
	       (my-or (begin (set! x (+ 1 x)) 
			     x) 
		      #f))
	     (my-or 6)
	     )])
      (display-results correct answers equal?)))

(define (test-+=)
    (let ([correct '(
		     25
		     (41 31 41)
		     )]
          [answers 
            (list 
	     (let ([a 5]) 
	       (+= a 10) 
	       (+ a 10))
	     (begin (let* ((a 10) (b 21) (c (+= a (+= b a)))) (list a b c)))
	     )])
      (display-results correct answers equal?)))

(define (test-return-first)
    (let ([correct '(
		     2
		     5
		     3
		     (5 3)
		     )]
          [answers 
            (list 
	     (return-first 2)
	     (begin (let ([a 3]) 
		      (return-first (+ a 2) 
				    (set! a 7) 
				    a)))
	     (return-first (return-first 
			    3 
			    4 
			    5) 
			   1 
			   2)
	     (let ([a 4]) 
	       (let ([b (return-first 3 
				      (set! a 5) 
				      2)]) 
		 (list a b)))
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
  (display 'lexical-address) 
  (test-lexical-address)
  (display 'un-lexical-address) 
  (test-un-lexical-address)
  (display 'my-let) 
  (test-my-let)
  (display 'my-or) 
  (test-my-or)   
  (display '+=) 
  (test-+=)
  (display 'return-first) 
  (test-return-first)  
)

(define r run-all)
