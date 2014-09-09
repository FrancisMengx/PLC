;; Test code for CSSE 304 Assignment 10

(define (test-make-slist-leaf-iterator)
    (let ([correct '(
		     #f
		     #f
		     a
		     e
		     z
		     (c n b x a z)
		     #f
		     )]
          [answers 
            (list 
	     (let ((iter (make-slist-leaf-iterator 
			  (quote ((a b ())))))) 
	       (begin (iter) (iter)) (iter))
	     (let ((iter (make-slist-leaf-iterator 
			  (quote ((())))))) 
	       (iter))
	     (let ((iter (make-slist-leaf-iterator 
			  (quote ((() a (b c d ()) () e f g)))))) 
	       (iter))
	     (let ((iter (make-slist-leaf-iterator 
			  (quote ((() a (b () c (d ())) () e f g)))))) 
	       (begin (iter) (iter) (iter) (iter)) (iter))
	     (let ((iter (make-slist-leaf-iterator 
			  (quote ((() (() ()) (a) (z (x) d ()) () e f g)))))) 
	       (begin (iter)) (iter))
	     (let ((iter1 (make-slist-leaf-iterator 
			   (quote (a (b (c) (d)) (((e))))))) 
		   (iter2 (make-slist-leaf-iterator 
			   (quote (z (x n (v) ((m)))))))) 
	       (let loop ((count 2) 
			  (accum (quote ()))) 
		 (if (>= count 0) 
		     (loop (- count 1) 
			   (cons (iter1) (cons (iter2) accum))) 
		     accum)))
	     (let ((iter (make-slist-leaf-iterator 
			  (quote ((() (z) (a (x) d ()) () e f g)))))) 
	       (begin (iter) (iter) (iter) (iter) 
		      (iter) (iter) (iter) (iter) (iter)) (iter))
	     )])
      (display-results correct answers equal?)))

(define (test-free-vars)
    (let ([correct '(
		     (x)
		     (x)
		     ()
		     (y)
		     (x)
		     (x y z)
		     (x y)
		     ()
		     )]
          [answers 
            (list 
	     (free-vars (quote x))
	     (free-vars (quote (x x)))
	     (free-vars (quote (lambda (x) x)))
	     (free-vars (quote (lambda (x) y)))
	     (free-vars (quote (x (lambda (x) x))))
	     (free-vars (quote (x (lambda (x) (y z)))))
	     (free-vars (quote (x (lambda (x) y))))
	     (free-vars (quote ((lambda (y) (lambda (y) y)) 
				(lambda (x) (lambda (x) x)))))
	     )])
      (display-results correct answers sequal?-grading)))

(define (test-bound-vars)
    (let ([correct '(
		     ()
		     (x)
		     ()
		     (x)
		     ()
		     (x)
		     (y x)
		     )]
          [answers 
            (list 
	     (bound-vars (quote (x x)))
	     (bound-vars (quote (lambda (x) x)))
	     (bound-vars (quote (lambda (y) x)))
	     (bound-vars (quote (x (lambda (x) x))))
	     (bound-vars (quote (x (lambda (x) y))))
	     (bound-vars (quote (z (lambda (x) (lambda (y) x)))))
	     (bound-vars (quote ((lambda (y) (lambda (y) y)) 
				 (lambda (x) (z (lambda (x) x))))))
	     )])
      (display-results correct answers sequal?-grading)))

(define (test-occurs-free?)
    (let ([correct '(
		     #f
		     #t
		     #t
		     #t
		     #f
		     #f
		     #t
		     #f
		     #t
		     #f
		     #f
		     #t
		     #t
		     #t
		     )]
          [answers 
            (list 
	     (occurs-free? (quote x) (quote (lambda (a b x) x)))
	     (occurs-free? (quote x) (quote (lambda () (x))))
	     (occurs-free? (quote x) (quote (x (lambda (y x z) x))))
	     (occurs-free? (quote x) (quote (lambda (a b) (if (x a b) a b))))
	     (occurs-free? (quote a) (quote (lambda (a b) (if (x a b) a b))))
	     (occurs-free? (quote x) (quote (let ((x n)) 
					      ((lambda (y) (+ y x)) z))))
	     (occurs-free? (quote x) (quote (let ((w a) (y x)) 
					      ((lambda (y) (+ y x)) z))))
	     (occurs-free? (quote x) (quote (let* ((x a) (y x)) (y))))
	     (occurs-free? (quote b) (quote (let* ((y a) (x b)) ((x y) z))))
	     (occurs-free? (quote set!) (quote (lambda (x) (set! x y))))
	     (occurs-free? (quote x) (quote (lambda () 
					      (let* ((x a) 
						     (y x)) 
						(if (y z) 
						    (lambda () x) 
						    (lambda () y))))))
	     (occurs-free? (quote x) (quote (lambda () 
					      (let ((x a) 
						    (y x)) 
						(if (y z) (lambda () x) 
						    (lambda () y))))))
	     (occurs-free? (quote y) (quote (let ((y ((lambda (x) (+ x y)) z)))
					      (+ y y))))
	     (occurs-free? (quote z) (quote (let ((y ((lambda (x) (+ x y)) z)))
					      (+ y y))))
	     )])
      (display-results correct answers equal?)))

(define (test-occurs-bound?)
    (let ([correct '(
		     #t
		     #f
		     #t
		     #f
		     #t
		     #f
		     #t
		     #f
		     #f
		     #t
		     #t
		     #f
		     #t
		     #f
		     #t
		     #f
		     #t
		     #f
		     )]
          [answers 
            (list 
	     (occurs-bound? (quote x) (quote (lambda (a b x) x)))
	     (occurs-bound? (quote y) (quote (lambda (x a b) y)))
	     (occurs-bound? (quote x) (quote (x (lambda (y x z) x))))
	     (occurs-bound? (quote x) (quote (lambda (a b) 
					       (if (x a b) a b))))
	     (occurs-bound? (quote x) (quote (let ((x n)) 
					       ((lambda (y) (+ y x)) z))))
	     (occurs-bound? (quote x) (quote (let ((w a) (y x)) 
					       ((lambda (y) (+ y x)) z))))
	     (occurs-bound? (quote x) (quote (let* ((x a) (y x)) (y))))
	     (occurs-bound? (quote b) (quote (let* ((y a) (x b)) ((x y) z))))
	     (occurs-bound? (quote a) (quote (let* ((a x) (b c) (c b)) c)))
	     (occurs-bound? (quote b) (quote (let* ((a x) (b c) (c b)) c)))
	     (occurs-bound? (quote c) (quote (let* ((a x) (b c) (c b)) c)))
	     (occurs-bound? (quote set!) (quote (lambda (x) (set! x y))))
	     (occurs-bound? (quote x) (quote (lambda (x) (x))))
	     (occurs-bound? (quote x) (quote (lambda () x)))
	     (occurs-bound? (quote x) (quote (lambda () 
					       (let* ((x a) 
						      (y x)) 
						 (if (y z) 
						     (lambda () x) 
						     (lambda () y))))))
	     (occurs-bound? (quote z) (quote (lambda () 
					       (let* ((x a) 
						      (y x)) 
						 (if (y z) 
						     (lambda () x) 
						     (lambda () y))))))
	     (occurs-bound? (quote y) (quote (let ((y ((lambda (x) (+ x y)) 
						       z))) 
					       (+ y y))))
	     (occurs-bound? (quote z) (quote (let ((y ((lambda (x) (+ x y)) 
						       z))) 
					       (+ y y))))
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
  (display 'make-slist-leaf-iterator) 
  (test-make-slist-leaf-iterator)    
  (display 'free-vars) 
  (test-free-vars)
  (display 'bound-vars) 
  (test-bound-vars)  
  (display 'occurs-free?) 
  (test-occurs-free?)  
  (display 'occurs-bound?) 
  (test-occurs-bound?)
)

(define r run-all)
