;; Test code for CSSE 304 Assignment 7

(define (test-invert)
    (let ([correct '(
		     ((2 1) (4 3) (6 5))
		     ()
		     )]
          [answers 
            (list 
	     (invert '((1 2) (3 4) (5 6)))
	     (invert '())
	     )])
      (display-results correct answers equal?)))

(define (test-vector-index)
    (let ([correct '(
		     1
		     3
		     0
		     2
		     #f
		     )]
          [answers 
            (list 
	     (vector-index (lambda (x) (eq? x (quote a))) (quote #(b a c d)))
	     (vector-index (lambda (x) (eq? x (quote a))) (quote #(b c c a)))
	     (vector-index (lambda (x) (eq? x (quote a))) (quote #(a b a)))
	     (vector-index positive? (quote #(-3 0 1 4 -2)))
	     (vector-index positive? (quote #(-3 0 -1 -4 -2)))
	     )])
      (display-results correct answers equal?)))

(define (test-vector-append-list)
    (let ([correct '(
		     #(a b c d)
		     #(1 2 3 4 5 6)
		     #(a)
		     #(a)
		     #()
		     #(a b (c d e))
		     )]
          [answers 
            (list 
	     (vector-append-list (quote #(a b)) (quote (c d)))
	     (vector-append-list (quote #(1 2 3 4)) (quote (5 6)))
	     (vector-append-list (quote #(a)) (quote ()))
	     (vector-append-list (quote #()) (quote (a)))
	     (vector-append-list (quote #()) (quote ()))
	     (vector-append-list (quote #(a b)) (quote ((c d e))))
	     )])
      (display-results correct answers equal?)))

(define (test-qsort)
    (let ([correct '(
		     ()
		     (9 7 5 4 2)
		     (6 5 4 3 2 1)
		     (6 5 5 4 3 2 1 1)
		     ("rose-hulman" "great" "day" "at" "a")
		     )]
          [answers 
            (list 
	     (qsort < (quote ()))
	     (qsort > (quote (7 2 5 9 4)))
	     (qsort > (quote (6 3 2 4 1 5)))
	     (qsort > (quote (1 6 5 4 3 2 1 5)))
	     (qsort string>? (quote ("a" "great" "day" "at" "rose-hulman")))
	     )])
      (display-results correct answers equal?)))

(define (test-connected?)
    (let ([correct '(
		     #t
		     #t
		     #t
		     #t
		     #t
		     )]
          [answers 
            (list 
	     (and (connected? (quote ((a ())))) 
		  (not (connected? (quote ((a ()) (b ()))))))
	     (and (connected? (quote ((a (b)) (b (a))))) 
		  (not (connected? (quote ((a (b)) (c ()) (b (a)))))))
	     (and (connected? (quote ((a (b c)) (b (c a)) (c (b a))))) 
		  (not (connected? (quote ((a (b c)) (b (c a)) (c (b a)) 
					   (d (e f g)) (e (d f g)) (f (d e g)) 
					   (g (d e f)))))))
	     (and (not (connected? (quote ((a (b)) (b (a)) (c (d)) (d (c)))))) 
		  (connected? (quote ((a (b)) (k (j)) (j (k i)) (c (b d)) 
				      (i (h j)) (h (i g)) (g (f h)) (b (c a)) 
				      (f (e g)) (e (d f)) (d (c e))))))
	     (and (connected? (quote ((a ())))) 
		  (not (connected? (quote ((a (b c)) (b (a c)) (c (b a)) 
					   (d (e f)) (e (d f)) (f (d e)))))))
	     )])
      (display-results correct answers equal?)))

(define (test-reverse-it)
    (let ([correct '(
		     ()
		     (1)
		     (5 4 3 2 1)
		     )]
          [answers 
            (list 
	     (reverse-it '())
	     (reverse-it '(1))
	     (reverse-it '(1 2 3 4 5))
	     )])
      (display-results correct answers equal?)))

(define (test-BST)
    (let ([correct '(
		     #t
		     ()
		     (1)
		     (1 2)
		     (1 2 3)
		     (1 3 4 5 6 7 8 9)
		     (1 3 4 5 6 7 8 9)
		     8
		     #t
		     #t
		     #t
		     #t
		     #t
		     #t
		     #t
		     #t
		     #t
		     #t
		     #t
		     #t
		     #t
		     #t
		     #t
		     #t
		     )]
          [answers 
            (list 
	     (empty-BST? (empty-BST))
	     (BST-inorder (empty-BST))
	     (BST-inorder (BST-insert 1 (empty-BST)))
	     (BST-inorder (BST-insert 1 (BST-insert 2 (empty-BST))))
	     (BST-inorder (BST-insert 2 (BST-insert 1 (BST-insert 3 (empty-BST)))))
	     (BST-inorder (BST-insert-nodes (empty-BST) '(4 7 3 9 6 1 5 8)))
	     (BST-inorder (BST-insert-nodes (empty-BST) '(4 7 3 9 6 1 5 6 8)))
	     (BST-element (BST-left (BST-right (BST-right 
		 (BST-insert-nodes (empty-BST) '(4 7 3 9 6 1 5 8))))))
	     (and (not (BST-contains? (empty-BST) 1)) 
		  (BST-contains? (BST-insert 1 (empty-BST)) 1))
	     (and (not (BST-contains? (empty-BST) 1)) 
		  (BST-contains? (BST-insert-nodes (empty-BST) 
						   '(4 7 3 9 6 1 5 8)) 
				 8))
	     (and (not (BST-contains? (BST-insert-nodes (empty-BST) 
							'(4 7 3 9 6 1 5 8)) 
				      10)) 
		  (BST-contains? (BST-insert 1 (empty-BST)) 1))
	     (and (not (BST-contains? (empty-BST) 1)) 
		  (BST-contains? (BST-insert-nodes (empty-BST) 
						   '(4 7 3 9 6 1 5 8)) 
				 1))
	     (and (not (BST-contains? (BST-insert-nodes (empty-BST) 
							'(4 7 3 9 6 1 5 8)) 
				      2)) 
		  (BST-contains? (BST-insert 1 (empty-BST)) 1))
	     (and (not (BST-contains? (empty-BST) 1)) 
		  (BST-contains? (BST-insert-nodes (empty-BST) 
						   '(4 7 3 9 6 1 5 8)) 
				 4))
	     (and (not (BST-contains? (BST-insert-nodes (empty-BST) 
							'(4 7 3 9 6 1 5 8)) 
				      0)) 
		  (BST-contains? (BST-insert 1 (empty-BST)) 1))
	     (and (not (BST? #t)) (BST? '()))
	     (and (not (BST? '(1))) (BST? '()))
	     (and (not (BST? '(1 2 3))) (BST? '()))
	     (and (not (BST? '(1 ()))) (BST? '()))
	     (and (BST? '(1 () ())) (BST? '()))
	     (and (not (BST? '(a () ()))) (BST? '()))
	     (and (not (BST? '(1 (2 () ()) ()))) (BST? '()))
	     (and (not (BST? #t)) (BST? '(1 () (3 (2 () ()) ()))))
	     (and (not (BST? '(4 () (6 (3 () ()) ())))) (BST? '()))
	     )])
      (display-results correct answers equal?)))

(define (test-map-by-position )
    (let ([correct '(
		     (2 2 2 2)
		     )]
          [answers 
            (list 
	     (map-by-position (list cadr - length (lambda(x)(- x 3))) 
			      '((1 2) -2 (3 4) 5))
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
  (display 'invert ) 
  (test-invert )
  (display 'vector-index) 
  (test-vector-index)
  (display 'vector-append-list) 
  (test-vector-append-list)
  (display 'qsort) 
  (test-qsort)    
  (display 'connected?) 
  (test-connected?)
  (display 'reverse-it ) 
  (test-reverse-it)  
  (display 'BST) 
  (test-BST)  
  (display 'map-by-position ) 
  (test-map-by-position )
)

(define r run-all)
