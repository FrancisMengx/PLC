;; Test code for CSSE 304 Assignment 13



; I don't have a simple way to test for parsing erros off-line.
; I wrote this in the familar form of my test procedures, but you should not actually run it.
; You can read the "correct" answers to get an idea of wha thte error issue is for each case.
; run each case individually and make sure that it causes an error, with 'parse-exp


(define (test-parse-errors)
    (let ([correct '(
		     (*error* parse-exp "lambda-expression: incorrect length ~s" '((lambda (a))))
		     (*error* parse-exp "lambda-expression: incorrect length ~s" '((lambda x)))
		     (*error* parse-exp "expression ~s is not a proper list" '((a b . c)))
		     (*error* parse-exp "lambda's formal arguments ~s must all be symbols" '((a b 1)))
		     (*error* parse-exp "if-expression ~s does not have (only) test, then, and else" '((if a)))
		     (*error* parse-exp "~s-expression has incorrect length ~s" '(let (let [(a b)])))
		     (*error* parse-exp "~s-expression has incorrect length ~s" '(letrec (letrec [(a b)])))
		     (*error* parse-exp "declarations in ~s-expression not a list ~s" '(let (let [(a b) . c] e)))
		     (*error* parse-exp "declaration in ~s-exp is not a proper list ~s" '(let (let [(a b) (c d) (e . f)] g)))
		     (*error* parse-exp "lambda-expression: incorrect length ~s" '((lambda x)))
		     (*error* parse-exp "declarations in ~s-expression not a list ~s" '(let* (let* a b)))
		     (*error* parse-exp "declarations in ~s-expression not a list ~s" '(letrec (letrec a b)))
		     (*error* parse-exp "declaration in ~s-exp must be a list of length 2 ~s" '(let (let [(a b c) (d e)] f)))
		     (*error* parse-exp "declaration in ~s-exp must be a list of length 2 ~s" '(let* (let* [(a b c) (d e)] f)))
		     (*error* parse-exp "declaration in ~s-exp must be a list of length 2 ~s" '(letrec (letrec [(a b) (c)] d)))
		     (*error* parse-exp "vars in ~s-exp must be symbols ~s" '(let (let [(a b) (3 c)] d)))
		     (*error* parse-exp "vars in ~s-exp must be symbols ~s" '(letrec (letrec [(a b) (3 c)] d)))
		     (*error* parse-exp "vars in ~s-exp must be symbols ~s" '(let* (let* [(a b) (3 c)] d)))
		     (*error* parse-exp "~s-expression has incorrect length ~s" '(let (let [(a (lambda (x)))])))
		     (*error* parse-exp "set! expression ~s does not have (only) variable and expression" '((set! x)))
		     (*error* parse-exp "if-expression ~s does not have (only) test, then, and else" '((if x)))
		     (*error* parse-exp "set! expression ~s does not have (only) variable and expression" "")
		     )]
          [answers 
            (list 
(parse-exp (quote (lambda (a))))
(parse-exp (quote (lambda x)))
(parse-exp (quote (a b . c)))
(parse-exp (quote (lambda (a b 1) c)))
(parse-exp (quote (if a)))
(parse-exp (quote (let ((a b)))))
(parse-exp (quote (letrec ((a b)))))
(parse-exp (quote (let ((a b) . c) e)))
(parse-exp (quote (let ((a b) (c d) (e . f)) g)))
(parse-exp (quote (letrec ((a b) (c d) (e (lambda x))) g)))
(parse-exp (quote (let* a b)))
(parse-exp (quote (letrec a b)))
(parse-exp (quote (let ((a b c) (d e)) f)))
(parse-exp (quote (let* ((a b c) (d e)) f)))
(parse-exp (quote (letrec ((a b) (c)) d)))
(parse-exp (quote (let ((a b) (3 c)) d)))
(parse-exp (quote (letrec ((a b) (3 c)) d)))
(parse-exp (quote (let* ((a b) (3 c)) d)))
(parse-exp (quote (let ((a (lambda (x)))))))
(parse-exp (quote (set! x)))
(parse-exp '(let ((a (let ((b (if x))) b))) a))
(parse-exp '(set! x (let ((a (set! a b c))) a)))
	     )])
      (display-results correct answers error-equal?)))

(define (test-parse-unparse)
    (let ([correct '(
		     x
		     (lambda (x) (+ x 5))
		     (lambda x y z)
		     (let ((x y)) x x x y)
		     (lambda (x) 1 z)
		     (let* ((a b) (c d) (e f)) g h)
		     (let* ((a b)) b)
		     (lambda x x y)
		     (let ((x 1) 
			   (y (let () 
				(let ((z t)) 
				  z)))) 
		       (+ z y))
		     (lambda () 
		       (letrec ((foo 
				 (lambda (L) 
				   (if (null? L) 
				       3 
				       (if (symbol? (car L)) 
					   (cons (car L) 
						 (foo (cdr L))) 
					   (foo (cdr L))))))) foo))
		     (lambda (x) 
		       (if (boolean? x) 
			   #(1 2 3 4) 1234))
		     (lambda x (car x))
		     (lambda (c) (if (char? c) string 12345))
		     (lambda (datum) 
		       (or (number? datum) 
			   (boolean? datum) 
			   (null? datum) 
			   (string? datum) 
			   (symbol? datum) 
			   (pair? datum) 
			   (vector? datum)))
		     (lambda (t) 
		       (let ((L (build-list t)) 
			     (sum< (lambda (a b) 
				     (< (cdr a) (cdr b)))) 
			     (intsum? (lambda (x) 
					(symbol? (car x))))) 
			 (car (genmax sum< (filter intsum? L)))))
		     (letrec ((a (lambda () (b 2))) 
			      (b (lambda (x) (- x 4)))) 
		       (lambda () (a)))
		     (let* ((a (lambda () (c 4))) 
			    (b a)) 
		       (lambda (b) (a)))
		     (lambda x (cons a x))
		     (lambda x 
		       (let* ((a x) 
			      (b (cons x a))) b))
		     (lambda (a b c) 
		       (let* ((dbl (lambda x 
				     (append x x))) 
			      (lst (dbl a b c))) 
			 lst))
		     (lambda a 
		       (letrec ((stuff (lambda (b) 
					 (if (null? b) 
					     (list) 
					     (cons (car b) 
						   (stuff (cdr b))))))) 
			 (stuff a)))
		     (lambda (a b c) 
		       (let* ((a b) 
			      (d (append a c))) 
			 d))
		     )]
          [answers 
            (list 
	     (unparse-exp (parse-exp (quote x)))
	     (unparse-exp (parse-exp (quote (lambda (x) (+ x 5)))))
	     (unparse-exp (parse-exp (quote (lambda x y z))))
	     (unparse-exp (parse-exp (quote (let ((x y)) x x x y))))
	     (unparse-exp (parse-exp (quote (lambda (x) 1 z))))
	     (unparse-exp (parse-exp (quote (let* ((a b) (c d) (e f)) g h))))
	     (unparse-exp (parse-exp (quote (let* ((a b)) b))))
	     (unparse-exp (parse-exp (quote (lambda x x y))))
	     (unparse-exp (parse-exp (quote (let ((x 1) 
						  (y (let () 
						       (let ((z t)) 
							 z)))) 
					      (+ z y)))))
	     (unparse-exp 
	      (parse-exp 
	       (quote (lambda () 
			(letrec ((foo (lambda (L) 
					(if (null? L) 
					    3 
					    (if (symbol? (car L)) 
						(cons (car L) 
						      (foo (cdr L))) 
						(foo (cdr L))))))) 
			  foo)))))
	     (unparse-exp (parse-exp (quote (lambda (x) 
					      (if (boolean? x) #(1 2 3 4) 1234)))))
	     (unparse-exp (parse-exp (quote (lambda x (car x)))))
	     (unparse-exp (parse-exp (quote (lambda (c) (if (char? c) string 12345)))))
	     (unparse-exp (parse-exp (quote (lambda (datum) 
					      (or (number? datum) 
						  (boolean? datum) 
						  (null? datum) 
						  (string? datum) 
						  (symbol? datum) 
						  (pair? datum) 
						  (vector? datum))))))
	     (unparse-exp 
	      (parse-exp 
	       (quote 
		(lambda (t) 
		  (let ((L (build-list t)) 
			(sum< (lambda (a b) 
				(< (cdr a) (cdr b)))) 
			(intsum? (lambda (x) 
				   (symbol? (car x))))) 
		    (car (genmax sum< 
				 (filter intsum? L))))))))
	     (unparse-exp (parse-exp (quote (letrec ((a (lambda () 
							  (b 2))) 
						     (b (lambda (x) 
							  (- x 4)))) 
					      (lambda () (a))))))
	     (unparse-exp (parse-exp (quote (let* ((a (lambda () (c 4))) 
						   (b a)) 
					      (lambda (b) (a))))))
	     (unparse-exp (parse-exp (quote (lambda x (cons a x)))))
	     (unparse-exp (parse-exp (quote (lambda x 
					      (let* ((a x) 
						     (b (cons x a))) 
						b)))))
	     (unparse-exp (parse-exp (quote (lambda (a b c) 
					      (let* ((dbl (lambda x 
							    (append x x))) 
						     (lst (dbl a b c))) 
						lst)))))
	     (unparse-exp (parse-exp (quote (lambda a 
					      (letrec ((stuff (lambda (b) 
								(if (null? b) (list) 
								    (cons (car b) 
									  (stuff (cdr b))))))) 
						(stuff a))))))
	     (unparse-exp (parse-exp (quote (lambda (a b c) 
					      (let* ((a b) 
						     (d (append a c))) 
						d)))))
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

(define error-equal?
  (lambda (correct-answer my-answer)
    (and (pair correct-answer) (pair (cdr correct-answer))
	 (pair my-answer) (pair (cdr my-answer))
	 (eq? (car correct-answer) '*error*)
	 (eq? (car my-answer) '*error*)
	 (eq? (cadr my-answer) (cadr correct-answer))
	 ; for parse problem, both should be 'parse-exp
	 )))

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
;  (display 'parse-errors) 
;  (test-parse-errors)
  (display 'parse-unparse) 
  (test-parse-unparse)
)

(define r run-all)
