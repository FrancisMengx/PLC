

(define (test-union)
    (let ([correct '((a b c d e f g h j) (a b c d e) (a b c) ())]
          [answers 
            (list 
	     (union '(a b d e f h j) '(f c e g a))
	     (union '(a b c) '(d e))
	     (union '(a b c) '())
	     (union '() '())
            )])
    (display-results correct answers set-equals?)))



(define (test-reflexive?)
  (let ([correct '(#t #t #t #t)]
        [answers 
          (list
	   (reflexive? '((a a) (b b) (c d) (b c) (c c) (e e) (c a) (d d)))
           (not (reflexive? '((a a) (b b) (c d) (b c) (e e) (c a) (d d))))
           (not (reflexive? '((a a) (c d) (b c) (c c) (e e) (c a) (d d))))
	   (reflexive? '())
	   )])
    (display-results correct answers eq?)))

(define (test-matrix-ref)
  (let ([correct '(2 3 5)]
        [answers 
          (list
	   (matrix-ref '((1 2 3 4 5) (4 3 2 1 5) (5 4 3 2 1))  2 3)
	   (matrix-ref '((1 2 3 4) (4 3 2 1))  1 1)
	   (matrix-ref '((1 2 3 4 5) (4 3 2 1 5) (5 4 3 2 1))  0 4)
          )])
    (display-results correct answers equal?)))

(define (test-matrix?)
  (let ([correct '(#f #f #f #t #f #f #t #f)]
        [answers 
	 (list
	  (matrix? 5)
	  (matrix? "matrix")
	  (matrix? '(1 2 3))
	  (matrix? '((1 2 3)(4 5 6)))
	  (matrix? '#((1 2 3)(4 5 6)))
	  (matrix? '((1 2 3)(4 5 6)(7 8)))
	  (matrix? '((1)))
	  (matrix? '(()()()))
	 )])
    (display-results correct answers equal?)))

            


(define (test-matrix-transpose)
  (let ([correct '(((1 4) (2 5) (3 6)) ((1) (2) (3)) ((1 2 3)))]
        [answers 
          (list
	   (matrix-transpose '((1 2 3) (4 5 6)))
	   (matrix-transpose '((1 2 3)))
	   (matrix-transpose '((1) (2) (3)))
	  )])
    (display-results correct answers equal?)))

(define (test-last)
  (let ([correct '( 4 c (()()))]
        [answers 
          (list
	   (last '(1 5 2 4))
	   (last '(c)) 
	   (last '(() (()) (()())))
	  )])
    (display-results correct answers equal?)))

(define (test-all-but-last)
  (let ([correct '((1 5 2) () (() (())))]
        [answers 
          (list
	   (all-but-last '(1 5 2 4))
	   (all-but-last '(c)) 
	   (all-but-last '(() (()) (()())))	   
	  )])
    (display-results correct answers equal?)))


;;-----------------------------------------------

(define display-results
  (lambda (correct results test-procedure?)
     (display ": ")
     (pretty-print 
      (if (andmap test-procedure? correct results)
          'All-correct
          `(correct: ,correct yours: ,results)))))

(define set-equals?  ; are these list-of-symbols equal when
  (lambda (s1 s2)    ; treated as sets?
    (if (or (not (list? s1)) (not (list? s2)))
        #f
        (not (not (and (is-a-subset? s1 s2) (is-a-subset? s2 s1)))))))

(define is-a-subset?
  (lambda (s1 s2)
    (andmap (lambda (x) (member x s2))
      s1)))


;; You can run the tests individually, or run them all
;; by loading this file (and your solution) and typing (r)

(define (run-all)
  (display 'union) 
  (test-union)
  (display 'test-reflexive?) 
  (test-reflexive?)
  (display 'matrix-ref) 
  (test-matrix-ref)
  (display 'matrix?) 
  (test-matrix?)    
  (display 'matrix-transpose) 
  (test-matrix-transpose)
  (display 'last) 
  (test-last)  
  (display 'all-but-last) 
  (test-all-but-last)  
  
)

(define r run-all)
