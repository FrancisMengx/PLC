
(define (test-cross-product)
    (let ([correct '((-18 10 -3) (0 0 0))]
          [answers 
            (list 
              (cross-product '(1 3 4) '(3 6 2)) 
              (cross-product '(1 2 3) '(2 4 6))
            )])
    (display-results correct answers equal?)))



(define (test-parallel?)
  (let ([correct '(#f #t)]
        [answers 
          (list
	   (parallel? '(1 3 4) '(3 6 2))
	   (parallel? '(1 2 3) '(2 4 6))
	    )])
    (display-results correct answers equal?)))

(define (test-collinear?)
  (let ([correct '(#t #f)]
        [answers 
          (list
	   (collinear? '(1 2 3) '(4 5 6) '(10 11 12))
	   (collinear? '(1 2 3) '(4 5 6) '(10 11 13))
          )])
    (display-results correct answers equal?)))

(define (test-nearest-point)
  (let ([correct '((1 4 2) (1 4 2))]
        [answers 
          (list
	   (nearest-point '(1 4 3) '((1 5 2) (1 4 2) (1 6 13)))
	   (nearest-point '(1 4 3) '((1 4 2) (1 5 2) (1 6 13)))
	  )])
    (display-results correct answers set-equals?)))

            


(define (test-intersection)
  (let ([correct '((i h) () () ())]
        [answers 
          (list
	   (intersection '(a b d e f h i j) '(h q r i z))
	   (intersection '(g h i) '(j k l))
	   (intersection '(a p t) '())
	   (intersection '() '(g e t))
	  )])
    (display-results correct answers set-equals?)))

(define (test-subset?)
  (let ([correct ' (#t #f #f #t #t #t)]
        [answers 
          (list
	    (subset? '(c b) '(a d b e c))
	    (subset? '(c b) '(a d b e))
	    (subset? '(c b) '())
	    (subset? '(c b) '(b c))
	    (subset? '() '())
	    (subset? '() '(x y))
	  )])
    (display-results correct answers equal?)))

(define (test-relation?)
  (let ([correct '(#f #t #t #t #f #f #f #f )]
        [answers 
          (list
	   (relation? 5) 
	   (relation? '())
	   (relation? '((a b) (b c))) 
	   (relation? '((a b) (b a) (a a) (b b))) 
	   (relation? '((a b) (b c d))) 
	   (relation? '((a b) (c d) (a b))) 
	   (relation? '((a b) (c d) "5")) 
	   (relation? '((a b) . (b c))) 
	  )])
    (display-results correct answers equal?)))

(define (test-domain)
  (let ([correct '((2 3 1) ())]
        [answers 
          (list
	    (domain '((1 2) (3 4) (1 3) (2 7) (1 6)))
	    (domain '())
	  )])
    (display-results correct answers set-equals?)))

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
  (display 'cross-product) 
  (test-cross-product)
  (display 'test-parallel?) 
  (test-parallel?)
  (display 'collinear?) 
  (test-collinear?)
  (display 'nearest-point) 
  (test-nearest-point)    
  (display 'intersection) 
  (test-intersection)
  (display 'subset?) 
  (test-subset?)  
  (display 'relation?) 
  (test-relation?)  
  (display 'domain) 
  (test-domain)  
  
)

(define r run-all)
