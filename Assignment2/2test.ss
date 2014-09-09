

(define (test-choose)
    (let ([correct '(1 3 210)]
          [answers 
            (list 
              (choose 0 0) 
              (choose 3 2)
              (choose 10 6)
            )])
    (display-results correct answers equal?)))

(define (test-make-range)
  (let ([correct '(() (0 1 2 3 4) (5 6 7 8) (25 26 27 28 29) (31) ())]
        [answers 
         (list (make-range 0 0) 
               (make-range 0 5)       
               (make-range 5 9)
	       (make-range 25 30)
               (make-range 31 32)
               (make-range 7 4))])
    (display-results correct answers equal?)))


(define (test-set?)
  (let ([correct '(#t #t #t #t #t #f #t #f)]
        [answers 
          (list
            (and (set? '()) (not (set? '(1 1))))
	    (and (set? '(1 2 3) ) (not (set? '(1 2 1))))
	    (and (set? '(1 (2 3) (3 2) 5)) (not (set? '(1 3 1 2)))) 
	    (and (not (set? '(1 (2 3) (3 2) 5 (3 2))))  (set? '())) 
	    (set? '(r o s e - h u l m a n))
	    (set? '(c o m p u t e r s c i e n c e))
	    (set? '((i) (a m) (a) (s e t)))
	    (set? '((i) (a m) (n o t) (a) (s e t) (a m) (i)))
	    )])
    (display-results correct answers equal?)))

(define (test-sum-of-squares)
  (let ([correct '(84 0)]
        [answers 
          (list
            (sum-of-squares '(1 3 5 7)) 
            (sum-of-squares '()))])
    (display-results correct answers equal?)))

(define (test-make-vec-from-points)
  (let ([correct '((2 3 -2) (4 -9 -2))]
        [answers 
          (list
            (make-vec-from-points '(1 3 4) '(3 6 2))
            (make-vec-from-points '(-1 3 4) '(3 -6 2)))])
    (display-results correct answers 
      (lambda (x y) (andmap set-equals? x y)))))
            


(define (test-dot-product)
  (let ([correct '(25 0)]
        [answers 
          (list
            (dot-product '(1 -3 5) '(2 4 7))
            (dot-product '(1 5 3) '(3 3 -6)))])
    (display-results correct answers equal?)))


(define (test-vec-length)
  (let ([correct '(13 0)]
        [answers 
          (list
            (vec-length '(3 -4 12))
            (vec-length '(0 0 0)))])
    (display-results correct answers equal?)))

(define (test-distance)
  (let ([correct '(13 29 0)]
        [answers 
          (list
            (distance '(4 7 8) '(7 11 -4) )
            (distance '(3 1 2) '(15 -15 23))
	    (distance '(4 7 8) '(4 7 8)))])
    (display-results correct answers equal?)))



;;-----------------------------------------------

(define display-results
  (lambda (correct results test-procedure?)
     (display ": ")
     (pretty-print 
      (if (test-procedure? correct results)
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
  (display 'test-choose) 
  (test-choose)
  (display 'make-range) 
  (test-make-range)
  (display 'set?) 
  (test-set?)
  (display 'sum-of-squares) 
  (test-sum-of-squares)    
  (display 'test-make-vec-from-points) 
  (test-make-vec-from-points)
  (display 'dot-product) 
  (test-dot-product)  
  (display 'vec-length) 
  (test-vec-length)  
  (display 'distance) 
  (test-distance)  
  
)

(define r run-all)
