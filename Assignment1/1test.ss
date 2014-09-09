

(define (test-Fahrenheit->Celsius)
    (let ([correct '(0 -40 9 -160/9)]
          [answers 
            (list 
              (Fahrenheit->Celsius 32) 
              (Fahrenheit->Celsius -40)
              (Fahrenheit->Celsius 241/5)
              (Fahrenheit->Celsius 0))])
    (display-results correct answers equal?)))

(define (test-interval-contains?)
  (let ([correct '(#t #t #f #f #t)]
        [answers 
         (list (interval-contains? '(5 8) 6) 
               (interval-contains? '(5 8) 5)       
               (interval-contains? '(5 8) 4)
               (interval-contains? '(5 5) 14)
               (interval-contains? '(5 5) 5))])
    (display-results correct answers equal?)))


(define (test-interval-intersects?)
  (let ([correct '(#t #t #t #t #t #t #f #f)]
        [answers 
          (list
            (interval-intersects? '(1 4) '(2 5))
           	(interval-intersects? '(2 5) '(1 4))
           	(interval-intersects? '(1 4) '(4 5))
           	(interval-intersects? '(4 5) '(1 4))
           	(interval-intersects? '(2 5) '(1 14))
           	(interval-intersects? '(1 14) '(2 5))
           	(interval-intersects? '(1 3) '(12 17))
           	(interval-intersects? '(12 17) '(1 3)))])
    (display-results correct answers equal?)))



(define (test-interval-union)
  (let ([correct '(((1 6)) ((1 5)) ((1 6)) ((1 5)) ((1 5)) 
                   ((1 5) (15 25)) ((25 25) (5 5)) ((5 5)))]
        [answers 
          (list
            (interval-union '(1 5) '(2 6))
            (interval-union '(1 5) '(2 4))
            (interval-union '(2 6) '(1 5))
            (interval-union '(1 5) '(5 5))
            (interval-union '(5 5) '(1 5))
            (interval-union '(1 5) '(15 25))
            (interval-union '(5 5) '(25 25))
            (interval-union '(5 5) '(5 5)))])
    (display-results correct answers 
      (lambda (x y) (andmap set-equals? x y)))))
            


(define (test-divisible-by-7?)
  (let ([correct '(#f #t #t #f)]
        [answers 
          (list
            (divisible-by-7? 12)
            (divisible-by-7? 42) 
            (divisible-by-7? 0) 
            (divisible-by-7? 19))])
    (display-results correct answers equal?)))

(define (test-ends-with-7?)
  (let ([correct '(#f #t)]
        [answers 
          (list
            (ends-with-7? 172) 
            (ends-with-7? 4412368939284856837))])
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
  (display 'test-Fahrenheit->Celsius) 
  (test-Fahrenheit->Celsius)
  (display 'interval-contains) 
  (test-interval-contains?)
  (display 'interval-intersects?) 
  (test-interval-intersects?)
  (display 'test-interval-union) 
  (test-interval-union)
  (display 'divisible-by-7?) 
  (test-divisible-by-7?)  
  (display 'ends-with-7?) 
  (test-ends-with-7?)  
)

(define r run-all)
