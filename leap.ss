(define (leap y)
  (if (or (and (= (remainder y 4) 0) (not (= (remainder y 100) 0))) (= (remainder y 400) 0))
      #t
      #f))