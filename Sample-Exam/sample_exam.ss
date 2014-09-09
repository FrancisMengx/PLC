(define compose2 
 (lambda (f g) 
 (lambda (x) 
 (f (g x)))))

(define (nth-cdr n)
(cond ((equal? n 0) (lambda (x) x))
	  (else (compose2 (nth-cdr (- n 1)) cdr))
))

(define (up lst)
	(if (null? lst)
	    '()
	    (if (list? (car lst))
	        (append (car lst) (up (cdr lst)))
	        (up (cdr lst))))
)

(define (pair-up ls)
  (if (null? ls)
      '()
      (if (null? (cdr ls))
          (cons (cons (car ls) (cons (car ls) '())) '())
          (cons (cons (car ls) (cons (cadr ls) '())) (pair-up (cddr ls))))))

(define (merge-list lstA lstB)
  (cond
    [(null? lstA) lstB]
    [(null? lstB) lstA]
    [(< (car lstA) (car lstB))  
         (cons (car lstA)  (merge-sort (cdr lstA) lstB))]
    [else (cons (car lstB) (merge-sort lstA (cdr lstB)))]))

(define (devide-list ls)
 (if (null? ls)
     '()
     (cons (cons (car ls) '()) (devide-list (cdr ls)))))

(define (merge-sort ls)
  (let merge-n-sort ([dls (devide-list ls)])
  	(if (equal? (length dls) 1)
  	    (car dls)
  	    )))

(define (list-set lst n x)
  (if (zero? n)
      (append (cons x '()) lst)
      (append (cons (car lst) '()) (list-set (cdr lst) (- n 1) x))))

