;Francis Meng Assignment 7
;Question1
(define (invert list)
  (if (null? list)
      '()
      (cons (reverse (car list)) (invert (cdr list)))))

;Question2
(define (vector-index pred v)
  (let search ([pred pred] [vector v] [index 0])
    (if (>= index (vector-length vector))
        #f
        (if (pred (vector-ref vector index))
            index
            (search pred vector (+ index 1))))))

;Question3
(define (vector-append-list v lst)
(if (null? lst)
    v
    (let append-list ([ls lst] [index 0] [result (make-vector (+ (vector-length v) (list-length lst)))])
      (if (>= index (vector-length v))
          (if (null? ls)
              result
              (vector-set! result index (car ls)))
          (vector-set! result index (vector-ref v index))) (if (>= index (vector-length v))
              (if (null? ls)
                  result
                  (append-list (cdr ls) (+ 1 index) result))
              (append-list ls (+ 1 index) result))))
)

(define (list-length ls)
    (if (null? ls)
        0
        (+ 1 (list-length (cdr ls)))))

;Question4
(define (qsort pred ls)
  (if (null? ls)
      '()
      (let ([filtered-ls (filter-list pred ls)])
    (append (append (qsort pred (car (cdr filtered-ls))) (cons (car ls) '())) (qsort pred (car filtered-ls))))
  )
)

(define (filter-list op ls)
  (let filter-single ([l (cdr ls)] [pos '()] [neg '()] [pivot (car ls)])
    (if (null? l)
        (list pos neg)
        (if (op pivot (car l))
            (filter-single (cdr l) (append pos (cons (car l) '())) neg pivot)
            (filter-single (cdr l) pos (append neg (cons (car l) '())) pivot))))
)

;Question5
(define (connected? g)
  (cond ((null? g) #t)
      ((null? (cdr g)) #t)
      ((null? (cadr (car g))) #f)
      (else 
        (let check-connectivity ([group (cons (append (append '() (cons (car (car g)) '())) (cadr (car g))) '())]
                     [cur-g (cdr g)]
                    )
          (cond ((null? cur-g) (if (> (list-length group) 1)
              #f
              #t))
                ((null? (cadr (car cur-g))) #f)
                (else (check-connectivity (intersect-union group (append (cons (car (car cur-g)) '()) (cadr (car cur-g)))) (cdr cur-g)))
      )
  )
)
  )
)
(define (add-set set e)
  (if (member e set)
      set
      (append set (cons e '()))))

(define (intersect-union ls1 ls2)
  (if (null? ls2)
      ls1
      (if (null? (cdr ls1))
      (if (intersect? (car ls1) ls2)
          (cons (union (car ls1) ls2) '())
          (cons (car ls1) (cons ls2 '())))
      (if (intersect? (car ls1) ls2)
          (intersect-union (cons (union (car ls1) ls2) '()) (cadr ls1))
          (cons (car ls1) (intersect-union (cdr ls1) ls2)))))
  
)

(define (union ls1 ls2)
  (if (null? ls2)
      ls1
      (if (member (car ls2) ls1)
          (union ls1 (cdr ls2))
          (union (append ls1 (cons (car ls2) '())) (cdr ls2)))))


(define (intersect? ls1 ls2)
  (if (null? (cdr ls1))
      (if (member (car ls1) ls2)
          #t
          #f)
      (if (member (car ls1) ls2)
          #t
          (intersect? (cdr ls1) ls2))))
  

(define (intersection s1 s2)
  (if (equal? s1 '())
      '()
    (if (member (car s1) s2)
        (cons (car s1) (intersection (cdr s1) s2))
        (intersection (cdr s1) s2)))
 )

(define (in-list? ls el)
    (if (null? ls)
        #f
        (if (equal? (car ls) el)
            #t
            (in-list? (cdr ls) el)))
)


;Question6
(define (reverse-it ls)
  (cond ((null? ls) '())
        ((null? (cdr ls)) ls)
        (else (append (reverse-it (cdr ls)) (cons (car ls) '())))
    ))

;Question7
;Question 7.1
(define (empty-BST)
  '()
)
;Question 7.2
(define (empty-BST? obj)
  (equal? obj (empty-BST)))
;Question 7.3
(define (BST-insert num bst)
  (if (null? bst)
      (cons num (list '() '()))
      (if (> num (car bst))
      (list (car bst) (cadr bst) (BST-insert num (caddr bst)))
      (if (equal? num (car bst))
          bst
          (list (car bst) (BST-insert num (cadr bst)) (caddr bst))))))
;Question 7.4
(define (BST-inorder bst)
  (cond ((null? bst) '())
        ;((and (null? (cadr bst)) (null? (caddr bst))) (cons (car bst) '()))
        (else (append (append (BST-inorder (cadr bst)) (cons (car bst) '())) (BST-inorder (caddr bst))))
  )
)
;Question 7.5
(define (BST? bst)
  (cond ((null? bst) #t)
    ((not (list? bst)) #f)
    ((null? (cdr bst)) #f)
    ((null? (cddr bst)) #f)
      ((not (list? (cadr bst))) #f)
    ((not (list? (caddr bst))) #f)
    ((not (number? (car bst))) #f)
    ((is-sorted? (BST-inorder bst)) #t)
    (else #f))
)
(define (is-sorted? ls)
  (if (null? (cdr ls))
      #t
      (if (< (car ls) (cadr ls))
          (is-sorted? (cdr ls))
          #f)))
;Question 7.6
(define (BST-element tree)
(car tree)
)
(define (BST-left tree)
  (cadr tree))
(define (BST-right tree)
  (caddr tree))
;Question 7.7
(define (BST-insert-nodes bst nums)
  (if (null? nums)
      bst
      (BST-insert-nodes (BST-insert (car nums) bst) (cdr nums))))
;Question 7.8
(define (BST-contains? bst num)
(if (null? bst)
    #f
    (if (equal? (car bst) num)
        #t
        (if (> num (car bst))
            (BST-contains? (caddr bst) num)
            (BST-contains? (cadr bst) num)))))

;Question8
(define (map-by-position fn-list arg-list)
  (map (lambda (x y) (x y)) fn-list arg-list)
)

