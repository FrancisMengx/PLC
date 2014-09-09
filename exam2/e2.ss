; startng code for students

;#1 product-cps

; Provided code.  Do not modify it.
(define append-cps 
  (lambda (l1 l2 k)
    (if (null? l1)
  (k l2)
  (append-cps (cdr l1)
        l2
        (lambda (appended-cdr)
          (k (cons (car l1)
             appended-cdr)))))))

(define map-cps
  (lambda (proc-cps ls k)
    (if (null? ls)
  (k '())
  (proc-cps (car ls)
      (lambda (proced-car)
         (map-cps proc-cps (cdr ls)
        (lambda (mapped-cdr)
          (k (cons proced-car mapped-cdr)))))))))

(define list?-cps
  (lambda (ls k)
    (cond [(null? ls) (k #t)]
          [(not (pair? ls)) (k #f)]
          [else (list?-cps (cdr ls) k)])))

(define length-cps
  (lambda (ls k)
    (if (null? ls)
        (k 0)
        (length-cps (cdr ls)
                    (lambda  (len)
                      (k (+ 1 len)))))))

(define matrix?-cps
  (lambda (m k)
    (list?-cps 
     m
     (lambda (is-list?)
       (if (not is-list?)
           (k #f)
           (if (null? m)
               (k #f)
               (if (null? (car m))
       (k #f)
                   (andmap-cps 
                    list?-cps 
                    m
                    (lambda (full-of-lists?)
                      (if (not full-of-lists?)
                          (k #f)
                          (andmap-cps 
                           (lambda  (L k)
                             (length-cps 
                              L
                              (lambda  (length-L)
                                (length-cps 
                                 (car m)
                                 (lambda  (length-car)
                                   (k (= length-L length-car)))))))
                           (cdr m) k)))))))))))

(define andmap-cps
  (lambda (pred-cps ls k)
    (if (null? ls)
  (k #t)
  (pred-cps (car ls)
      (lambda (v)
        (if v
      (andmap-cps pred-cps
            (cdr ls)
                                    k)
      (k #f)))))))

; Write your code for #1 here.

(define (product-cps x y k)
  (if (null? y)
      (k '())
      (let loop ((x x) (m k))
        (if (null? x)
            (m '())
            (loop (cdr x) (lambda (v) 
              (map-cps (lambda (s k) (k (list (car x) s))) y (lambda (v2) 
                  (append-cps v v2 m)
                )
              )
            )
          )
        )
    )
  )
)



;#2 expand-lets.  Insert your datatype, parse,  and un-parse code. Modify that code as needed.
;                 Then write the expand-lets procedure.

(load "chez-init.ss") ; remove this isf using Dr. Scheme EoPL language

(define-datatype expression expression?  ; based on the simple expression grammar, EoPL-2 p6
  (num-exp
    (id number?)
  )
  (var-exp
    (id symbol?))
  (lambda-exp
    (arg sym-ls?)
    (body list?))
  (app-exp
    (rator expression?)
    (rand list?))
  (if-else-exp
    (condition expression?)
    (t-body expression?)
    (f-body expression?)
  )
  (if-exp
    (condition expression?)
    (body expression?)
  )
  (let-exp
    (type symbol?)
    (assign list?)
    (body list?)
  )
)
(define (sym-ls? arg)
  (if (or (symbol? arg) (list? arg))
      #t
      #f)
)
(define parse-exp
  (lambda (datum)
    (cond
      ((number? datum) (num-exp datum))
      ((symbol? datum) (var-exp datum))
      ((not (list? datum)) (eopl:error 'parse-exp
              "expression ~s is not a proper list" datum))
      ((pair? datum)
       (cond ((eqv? (car datum) 'lambda) (parse-lambda datum))
             ((eqv? (car datum) 'if) (parse-if datum))
             ((or (eqv? (car datum) 'let) (eqv? (car datum) 'let*) (eqv? (car datum) 'letrec)) (parse-let datum))
             (else (app-exp (parse-exp (car datum)) (map parse-exp (cdr datum)))))
       )
      (else (eopl:error 'parse-exp
              "Invalid concrete syntax ~s" datum)))))


(define (parse-lambda datum)
  (if (>= (length datum) 3)
      (if (list? (cadr datum))
          (lambda-exp (parse-lambda-args (cadr datum)) (map parse-exp (cddr datum)))
        (lambda-exp (cadr datum) (map parse-exp (cddr datum))))

      (eopl:error 'parse-exp
              "lambda-expression: incorrect length ~s" datum)
))

(define (parse-lambda-args arg)
  (if (symbol? arg) arg
      (if (andmap symbol? arg)
          arg
          (eopl:error 'parse-exp
              "lambda's formal arguments ~s must all be symbols" arg)))
)

(define (parse-if datum)
  (cond ((equal? (length datum) 3) (if-exp (parse-exp (cadr datum)) (parse-exp (caddr datum))))
        ((equal? (length datum) 4) (if-else-exp (parse-exp (cadr datum)) (parse-exp (caddr datum)) (parse-exp (cadddr datum))))
        (else (eopl:error 'parse-exp
              "if-expression ~s does not have (only) test, then, and else" datum))
  )
)

(define (parse-let datum)
  (cond ((>= (length datum) 3) (if (parse-let-assign  datum)
      (let-exp (car datum) (map (lambda (x) (list (car x) (parse-exp (cadr x)))) (cadr datum)) (map parse-exp (cddr datum)))
      (eopl:error 'parse-exp
              "~s-expression has incorrect length ~s" (car datum) datum)))
        (else (eopl:error 'parse-exp
              "~s-expression has incorrect length ~s" (car datum) datum))
  )
)
(define (parse-let-assign assign)
  (if (not (list? (cadr assign)))
      (eopl:error 'parse-exp
              "declarations in ~s-expression not a list ~s" (car assign) assign)
      (if (andmap list? (cadr assign))
          (if (andmap two? (cadr assign))
              (if (map parse-exp (map cadr (cadr assign))) 
                  (if (andmap symbol? (map car (cadr assign)))
                      #t
                      (eopl:error 'parse-exp
              "vars in ~s-exp must be symbols ~s" (car assign) assign))
              )
              (eopl:error 'parse-exp
              "declaration in ~s-exp must be a list of length 2 ~s" (car assign) assign))
          (eopl:error 'parse-exp
              "declaration in ~s-exp is not a proper list ~s" (car assign) assign)))
)

(define (two? list)
  (if (equal? (length list) 2)
      #t
      #f)
)

(define unparse-exp ; an inverse for parse-exp
  (lambda (exp)
    (cases expression exp
      (num-exp (id) id)
      (var-exp (id) id)
      (lambda-exp (arg body) 
        (append (list 'lambda arg)
          (if (list? (car body))
              (map unparse-exp body)
              (list (unparse-exp body)))))
      (if-else-exp (condition t-body f-body)
        (list 'if (unparse-exp condition) (unparse-exp t-body) (unparse-exp f-body))
      )
      (if-exp (condition body)
        (list 'if (unparse-exp condition) (unparse-exp body))
      )
      (let-exp (type assign body)
        (append (list type assign) (map unparse-exp body))
      )
      (app-exp (rator rand)
        (append (list (unparse-exp rator))
              (map unparse-exp rand))))))

(define (expand-lets exp)
  (cases expression exp
    (num-exp (id) (num-exp id))
    (var-exp (id) (var-exp id))
    (lambda-exp (arg body) (if (list? (car body))
        (lambda-exp arg (map expand-lets body))
        (lambda-exp arg (expand-lets body))))
    (if-else-exp (condition t-body f-body)
      (if-else-exp condition (expand-lets t-body) (expand-lets f-body))
    )
    (if-exp (condition body)
      (if-exp condition (expand-lets body))
    )
    (let-exp (type assign body)
      (app-exp (lambda-exp (map car assign) (expand-lets (car body))) (map (lambda (k) (expand-lets (cadr k))) assign))
    )
    (app-exp (rator rand)
      (app-exp (expand-lets rator) (map expand-lets rand))
    )
  )
)

;#3  lyst

; provided code.  Do not modify it
(define make-stack
 (lambda ()
  (let ([stk '()])
   (lambda (msg  . args ) 
    (case msg
      [(empty?) (null? stk)]
      [(push)   (set! stk (cons (car args) stk))]
      [(pop)    (if (null? stk)
        (errorf 'pop "attempt to pop empty stack")
        (let ([top (car stk)])
          (set! stk (cdr stk))
          top))]
      [(top)    (car stk)]
      [(display) (list '*stack* stk)]
      [else (errorf 'stack "illegal message to stack object: ~a" msg)])))))

;-------------------------Write your make-lyst code (and any helpers it needs) here
(define (make-lyst)
  (let ([lyst-left (make-stack)] [lyst-right (make-stack)])
    (lambda (msg . args)
      (case msg
        [(length) (+ (length (cadr (lyst-left 'display))) (length (cadr (lyst-right 'display))))]
        [(get-current-item) (lyst-right 'top)]
        [(insert-at-current-pos) (lyst-right 'push (car args))]
        [(get-current-pos) (length (cadr (lyst-left 'display)))]
        [(shift) (let helper ([count (car args)])
          (if (positive? count)
              (and (let ((p (lyst-right 'pop)))
                (lyst-left 'push p)) (helper (- count 1)))
              (if (negative? count)
                  (and (let ((p (lyst-left 'pop)))
                  (lyst-right 'push p)) (helper (+ count 1)))
                  )
          ))]
        [(goto) (let goto-helper ([pos (- (length (cadr (lyst-left 'display))) (car args))])
            (if (positive? pos)
                (and (let ((p (lyst-left 'pop)))
                (lyst-right 'push p)) (goto-helper (- pos 1)))
                (if (negative? pos)
                    (and (let ((p (lyst-right 'pop)))
                (lyst-left 'push p)) (goto-helper (+ pos 1)))
                    ))
          )]
        [(replace-current-item) (and (lyst-right 'pop) (lyst-right 'push (car args)))]
        [(remove-current) (lyst-right 'pop)]
      ) 
    )
  )
)


