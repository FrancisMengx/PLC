;(load "chez-init.ss") ; remove this isf using Dr. Scheme EoPL language

(define-datatype expression expression?  ; based on the simple expression grammar, EoPL-2 p6
  (vec-exp
    (id vector?)
  )
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
  (set-exp
    (var symbol?)
    (body expression?)
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
      ((vector? datum) (vec-exp datum))
      ((number? datum) (num-exp datum))
      ((symbol? datum) (var-exp datum))
      ((not (list? datum)) (eopl:error 'parse-exp
              "expression ~s is not a proper list" datum))
      ((pair? datum)
       (cond ((eqv? (car datum) 'lambda) (parse-lambda datum))
             ((eqv? (car datum) 'if) (parse-if datum))
             ((or (eqv? (car datum) 'let) (eqv? (car datum) 'let*) (eqv? (car datum) 'letrec)) (parse-let datum))
             ((eqv? (car datum) 'set!) (parse-set datum))
             (else (app-exp (parse-exp (car datum)) (map parse-exp (cdr datum)))))
       )
      (else (eopl:error 'parse-exp
              "Invalid concrete syntax ~s" datum)))))
(define (parse-set datum)
  (if (equal? (length datum) 3)
      (set-exp (cadr datum) (parse-exp (caddr datum)))
      (eopl:error 'parse-exp
              "set! expression ~s does not have (only) variable and expression" datum))
)


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
      (let-exp (car datum) (cadr datum) (map parse-exp (cddr datum)))
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
      (vec-exp (id) id)
      (num-exp (id) id)
      (var-exp (id) id)
      (lambda-exp (arg body) 
        (append (list 'lambda arg)
          (map unparse-exp body)))
      (if-else-exp (condition t-body f-body)
        (list 'if (unparse-exp condition) (unparse-exp t-body) (unparse-exp f-body))
      )
      (if-exp (condition body)
        (list 'if (unparse-exp condition) (unparse-exp body))
      )
      (let-exp (type assign body)
        (append (list type assign) (map unparse-exp body))
      )
      (set-exp (var body)
        (list 'set var (unparse-exp body))
      )
      (app-exp (rator rand)
        (append (list (unparse-exp rator))
              (map unparse-exp rand))))))

(define occurs-free? ; in parsed expression
  (lambda (var exp)
    (cases expression exp
      (var-exp (id) (eqv? id var))
      (lambda-exp (id body)
        (and (not (eqv? id var))
             (occurs-free? var body)))
      (app-exp (rator rand)
        (or (occurs-free? var rator)
            (occurs-free? var rand))))))


