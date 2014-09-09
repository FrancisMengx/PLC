;:  Single-file version of the interpreter.
;; Easier to submit to server, probably harder to use in the development process

; (load "chez-init.ss") 



;-------------------+
;                   |
;    DATATYPES      |
;                   |
;-------------------+

;; Parsed expression datatypes
(define-datatype expression expression?  ; based on the simple expression grammar, EoPL-2 p6
  [var-exp      (id symbol?)]
  [val-exp      (val integer?)]
  [lit-exp          (datum (lambda (x) (ormap (lambda (pred) (pred x)) (list number? vector? boolean? symbol? string? pair? null?))))]
  [lambda-exp     (args symbol?)
            (body (list-of expression?))]
  [lambda-list-exp  (args pair?)
            (body (list-of expression?))]
  [let-exp      (assign list?)
            (body (list-of expression?))]
  [app-exp      (rator expression?)
            (rands (list-of expression?))]
  [prim-exp     (proc proc-val?)
            (args list?)]
  [if-exp       (con expression?)
            (body expression?)]
  [if-else-exp    (con expression?)
            (t-body expression?)
            (f-body expression?)]
  [set!-exp     (var symbol?)
            (val expression?)])
  

; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc    (name (lambda (x) (member x *prim-proc-names*)))]
  [closure    (vars (list-of symbol?))
          (body (list-of expression?))
          (env environment?)])

;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  [empty-env-record]
  [extended-env-record  (syms (list-of symbol?))
              (vals (list-of scheme-value?))
              (env environment?)])
;-------------------+
;                   |
;    PARSER         |
;                   |
;-------------------+


; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define parse-exp
  (lambda (datum)
    (cond   [(member datum *prim-proc-names*)   (lit-exp (prim-proc datum))]
        [(integer? datum)   (val-exp datum)]
        [(symbol? datum)  (var-exp datum)]
        [(null? datum)    (lit-exp datum)]
        [(vector? datum)  (lit-exp datum)]
        [(boolean? datum)   (lit-exp datum)]
        [(number? datum)  (lit-exp datum)]
        [(string? datum)  (lit-exp datum)]
        [(pair? datum)    (cond   [(eqv? (car datum) 'lambda)         (parse-lambda-exp datum)]
                      [(eqv? (car datum) 'set!)           (parse-set!-exp datum)]
                      [(eqv? (car datum) 'if)           (parse-if-exp datum)]
                      [(eqv? (car datum) 'let)            (parse-let-exp datum)]
                      [(eqv? (car datum) 'quote)          (lit-exp (cadr datum))]
                      [(member (car datum) *prim-proc-names*)   (prim-exp (prim-proc (car datum)) (map parse-exp (cdr datum)))]
                      [(list? datum)                (app-exp (parse-exp (car datum)) (map parse-exp (cdr datum)))]
                      [else                     (eopl:error 'parse-exp "expression ~s is not a proper list" datum)])]
        [else       (eopl:error 'parse-exp "Invalid concrete syntax ~s" datum)])))

(define parse-lambda-exp
  (lambda (exp)
    (if (> (length exp) 2) 
      (if (symbol? (cadr exp)) (lambda-exp (cadr exp) (map parse-exp (cddr exp)))
        (lambda-list-exp (parse-lambda-args (cadr exp)) (map parse-exp (cddr exp))))
      (eopl:error 'parse-exp "lambda-expression: incorrect length ~s" exp))))

(define parse-lambda-args
  (lambda (args)
    (if (andmap symbol? args) args
    (eopl:error 'parse-exp "lambda's formal arguments ~s must all be symbols" args))))

(define parse-if-exp
  (lambda (exp)
    (cond   [(= (length exp) 3)   (if-exp (parse-exp (cadr exp)) (parse-exp (caddr exp)))]
        [(= (length exp) 4)   (if-else-exp (parse-exp (cadr exp)) (parse-exp (caddr exp)) (parse-exp (cadddr exp)))]
        [else           (eopl:error 'parse-exp "if-expression ~s does not have (only) test, then, and else" exp)])))

(define parse-set!-exp
  (lambda (exp)
    (if (= (length exp) 3) (set!-exp (cadr exp) (parse-exp (caddr exp)))
      (eopl:error 'parse-exp "set! expression ~s does not have (only) variable and expression" exp))))

(define parse-let-exp
  (lambda (exp)
    (if (> (length exp) 2) (let-exp (parse-let-args (cadr exp)) (map parse-exp (cddr exp)))
      (eopl:error 'parse-exp "~s-expression has incorrect length ~s" exp))))

(define parse-let*-exp
  (lambda (exp)
    (if (> (length exp) 2) (let*-exp (parse-let-args (cadr exp)) (map parse-exp (cddr exp)))
      (eopl:error 'parse-exp "~s-expression has incorrect length ~s" exp))))

(define parse-letrec-exp
  (lambda (exp)
    (if (> (length exp) 2) (letrec-exp (parse-let-args (cadr exp)) (map parse-exp (cddr exp)))
      (eopl:error 'parse-exp "~s-expression has incorrect length ~s" exp))))

(define parse-let-args
  (lambda (args)
    (cond   [(not (list? args))                 (eopl:error 'parse-exp "decls: not all proper lists: ~s" args)]
        [(not (andmap list? args))              (eopl:error 'parse-exp "decls: not all proper lists: ~s" args)]
        [(not (andmap (lambda (l) (= 2 (length l))) args))  (eopl:error 'parse-exp "declaration in ~s-exp must be a list of length 2 ~s" args)]
        [(not (andmap (lambda (l) (symbol? (car l))) args)) (eopl:error 'parse-exp "decls: first members must be symbols: ~s" args)]
        [else                         (map list (map var-exp (map car args)) (map parse-exp (map cadr args)))])))





;-------------------+
;                   |
;   ENVIRONMENTS    |
;                   |
;-------------------+





; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
	     (if (number? list-index-r)
		 (+ 1 list-index-r)
		 #f))))))

(define apply-env
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (cases environment env
      (empty-env-record ()
        (fail))
      (extended-env-record (syms vals env)
	(let ((pos (list-find-position sym syms)))
      	  (if (number? pos)
	      (succeed (list-ref vals pos))
	      (apply-env env sym succeed fail)))))))








;-----------------------+
;                       |
;   SYNTAX EXPANSION    |
;                       |
;-----------------------+



; To be added later









;-------------------+
;                   |
;   INTERPRETER    |
;                   |
;-------------------+

; top-level-eval evaluates a form in the global environment
(define *prim-proc-names* '(+ - * / set-car! set-cdr! vector->list list->vector length symbol? procedure? number? pair? vector? list? equal? eq? null? zero? add1 sub1 cons = >= <= not list quote car cdr cadr cdar cddr caar caaar caadr cadar cdaar caddr cdadr cddar cdddr))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))
(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
  (eval-exp form)))
  ; eval-exp is the main component of the interpreter
(define env init-env)
(define eval-exp
  (lambda (exp)
    (cases expression exp
      [lit-exp (datum)        datum]
      [val-exp (datum)        datum]
      [var-exp (id)         (apply-env env id; look up its value.
                        (lambda (x) x) ; procedure to call if id is in the environment 
                            (lambda () (eopl:error 'apply-env "variable not found in environment: ~s" id)))]
      [app-exp (rator rands)      (let  ([proc-value (eval-exp rator)]
                          [args (eval-rands rands)])
                        (apply-proc proc-value args))]
      [prim-exp (proc args)       (apply-proc proc (map eval-exp args))]
      [if-exp (con body)        (if (eval-exp con) (eval-exp body))]
      [if-else-exp (con t f)      (eval-exp (if (eval-exp con) t f))]
      [let-exp (assign body)      (set! env (extend-env (map cadr (map car assign)) (map eval-exp (map cadr assign)) env))
                      (eval-bodies body)]
      [lambda-list-exp (args body)  (closure args body env)]
      [lambda-exp (args body)  (closure args body env)]
      [else               (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

(define eval-bodies
  (lambda (b)
    (cond   [(null? (cdr b))  (eval-exp (car b))]
        [else         (eval-exp (car b))
                  (eval-bodies (cdr b))])))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands)
    (map eval-exp rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
      [closure (vars body env) (if (symbol? vars)
                                  (apply-closure (list args) (list vars) body env)
                                  (if (list? vars)
                                      (apply-closure args vars body env)
                                      (let ([format (format-lambda-pair vars args)])
                                        (apply-closure (cadr format) (car format) body env))))]
      ; You will add other cases
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define (apply-closure args vars body envi)
  (begin (set! env (extend-env vars args envi)) (eval-bodies body))
)

(define (format-lambda-pair vars args)
  (if (not (pair? (cdr vars)))
      (list (list (car vars) (cdr vars)) (list (car args) (cdr args)))
      (let ([result (format-lambda-pair (cdr vars) (cdr args))])
        (list (cons (car vars) (car result)) (cons (car args) (cadr result))))
      ))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
      [(+)      (apply + args)]
      [(-)      (apply - args)]
      [(*)      (apply * args)]
      [(/)      (apply / args)]
      [(add1)     (+ (1st args) 1)]
      [(sub1)     (- (1st args) 1)]
      [(cons)     (cons (1st args) (2nd args))]
      [(=)      (= (1st args) (2nd args))]
      [(>=)     (>= (1st args) (2nd args))]
      [(<=)     (<= (1st args) (2nd args))]
      [(not)      (not (1st args))]
      [(list)     args]
      [(quote)    args]
      [(zero?)    (zero? (1st args))]
      [(null?)    (null? (1st args))]
      [(eq?)      (apply eq? args)]
      [(equal?)   (apply equal? args)]
      [(pair?)    (pair? (1st args))]
      [(list?)    (list? (1st args))]
      [(vector?)    (vector? (1st args))]
      [(number?)    (number? (1st args))]
      [(symbol?)    (symbol? (1st args))]
      [(procedure?) (if (proc-val? (1st args)) #t (procedure? (1st args)))]
      [(length)   (length (1st args))]
      [(list->vector) (list->vector (1st args))]
      [(vector->list) (vector->list (1st args))]
      [(set-car!)   (set-car! (1st args) (2nd args))]
      [(set-cdr!)   (set-cdr! (1st args) (2nd args))]
      [(car)      (car (1st args))]
      [(cdr)      (cdr (1st args))]
      [(caar)     (caar (1st args))]
      [(cadr)     (cadr (1st args))]
      [(cddr)     (cddr (1st args))]
      [(cdar)     (cdar (1st args))]
      [(caddr)    (caddr (1st args))]
      [(cdadr)    (cdadr (1st args))]
      [(cddar)    (cddar (1st args))]
      [(caadr)    (caadr (1st args))]
      [(cdaar)    (cdaar (1st args))]
      [(cadar)    (cadar (1st args))]
      [(caaar)    (caaar (1st args))]
      [(cdddr)    (cdddr (1st args))]
      [else     (error 'apply-prim-proc "Bad primitive procedure name: ~s" prim-proc)])))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (parse-exp x))))










