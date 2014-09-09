; top-level-eval evaluates a form in the global environment
(define *prim-proc-names* '(+ - * / > < set-car! set-cdr! vector->list list->vector length symbol? procedure? number? pair? vector? list? equal? eq? null? zero? add1 sub1 cons = >= <= not list quote car cdr cadr cdar cddr caar caaar caadr cadar cdaar caddr cdadr cddar cdddr))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))

(define top-level-eval
	(lambda (form)
    ; later we may add things that are not expressions.
	(eval-exp form init-env)))
	; eval-exp is the main component of the interpreter
(define global-env init-env)
(define eval-exp
    (lambda (exp envior)
    (cases expression exp
      [lit-exp (datum)        datum]
      [val-exp (datum)        datum]
      [var-exp (id)         (apply-env envior id; look up its value.
                        (lambda (x) x) ; procedure to call if id is in the environment 
                            (lambda () (eopl:error 'apply-env "variable not found in environment: ~s" id)))]
      [app-exp (rator rands)      (let  ([proc-value (eval-exp rator envior)]
                          [args (eval-rands rands envior)])
                        (apply-proc proc-value args))]
      [prim-exp (proc args)       (apply-proc proc (map (lambda (x) (eval-exp x envior)) args))]
      [if-exp (con body)        (if (eval-exp con envior) (eval-exp body envior))]
      [if-else-exp (con t f)      (eval-exp (if (eval-exp con envior) t f) envior)]
      [let-exp (assign body)     (eval-bodies body (extend-env (map cadr (map car assign)) (map (lambda (x) (eval-exp x envior)) (map cadr assign)) envior))]
      [lambda-list-exp (args body)  (closure args body envior)]
      [lambda-exp (args body)  (closure args body envior)]
      [else               (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))


(define eval-bodies
	(lambda (b env)
		(cond 	[(null? (cdr b)) 	(eval-exp (car b) env)]
				[else 				(eval-exp (car b) env)
									(eval-bodies (cdr b) env)])))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands envior)
    (map (lambda (x) (eval-exp x envior)) rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
      [closure (vars body e) (if (symbol? vars)
                                  (apply-closure (list vars) (list args) body e)
                                  (if (list? vars)
                                      (apply-closure vars args body e)
                                      (let ([f (format-lambda-pair vars args)])
                                        (apply-closure (car f) (cadr f) body e))))]
			; You will add other cases
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define (apply-closure  vars args body envi)
  (eval-bodies body (extend-env vars args envi)))

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
    	[(+)			(apply + args)]
    	[(-)			(apply - args)]
    	[(*)			(apply * args)]
    	[(/)			(apply / args)]
    	[(add1)			(+ (1st args) 1)]
    	[(sub1)			(- (1st args) 1)]
    	[(cons)			(cons (1st args) (2nd args))]
    	[(=)			(= (1st args) (2nd args))]
    	[(>=)			(>= (1st args) (2nd args))]
    	[(<=)			(<= (1st args) (2nd args))]
      [(<)     (< (1st args) (2nd args))]
      [(>)     (> (1st args) (2nd args))]
    	[(not)			(not (1st args))]
    	[(list)			args]
    	[(quote)		args]
    	[(zero?)		(zero? (1st args))]
    	[(null?)		(null? (1st args))]
    	[(eq?)			(apply eq? args)]
    	[(equal?)		(apply equal? args)]
    	[(pair?)		(pair? (1st args))]
    	[(list?)		(list? (1st args))]
    	[(vector?)		(vector? (1st args))]
    	[(number?)		(number? (1st args))]
    	[(symbol?)		(symbol? (1st args))]
    	[(procedure?)	(if (proc-val? (1st args)) #t (procedure? (1st args)))]
    	[(length)		(length (1st args))]
    	[(list->vector)	(list->vector (1st args))]
    	[(vector->list)	(vector->list (1st args))]
    	[(set-car!) 	(set-car! (1st args) (2nd args))]
    	[(set-cdr!) 	(set-cdr! (1st args) (2nd args))]
    	[(car)			(car (1st args))]
    	[(cdr)			(cdr (1st args))]
    	[(caar)			(caar (1st args))]
    	[(cadr)			(cadr (1st args))]
    	[(cddr)			(cddr (1st args))]
    	[(cdar)			(cdar (1st args))]
    	[(caddr)		(caddr (1st args))]
    	[(cdadr)		(cdadr (1st args))]
    	[(cddar)		(cddar (1st args))]
    	[(caadr)		(caadr (1st args))]
    	[(cdaar)		(cdaar (1st args))]
    	[(cadar)		(cadar (1st args))]
    	[(caaar)		(caaar (1st args))]
    	[(cdddr)		(cdddr (1st args))]
    	[else			(error 'apply-prim-proc "Bad primitive procedure name: ~s" prim-proc)])))

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










