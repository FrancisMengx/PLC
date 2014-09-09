; top-level-eval evaluates a form in the global environment
(define *prim-proc-names* '(+ - * / > < assq vector-set! exit quotient member set-car! set-cdr! vector->list list->vector list-tail vector-ref apply append map length symbol? procedure? number? pair? vector? list? equal? eq? eqv? null? zero? add1 sub1 cons = >= <= not list  vector quote car cdr cadr cdar cddr caar caaar caadr cadar cdaar caddr cdadr cddar cdddr))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))

(define reset-global-env 
 (lambda () (set! global-env init-env ))) 

(define top-level-eval
	(lambda (form)
    ; later we may add things that are not expressions.
	(eval-exp form (empty-env) (init-k))))
	; eval-exp is the main component of the interpreter
(define global-env init-env)
(define eval-exp
    (lambda (exp envior k)
    (cases expression exp
      [lit-exp (datum)      		(apply-k k datum)]
      [val-exp (datum)      		(apply-k k datum)]
      [ref-exp  (id)          (apply-env-ref envior id; look up its value.
                              k ; procedure to call if id is in the environment 
                                (lambda () (eopl:error 'apply-env "variable not found in environment: ~s" id)) #f)]
      [var-exp (id)      			(apply-env envior id; look up its value.
                        			k ; procedure to call if id is in the environment 
                            		(lambda () (eopl:error 'apply-env "variable not found in environment: ~s" id)) #f)]
      [app-exp (rator rands)    	(eval-exp rator envior (rator-k rands envior k))]

      [prim-exp (proc args)       	(eval-exp (lit-exp proc) envior (prim-k args envior k))]
      [begin-exp (bodies)           (eval-bodies bodies envior k)]
      [if-exp (con body)      	  	(if (eval-exp con envior k) (eval-exp body envior k))]
      [if-else-exp (con t f)      	(eval-exp (if (eval-exp con envior k) t f) envior k)]
      [letrec-exp (assign body)     (eval-bodies body (extend-env-recursively (map cadr (map car assign)) (map (lambda (x) (map cadr x)) (map cadr (map cadr assign))) (map caddr (map cadr assign)) envior) k)]
      [let*-exp (assign body)       (eval-bodies body (extend-let*-env (map cadr (map car assign))  (map cadr assign) envior) k)]
      [lambda-list-exp (args body)  (apply-k k (closure args body envior))]
      [lambda-exp (args body) 		(apply-k k (closure args body envior))]
      [define-exp (var val)       (set! global-env (extend-env (list var) (list (eval-exp val envior k)) global-env))]
      [and-exp (bodies) 			(if (all-true (map (lambda (x) (eval-exp x envior k)) bodies)) #t #f)]
      [or-exp (bodies) 				(any-true bodies envior k)]
      [while-exp (con bodies) 		(while-loop con bodies envior k)]
      [set!-exp (var val) (let ([value (eval-exp val envior k)]
                                [varval (apply-env envior var; look up its value.

                              (lambda (x) x) ; procedure to call if id is in the environment 
                                (lambda () (eopl:error 'apply-env "variable not found in environment: ~s" var)) #f)])
                            (if (box? varval)
                                (if (box? value)
                                    (set-ref! varval (unbox value))
                                    (set-ref! varval value))
                                (set-ref! (apply-env-ref envior var (lambda (x) x) ; procedure to call if id is in the environment 
                                (lambda () (eopl:error 'apply-env "variable not found in environment: ~s" id)) #f) value)))]
      [cond-exp (conds bodies) 		(eval-bodies (list-ref bodies (first-true-index 0 conds envior k)) envior k)]
      [callcc-exp (rec)           (eval-exp rec envior (callcc-k k))]
      [else 						(eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))


(define (flatten ls)
  (if (null? ls)
      '()
      (append (car ls) (flatten (cdr ls))))
)

(define (extend-let*-env vars vals env)
  (if (null? vars)
      env
      (extend-let*-env (cdr vars) (cdr vals) (extend-env (list (car vars)) (list (eval-exp (car vals) env k)) env)))
)

(define expand-syntax
	(lambda (exp)
	(cases expression exp
		[val-exp (val) 						(val-exp val)]
		[var-exp (id) 						(var-exp id)]
    [ref-exp (id)              (ref-exp id)]
		[lit-exp (body)						(lit-exp body)]
		[lambda-exp (args body) 			(lambda-exp args (map expand-syntax body))]
		[lambda-list-exp (args body) 		(lambda-list-exp args (map expand-syntax body))]
		[set!-exp (var val) 				(set!-exp var (expand-syntax val))]
		[prim-exp (proc args) 				(prim-exp proc (map expand-syntax args))]
		[if-exp (con body) 					(if-exp (expand-syntax con) (expand-syntax body))]
		[if-else-exp (con t-body f-body) 	(if-else-exp (expand-syntax con) (expand-syntax t-body) (expand-syntax f-body))]
		[let-exp (assign body) 				(app-exp (lambda-list-exp (map var-exp (map cadr (map car assign))) (map expand-syntax body)) (map expand-syntax (map cadr assign)))]
    [let*-exp (assign body)       (let*-exp (map (lambda (x) (list (car x) (expand-syntax (cadr x)))) assign) (map expand-syntax body))]
		[letrec-exp (assign body)     (letrec-exp (map (lambda (x) (list (car x) (expand-syntax (cadr x)))) assign) (map expand-syntax body))]
    [named-let-exp (name assign body) (letrec-exp (list (list name (lambda-list-exp (map var-exp (map cadr (map car assign))) (map expand-syntax body)))) (list (app-exp name (map cadr assign))))]
    [cond-exp (conds bodies)			(cond-exp (map expand-syntax conds) (map (lambda (x) (map expand-syntax x)) bodies))]
		[case-exp (c conds bodies)			(case->cond c conds bodies)]
		[app-exp (rator rand) 				(app-exp (expand-syntax rator) (map expand-syntax rand))]
		[begin-exp (bodies) 				(begin-exp (map expand-syntax bodies))]
		[while-exp (con bodies) 			(while-exp (expand-syntax con) (map expand-syntax bodies))]
    [define-exp (var val)       (define-exp var (expand-syntax val))]
		[and-exp (bodies) 					(and-exp (map expand-syntax bodies))]
		[or-exp (bodies) 					(or-exp (map expand-syntax bodies))]
    [callcc-exp (rec)         (callcc-exp (expand-syntax rec))]
  )))
	
(define while-loop
	(lambda (con bodies envior k)
		(if (eval-exp con envior k) (begin (eval-bodies bodies envior k) (while-loop con bodies envior k)))))

(define case->cond
	(lambda (c conds bodies)
		(cond-exp (map (lambda (x) (if (eqv? 'else (cadr x)) (lit-exp #t) (prim-exp (prim-proc 'member) (list c x)))) conds)
		 (map (lambda (x) (map expand-syntax x)) bodies))))

(define first-true-index
	(lambda (c ls envior)
		(cond	[(null? ls)					#f]
				[(eval-exp (car ls) envior k)	c]
				[else						(first-true-index (+ c 1) (cdr ls) envior)])))

(define any-true
	(lambda (ls envior)
		(if (null? ls)
        #f
        (let ([val (eval-exp (car ls) envior k)])
          (if val
              val
              (any-true (cdr ls) envior))))))

(define all-true
	(lambda (ls)
		(cond 	[(null? ls) #t]
				[(car ls)	(all-true (cdr ls))]
				[else 		#f])))

(define eval-bodies
	(lambda (b env k)
		(cond 	[(null? (cdr b)) 	(eval-exp (car b) env k)]
				[else 				(eval-exp (car b) env k)
									(eval-bodies (cdr b) env k)])))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands envior k)
    (map (lambda (x) (if (eqv? 'var-exp (car x))
        (eval-exp (ref-exp (cadr x)) envior k)
        (eval-exp x envior k))) rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args k)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op (map (lambda (x) (if (box? x)
          (unbox x)
          x)) args))]
      [closure (vars body e) (if (null? vars)
          (apply-closure vars args body e k)
          (if (eqv? (car vars) 'var-exp)
                                  (apply-closure (list vars) (list (map (lambda (x) (if (box? x)
                                      (unbox-to-val x)
                                      x)) args)) body e k)
                                  (if (list? vars)
                                      (apply-closure vars (map (lambda (x y) (if (eqv? (car x) 'ref-exp)
                                          y
                                          (if (box? y)
                                              (unbox-to-val y)
                                              y))) vars args) body e k)
                                      (let ([f (format-lambda-pair vars args)])
                                        (apply-closure (car f) (map (lambda (x) (if (box? x)
                                      (unbox-to-val x)
                                      x)) (cadr f)) body e k)))))]
      [cont (k)
            (display args)
            (apply-k k (car args))]
			; You will add other cases
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define (unbox-to-val x)
  (if (not(box? x))
      x
      (unbox-to-val (unbox x)))
)

(define (apply-closure  vars args body envi k)
  (eval-bodies body (extend-env (map (lambda (x) (if (list? x)
      (cadr x)
      x)) vars) args envi) k)
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
    	[(+)			(apply + args)]
    	[(-)			(apply - args)]
    	[(*)			(apply * args)]
    	[(/)			(apply / args)]
    	[(quotient)		(apply quotient args)]
    	[(add1)			(+ (1st args) 1)]
    	[(sub1)			(- (1st args) 1)]
    	[(cons)			(cons (1st args) (2nd args))]
    	[(member) 		(member (1st args) (2nd args))]
    	[(=)			(= (1st args) (2nd args))]
    	[(>=)			(>= (1st args) (2nd args))]
    	[(<=)			(<= (1st args) (2nd args))]
    	[(<)     		(< (1st args) (2nd args))]
    	[(>)    		(> (1st args) (2nd args))]
    	[(not)			(not (1st args))]
    	[(list)			args]
    	[(vector)  		(apply vector args)]
    	[(vector-set!) 	(vector-set! (1st args) (2nd args) (3rd args))]
    	[(vector-ref) 	(vector-ref (1st args) (2nd args))]
    	[(quote)		args]
    	[(apply)    	(apply (lambda x (apply-proc (1st args) x)) (flatten-last (cdr args)))]
    	[(map)      	(map (lambda (x) (apply-proc (1st args) (list x))) (2nd args))]
    	[(zero?)		(zero? (1st args))]
    	[(null?)		(null? (1st args))]
    	[(eq?)			(apply eq? args)]
      [(eqv?)    (apply eqv? args)]
    	[(equal?)		(apply equal? args)]
    	[(pair?)		(pair? (1st args))]
    	[(list?)		(list? (1st args))]
    	[(vector?)		(vector? (1st args))]
    	[(number?)		(number? (1st args))]
    	[(symbol?)		(symbol? (1st args))]
    	[(procedure?)	(if (proc-val? (1st args)) #t (procedure? (1st args)))]
    	[(length)		(length (1st args))]
      [(append)   (append (1st args) (2nd args))]
    	[(list->vector)	(list->vector (1st args))]
    	[(vector->list)	(vector->list (1st args))]
      [(list-tail)  (list-tail (1st args) (2nd args))]
    	[(set-car!) 	(set-car! (1st args) (2nd args))]
    	[(set-cdr!) 	(set-cdr! (1st args) (2nd args))]
      [(assq)     (assq (1st args) (2nd args))]
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
      [(exit)      (apply-k (exit-k) args)]
    	[else			(error 'apply-prim-proc "Bad primitive procedure name: ~s" prim-proc)])))

(define (flatten-last ls)
  (append (list-head ls (- (length ls) 1)) (car (last-pair ls))))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (expand-syntax (parse-exp x )))))






