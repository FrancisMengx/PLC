
;; Parsed expression datatypes
(define-datatype expression expression?  ; based on the simple expression grammar, EoPL-2 p6
	[var-exp 			(id symbol?)]
	[ref-exp			(id symbol?)]
	[val-exp			(val integer?)]
	[lit-exp        	(datum (lambda (x) (ormap (lambda (pred) (pred x)) (list number? vector? boolean? symbol? string? pair? null?))))]
	[lambda-exp			(args expression?)
						(body (list-of expression?))]
	[lambda-list-exp	(args (lambda (x) (or (null? x) (pair? x))))
						(body (list-of expression?))]

	[let-exp			(assign list?)
						(body (list-of expression?))]
	[named-let-exp		(name expression?)
						(assign list?)
						(body (list-of expression?))]
	[letrec-exp			(assign list?)
						(body (list-of expression?))]
	[let*-exp			(assign list?)
						(body (list-of expression?))]		
	[case-exp 			(c expression?)
						(cases (lambda (e) (map (lambda (x) (or (list? x) (eqv? 'else x))) e)))
						(bodies (lambda (x) (map (list-of expression?) x)))]
	[app-exp			(rator expression?)
						(rands (list-of expression?))]
	[prim-exp			(proc proc-val?)
						(args list?)]
	[if-exp 			(con expression?)
						(body expression?)]
	[if-else-exp		(con expression?)
						(t-body expression?)
						(f-body expression?)]
	[begin-exp			(bodies (list-of expression?))]
	[set!-exp			(var symbol?)
						(val expression?)]
	[define-exp			(var symbol?)
						(val expression?)]
	[and-exp 			(bodies (list-of expression?))]
	[or-exp 			(bodies (list-of expression?))]
	[cond-exp 			(cases (list-of expression?))
						(bodies (lambda (x) (map (list-of expression?) x)))]
	[while-exp 			(con expression?)
						(bodies (list-of expression?))]
	[callcc-exp			(rec expression?)]
	)

	

; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.



;; environment type definitions
(define scheme-value?
  (lambda (x) #t))



(define-datatype environment environment?
  [empty-env-record]
  [extended-env-record
   (syms (list-of symbol?))
   (vals (list-of box?))
   (env environment?)]
  [recursively-extended-env-record
	(proc-names (list-of symbol?))
	(ids (list-of (list-of symbol?)))
	(bodies (list-of (list-of expression?)))
	(env environment?)])



(define-datatype proc-val proc-val?
	[prim-proc 		(name (lambda (x) (member x *prim-proc-names*)))]
	[closure 		(vars (lambda (x) (or (symbol? x) (pair? x) (null? x))))
					(body (list-of expression?))
					(e environment?)]
	[cont  			(k kontinuation?)])


