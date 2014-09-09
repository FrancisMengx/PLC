
;; Parsed expression datatypes
(define-datatype expression expression?  ; based on the simple expression grammar, EoPL-2 p6
	[var-exp 			(id symbol?)]
	[val-exp			(val integer?)]
	[lit-exp        	(datum (lambda (x) (ormap (lambda (pred) (pred x)) (list number? vector? boolean? symbol? string? pair? null?))))]
	[lambda-exp			(args symbol?)
						(body (list-of expression?))]
	[lambda-list-exp	(args pair?)
						(body (list-of expression?))]
	[let-exp			(assign list?)
						(body (list-of expression?))]
	[app-exp			(rator expression?)
						(rands (list-of expression?))]
	[prim-exp			(proc proc-val?)
						(args list?)]
	[if-exp 			(con expression?)
						(body expression?)]
	[if-else-exp		(con expression?)
						(t-body expression?)
						(f-body expression?)]
	[set!-exp			(var symbol?)
						(val expression?)])
	

; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.



;; environment type definitions
(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))

(define-datatype proc-val proc-val?
	[prim-proc 		(name (lambda (x) (member x *prim-proc-names*)))]
	[closure 		(vars (lambda (x) (or (symbol? x) (pair? x))))
					(body (list-of expression?))
					(e environment?)])


