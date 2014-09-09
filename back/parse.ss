; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define parse-exp
	(lambda (datum)
		(cond 	[(member datum *prim-proc-names*) 	(lit-exp (prim-proc datum))]
				[(integer? datum) 	(val-exp datum)]
				[(symbol? datum)	(var-exp datum)]
				[(null? datum)		(lit-exp datum)]
				[(vector? datum) 	(lit-exp datum)]
				[(boolean? datum) 	(lit-exp datum)]
				[(number? datum) 	(lit-exp datum)]
				[(string? datum) 	(lit-exp datum)]
				[(pair? datum)		(cond 	[(eqv? (car datum) 'lambda) 				(parse-lambda-exp datum)]
											[(eqv? (car datum) 'set!) 					(parse-set!-exp datum)]
											[(eqv? (car datum) 'if) 					(parse-if-exp datum)]
											[(eqv? (car datum) 'let)  					(parse-let-exp datum)]
											[(eqv? (car datum) 'quote)					(lit-exp (cadr datum))]
											[(member (car datum) *prim-proc-names*) 	(prim-exp (prim-proc (car datum)) (map parse-exp (cdr datum)))]
											[(list? datum) 								(app-exp (parse-exp (car datum)) (map parse-exp (cdr datum)))]
											[else 										(eopl:error 'parse-exp "expression ~s is not a proper list" datum)])]
				[else				(eopl:error 'parse-exp "Invalid concrete syntax ~s" datum)])))

(define parse-lambda-exp
	(lambda (exp)
		(if (> (length exp) 2) 
			(if (symbol? (cadr exp)) (lambda-exp (cadr exp) (map parse-exp (cddr exp)))
				(lambda-list-exp (cadr exp) (map parse-exp (cddr exp))))
			(eopl:error 'parse-exp "lambda-expression: incorrect length ~s" exp))))

(define parse-lambda-args
	(lambda (args)
		(if (andmap symbol? args) args
		(eopl:error 'parse-exp "lambda's formal arguments ~s must all be symbols" args))))

(define parse-if-exp
	(lambda (exp)
		(cond 	[(= (length exp) 3) 	(if-exp (parse-exp (cadr exp)) (parse-exp (caddr exp)))]
				[(= (length exp) 4) 	(if-else-exp (parse-exp (cadr exp)) (parse-exp (caddr exp)) (parse-exp (cadddr exp)))]
				[else 					(eopl:error 'parse-exp "if-expression ~s does not have (only) test, then, and else" exp)])))

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
		(cond 	[(not (list? args)) 								(eopl:error 'parse-exp "decls: not all proper lists: ~s" args)]
				[(not (andmap list? args)) 							(eopl:error 'parse-exp "decls: not all proper lists: ~s" args)]
				[(not (andmap (lambda (l) (= 2 (length l))) args)) 	(eopl:error 'parse-exp "declaration in ~s-exp must be a list of length 2 ~s" args)]
				[(not (andmap (lambda (l) (symbol? (car l))) args))	(eopl:error 'parse-exp "decls: first members must be symbols: ~s" args)]
				[else 												(map list (map var-exp (map car args)) (map parse-exp (map cadr args)))])))