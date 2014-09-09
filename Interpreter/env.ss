; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define empty-env
	(lambda () (empty-env-record)))

(define extend-env
	(lambda (syms vals env)
		(extended-env-record syms (map box vals) env)))

(define extend-env-recursively
(lambda (proc-names ids bodies old-env)
	(recursively-extended-env-record 
	proc-names ids bodies old-env)))


(define list-find-position
	(lambda (sym los)
		(list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
	(lambda (pred ls)
		(cond 	[(null? ls) 		#f]
				[(pred (car ls))	0]
				[else 				(let ([list-index-r (list-index pred (cdr ls))])
										(if (number? list-index-r) (+ 1 list-index-r) #f))])))

(define (apply-env env sym succeed fail global)
	(deref (apply-env-ref env sym succeed fail global))
)

(define (deref cell)
  (unbox cell))

(define (set-ref! cell value)
	(set-box! cell value)
)

(define apply-env-ref
	(lambda (env sym succeed fail global) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
		(cases environment env
			[empty-env-record () 					(if global (fail) (apply-env-ref global-env sym succeed fail #t))]
			[extended-env-record (syms vals env)	(let ([pos (list-find-position sym syms)])
														(if (number? pos) (apply-k succeed (list-ref vals pos))
															(apply-env-ref env sym succeed fail global)))]
			[recursively-extended-env-record
				(procnames idss bodies old-env)
				(let ([pos 
					(list-find-position sym procnames)])
					(if (number? pos)
					(box (closure (map var-exp (list-ref idss pos))
					(list-ref bodies pos)
					env))
					(apply-env-ref old-env sym succeed fail #f)))]
			)))