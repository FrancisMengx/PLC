
(define-datatype kontinuation kontinuation?
	[init-k]
	[rator-k (rands (list-of expression?))
			 (env environment?)
			 (k kontinuation?)]
	[rands-k (proc-value scheme-value?)
			 (k kontinuation?)]
	[prim-k (args (list-of expression?))
			 (env environment?)
			 (k kontinuation?)]
	[callcc-k (next-cont kontinuation?)]
	[exit-k]
)

(define (apply-k k val)
  (cases kontinuation k
    [init-k () val]
    [rator-k (rands env k)
			 (eval-rands rands
				env
			 (rands-k val k))]
	[rands-k (proc-value k)
			(if (box? proc-value)
			    (apply-proc (unbox proc-value) val k)
			    (apply-proc proc-value val k))
			]
	[prim-k (args env k)
			(eval-rands args env (rands-k val k))]

	[callcc-k (next-k)
		(cases proc-val val
          [closure (id body env)
            (eval-exp (car body) (extend-env (map (lambda (x) (cadr x)) id) (list (cont next-k)) env) next-k)]
          [prim-proc (op) (apply-prim-proc op proc-val next-k)]
          [cont (k)
            (apply-k k (car proc-val))]
          [else
            (eopl:error 'apply-k "call/cc did not receive a proper procedure ~s" val)])]
	[exit-k	() (begin (pretty-print val))]
  ))
