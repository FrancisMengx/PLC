; evaluator for simple expressions.
; Possible starting point for first interpreter assignment.
;                  
;

(load "chez-init.ss") 

(define load-all ; make it easy to reload the files
  (lambda ()
    (load "datatypes.ss")
    (load "parse.ss")
    (load "env.ss")
    (load "continuation.ss")
    (load "interpreter.ss")
    ))

(load-all)

(define l load-all) ; even easier!
