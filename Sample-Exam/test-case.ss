(let ([3rd-cdr (nth-cdr 3)]) (3rd-cdr '(a b c d e)))
(let ([0th-cdr (nth-cdr 0)]) (0th-cdr '(a b c d e)))
(let ([8th-cdr (nth-cdr 8)]) (8th-cdr '(a b c d e f g h i)))
(map (lambda (k) 
       ((nth-cdr k) '(1 2 3 4 5 6 7 8 9)))
     '(0 1 2 3 4 5 6 7 8))


(up '((1 2) (3 4)))
(up '((x (y)) z (4) 2))
(up '())
(up '(1 2 3))
(up '((((a)))))
(pair-up '(1 2 3 4 5 6))
(pair-up '(1 2 3 4 5))
(pair-up '(1))
(pair-up '())






(max-contiguous-nonempty-subsequence-sum '(-5)) 
(max-contiguous-nonempty-subsequence-sum '(-3 4 -2)) 
(max-contiguous-nonempty-subsequence-sum '(-2 4 -2 6 -1 3 -18 5 3))
(max-contiguous-nonempty-subsequence-sum '(-7 -6 -4 -8)) 
(max-contiguous-nonempty-subsequence-sum '(-34980498509458045230982382802 -6349302123049834918120 -13498049850945804523098238280 ))
(max-contiguous-nonempty-subsequence-sum '(-2 11 -4 13 -5 2))
(max-contiguous-nonempty-subsequence-sum '(1 -3 4 -2 -1 6))
(max-contiguous-nonempty-subsequence-sum '(1 2 -1 3 -1 4 5 -1 6 7 -1 8 9 -1 10))
