; After loading your interpreter and doing (rep), 
; paste these expressions one-at-a-time into Scheme window.
; There will not be test cases on the server.

(eval-one-exp '
(+ 5 (call/cc 
  (lambda (k) (+ 6 (k 7)))))) ; 1. answer: 12      12 points


(eval-one-exp '
(+ 3 (call/cc (lambda (k) (* 2 5)))))  ; 2. answer: 13  4 points


(begin
  (reset-global-env)
  (eval-one-exp '
   (define xxx #f))
  (eval-one-exp '
   (+ 5 (call/cc (lambda (k) 
		   (set! xxx k)
		   2))))
  (eval-one-exp '
   (* 7 (xxx 4)))) ; answer: 9                       13  points

(eval-one-exp '(call/cc procedure?)) ; answer:  #t   8  points

(begin 
  (reset-global-env)
  (eval-one-exp '
   (define strange1
     (lambda (x)
       (display 1)
       (call/cc x)
       (display 2)
       (newline))))
  
  (eval-one-exp '
   (strange1 (call/cc (lambda (k) (k k))))))  ; answer: 112     20  points

;; It was pointed out late in the process (Saturday evening) that thie
;; next test case violates the ""you do not have to support  re-definition" rule.
;; Thus, below, I provide an alternative test case.  You will get credit if your
;; interpreter passes either one.

; Original code.

(eval-one-exp '(define tester
  (call/cc (lambda (k)
     (lambda (x)
       (if (= x 7)
	   (k 1000)
	   (+ x 4)))))))

(eval-one-exp '(map tester '(1 3 5 7 9 11)))

(eval-one-exp 'tester)  ; answer:  1000    13 points

; Compliant alternative test code. Pas this test or the above test to 
; earn the 13 points.

(eval-one-exp '(define break-out-of-map #f))

(eval-one-exp '
(set! break-out-of-map
  (call/cc (lambda (k)
     (lambda (x)
       (if (= x 7)
	   (k 1000)
	   (+ x 4)))))))

(eval-one-exp '(map break-out-of-map '(1 3 5 7 9 11)))

(eval-one-exp 'break-out-of-map)  ; answer:  1000    13 points

;; The next case does not count in Spring, 2014 because I added it late.
;; I will assign points to it for a future term.

(eval-one-exp '
(define jump-into-map #f))

(eval-one-exp '
(define do-the-map
  (lambda (x) 
    (map (lambda (v)
	   (if (= v 7)
	       (call/cc (lambda (k) (set! jump-into-map k) 100))
	       (+ 3 v)))
	 x))))   

(eval-one-exp '
 (do-the-map '(3 4 5 6 7 8 9 10))) ; answer: (6 7 8 9 100 11 12 13)

(eval-one-exp '
 (list (jump-into-map 987654321))) ; answer: (6 7 8 9 987654321 11 12 13)
	  
	 

;----------------   exit

(begin
  (reset-global-env)
  (eval-one-exp '
   (+ 4 (exit 5 (exit 6 7))) ; answer (6 7)        5 points
))

(begin
  (reset-global-env)
  (eval-one-exp '
   (+  3 (- 2 (exit 5)))))   ; answer (5)         5 points

(begin
  (reset-global-env)
  (eval-one-exp '
   (- 7 (if (exit 3) 4 5)))) ; answer (3)         5 points

(begin 
  (reset-global-env)		    
  (eval-one-exp '(call/cc (lambda (k) (+ 100 (exit (+ 3 (k 12))))))))  ; Answer 12      8 points

(begin
  (reset-global-env)
  (eval-one-exp '(call/cc (lambda (k) (+ 100 (k (+ 3 (exit 12)))))))) ; answer (12)     7 points


		