(define var-exp (lambda (var) var))
(define lambda-exp (lambda (var exp) (list 'lambda var exp)))
(define app-exp (lambda (exp1 exp2) (list exp1 exp2)))
(define var-exp? symbol?)
(define lambda-exp? (lambda (exp) (if symbol? exp) #f (if (eqv? (car exp) 'lambda) #t #f)))
(define app-exp? (lambda (exp) (if symbol? exp) #f (if (eqv? (car exp) 'lambda) #f #t)))
(define var-exp->var (lambda (exp) exp))
(define lambda-exp->bound-var (lambda (exp) (car exp)))
(define lambda-exp->body (lambda (exp) (cadr exp)))
(define app-exp->rator (lambda (exp) (car exp)))
(define app-exp->rator (lambda (exp) (cadr exp)))
