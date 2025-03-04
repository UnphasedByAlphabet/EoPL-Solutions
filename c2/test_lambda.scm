#lang racket
(require rackunit)
(require "lambda_calc.scm")  ;; Import the file containing your definitions

;; Testing Constructors
(check-equal? (var-exp 'x) '(var_expr x))
(check-equal? (lambda-exp 'x '(+ x 1)) '(lambda_expr (x) (+ x 1)))
(check-equal? (app-exp '(f) '(2)) '(app_expr (f) (2)))

;; Testing Predicates
(check-true (var-exp? '(var_expr x)))
(check-false (var-exp? '(lambda_expr (x) x)))
(check-true (lambda-exp? '(lambda_expr (x) x)))
(check-false (lambda-exp? '(var_expr x)))
(check-true (app-exp? '(app_expr (f) (2))))
(check-false (app-exp? '(lambda_expr (x) x)))

;; Testing Selectors
(check-equal? (var-exp->var '(var_expr x)) 'x)
(check-equal? (lambda-exp->bound-var '(lambda_expr (x) (+ x 1))) 'x)
(check-equal? (lambda-exp->body '(lambda_expr (x) (+ x 1))) '(+ x 1))
(check-equal? (app-exp->rator '(app_expr (f) (2))) '(f))
(check-equal? (app-exp->rand '(app_expr (f) (2))) '(2))

;; Testing Parsing
(check-equal? (parse-expression 'x) '(var_expr x))
(check-equal? (parse-expression '(lambda (x) (+ x 1))) '(lambda_expr (x) (+ x 1)))
(check-equal? (parse-expression '(f 2)) '(app_expr f 2))

;; Testing Unparsing
(check-equal? (unparse-expression '(var_expr x)) 'x)
(check-equal? (unparse-expression '(lambda_expr (x) (+ x 1))) '(lambda (x) (+ x 1)))
(check-equal? (unparse-expression '(app_expr f 2)) '(f 2))

;; Testing Invalid Expressions (Error Handling)
(check-exn exn:fail? (lambda () (parse-expression 'invalid-expr)))
(check-exn exn:fail? (lambda () (unparse-expression 'invalid-expr)))
