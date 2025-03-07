#lang racket
(require eopl)

;; Define constructors for expressions
(define var-exp (lambda (var) (list 'var_expr var)))
(define lambda-exp (lambda (var exp) (list 'lambda_expr (list var) exp)))
(define app-exp (lambda (exp1 exp2) (list 'app_expr exp1 exp2)))

;; Predicates to check expression types
(define var-exp? 
  (lambda (lcexp) 
    (and (pair? lcexp) 
         (eqv? (car lcexp) 'var_expr))))

(define lambda-exp? 
  (lambda (lcexp) 
    (and (pair? lcexp) 
         (eqv? (car lcexp) 'lambda_expr))))

(define app-exp? 
  (lambda (lcexp) 
    (and (pair? lcexp) 
         (eqv? (car lcexp) 'app_expr))))  ;; Fixed from 'lambda_expr to 'app_expr

;; Selectors to extract parts of expressions
(define var-exp->var (lambda (exp) (cadr exp)))
(define lambda-exp->bound-var (lambda (exp) (car (cadr exp))))
(define lambda-exp->body (lambda (exp) (caddr exp)))
(define app-exp->rator (lambda (exp) (cadr exp)))
(define app-exp->rand (lambda (exp) (caddr exp)))  ;; Fixed duplicate definition

;; Parser to convert from S-expression to abstract syntax tree (AST)
(define parse-expression
  (lambda (sval)
    (cond
      [(symbol? sval) (var-exp sval)]
      [(and (pair? sval) (eqv? (car sval) 'lambda)) 
       (lambda-exp (car (cadr sval)) (caddr sval))]
      [(pair? sval) (app-exp (car sval) (cadr sval))]
      [else (report-invalid-expression sval)])))

;; Unparser to convert AST back to S-expression
(define unparse-expression
  (lambda (lcval)
    (cond
      [(var-exp? lcval) (cadr lcval)]
      [(lambda-exp? lcval) (list 'lambda (cadr lcval) (caddr lcval))]
      [(app-exp? lcval) (list (cadr lcval) (caddr lcval))]
      [else (report-invalid-expression lcval)])))  ;; Fixed missing argument

;; Error reporting function
(define (report-invalid-expression expr)
  (eopl:error 'report-invalid-expression
              "Invalid Lambda cond expression: ~a" expr))

;; === Helper Function for Testing ===
(define test 
(lambda (name expected actual)
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (displayln "✅ Pass")
      (begin
        (display "❌ Fail. Expected: ") 
        (display expected)
        (display ", but got: ")
        (displayln actual)))))

;; === Constructor Tests ===
(test "var-exp" '(var_expr x) (var-exp 'x))
(test "lambda-exp" '(lambda_expr (x) (+ x 1)) (lambda-exp 'x '(+ x 1)))
(test "app-exp" '(app_expr f 2) (app-exp 'f '2))

;; === Predicate Tests ===
(test "var-exp? (valid)" #t (var-exp? '(var_expr x)))
(test "var-exp? (invalid)" #f (var-exp? '(lambda_expr (x) x)))
(test "lambda-exp? (valid)" #t (lambda-exp? '(lambda_expr (x) x)))
(test "lambda-exp? (invalid)" #f (lambda-exp? '(var_expr x)))
(test "app-exp? (valid)" #t (app-exp? '(app_expr f 2)))
(test "app-exp? (invalid)" #f (app-exp? '(lambda_expr (x) x)))

;; === Selector Tests ===
(test "var-exp->var" 'x (var-exp->var '(var_expr x)))
(test "lambda-exp->bound-var" 'x (lambda-exp->bound-var '(lambda_expr (x) (+ x 1))))
(test "lambda-exp->body" '(+ x 1) (lambda-exp->body '(lambda_expr (x) (+ x 1))))
(test "app-exp->rator" 'f (app-exp->rator '(app_expr f 2)))
(test "app-exp->rand" '2 (app-exp->rand '(app_expr f 2)))

;; === Parsing Tests ===
(test "parse-expression (variable)" '(var_expr x) (parse-expression 'x))
(test "parse-expression (lambda)" '(lambda_expr (x) (+ x 1)) (parse-expression '(lambda (x) (+ x 1))))
(test "parse-expression (application)" '(app_expr f 2) (parse-expression '(f 2)))

;; === Unparsing Tests ===
(test "unparse-expression (variable)" 'x (unparse-expression '(var_expr x)))
(test "unparse-expression (lambda)" '(lambda (x) (+ x 1)) (unparse-expression '(lambda_expr (x) (+ x 1))))
(test "unparse-expression (application)" '(f 2) (unparse-expression '(app_expr f 2)))

