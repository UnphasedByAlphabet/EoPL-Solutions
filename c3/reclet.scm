#lang racket
(require eopl)


(define the-lexical-spec
    '((whitespace (whitespace) skip)
        (comment ("%" (arbno (not #\newline))) skip)
        (identifier
            (letter (arbno (or letter digit "_" "-" "?")))
            symbol)
        (number (digit (arbno digit)) number)
        (number ("-" digit (arbno digit)) number)))


(define the-grammar
    '((program (expression) a-program)
    (expression (number) const-exp)
    (expression
        ("-" "(" expression "," expression ")")
        diff-exp)
    (expression 
        ("+" "(" expression "," expression ")")
        add-exp)
    (expression 
        ("*" "(" expression "," expression ")")
        mul-exp)
    (expression 
        ("/" "(" expression "," expression ")")
        quot-exp)
    (expression
        ("zero?" "(" expression ")")
        zero?-exp)
    (expression
        ("proc" "(" (separated-list identifier ",") ")" expression) proc-exp)
    (expression
        ("traceproc" "(" (separated-list identifier ",") ")" expression) traceproc-exp)
    (expression
        ("(" expression (arbno expression) ")") call-exp)
    (expression
        ("if" expression "then" expression "else" expression)
        if-exp)
    (expression (identifier) var-exp)
    (expression
        ("let" identifier "=" expression "in" expression)
        let-exp)
    (expression
        ("letproc" identifier "(" (separated-list identifier ",") ")" expression "in" expression)
        letproc-exp)
    (expression
        ("letrec" identifier "(" (separated-list identifier ",") ")" "=" expression "in" expression)
        letrec-exp)
    (expression ; Extension minus of exercise 3.6
        ("minus" "(" expression ")")
        minus-expression)
    (expression ("emptylist") emptylist-exp)
    (expression ("cons" "(" expression "," expression ")") cons-exp)
    (expression ("null?" "(" expression ")") null?-exp)
    (expression ("car" expression) car-exp)
    (expression ("cdr" expression) cdr-exp)))


(sllgen:make-define-datatypes the-lexical-spec the-grammar)
(define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))

; proc? SchemeVal -> bool
; procedure: Var x Exp x Env -> Proc
(define-datatype proc proc?
    (procedure
        (var (list-of symbol?))
        (body expression?)
        (saved-env environment?)))

(define apply-procedure-custom-env
(lambda (vars body vals env)
    (if (null? vars)
        (value-of body env)
        (if (pair? vars)
            (apply-procedure-custom-env (cdr vars) body (cdr vals)
                (extend-env (car vars) (car vals) env))
            (apply-procedure-custom-env '() body '()
                (extend-env (car vars) (car vals) env))
        ))))

(define apply-procedure
(lambda (exp vals)
    (cases proc exp
        (procedure (vars body saved-env)
            (apply-procedure-custom-env vars body vals saved-env)))))

(define-datatype expval expval?
    (num-val
        (num number?))
    (bool-val
        (bool boolean?))
    (proc-val
        (proc proc?))
    (traceproc-val
        (proc proc?))        
    (emptylist-val)
    (cons-val (first expval?) (rest expval?)))

; expval->num : ExpVal → Int
(define expval->num
(lambda (val)
    (cases expval val
        (num-val (num) num)
        (else (report-expval-extractor-error 'num val)))))

;expval->bool : ExpVal → Bool
(define expval->bool
(lambda (val)
    (cases expval val
        (bool-val (bool) bool)
        (else (report-expval-extractor-error 'bool val)))))

;expval->proc : ExpVal -> proc
(define expval->proc
(lambda (val)
    (cases expval val
        (proc-val (proc) proc)
        (else (report-expval-extractor-error 'proc val)))))

;expval->traceproc : ExpVal -> proc
(define expval->traceproc
(lambda (val)
    (cases expval val
        (traceproc-val (proc) proc)
        (else (report-expval-extractor-error 'proc val)))))


; https://github.com/svenpanne/EOPL3/blob/master/chapter3/exercise-3-09.rkt
(define expval->emptylist?
  (lambda (val)
    (cases expval val
      (emptylist-val () #t)
      (cons-val (first rest) #f)
      (else (report-expval-extractor-error 'cons-or-emptylist val)))))

(define expval->car
    (lambda val
        (cases expval val
            (cons-val (first rest) first)
            (else (report-expval-extractor-error 'conslist val)))))

(define expval->cdr
    (lambda val
        (cases expval val
            (cons-val (first rest) rest)
            (else (report-expval-extractor-error 'conslist val)))))

(define report-expval-extractor-error
(lambda (type val)
    (eopl:error (format "Invalid value (~s) provided for type '~s'" val type))))

(define run
(lambda (string)
    (value-of-program (scan&parse string))))

(define value-of-program
(lambda (pgm)
    (cases program pgm
        (a-program (exp1)
            (value-of exp1 (init-env))))))

(define report-missing-environment-var
    (lambda (search-var)
        (eopl:error (format "Variable ~s not found in environment" search-var))))


; environment?: SchemeVal -> bool
; empty-environment: () -> empty-environment
; extend-environment: symbol x ExpVal x environment -> extend-environment
(define-datatype environment environment?
    (empty-env)
    (extend-env
        (key symbol?)
        (value expval?)
        (saved-env environment?))
    (extend-env-rec
        (key symbol?)
        (boundvars (list-of symbol?))
        (body expression?)
        (saved-env environment?)
    ))

(define apply-env
(lambda (env search-var)
    (cases environment env
        (extend-env (key value saved-env)
            (if (eqv? key search-var)
                value
                (apply-env saved-env search-var)))
        (extend-env-rec (key boundvars body saved-env) 
            (if (eqv? key search-var)
                (proc-val (procedure boundvars body env))
                (apply-env saved-env search-var)))
        (empty-env (report-missing-environment-var search-var)))))


(define value-of
(lambda (exp env)
    (cases expression exp
        (const-exp (num) (num-val num))
        (var-exp (var) (apply-env env var))
        (proc-exp (var body) 
            (proc-val
                (procedure var body env)))
        (traceproc-exp (var body) 
            (traceproc-val
                (procedure var body env)))
        (call-exp (rator rand)
            (let ((args (map (lambda (x) (value-of x env)) rand))
                (val (value-of rator env)))
                (cases expval val
                    (proc-val (proc)
                        (apply-procedure proc args))
                    (traceproc-val (proc)
                        (let ()
                            (display (format "started proc\n"))
                            (define res (apply-procedure proc args))
                            (display (format "finished proc\n"))
                            res))
                    (else (report-expval-extractor-error 'proc val)))))
        (diff-exp (exp1 exp2)
            (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
                (let ((num1 (expval->num val1))
                    (num2 (expval->num val2)))
                    (num-val
                        (- num1 num2)))))
        (add-exp (exp1 exp2)
            (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
                (let ((num1 (expval->num val1))
                    (num2 (expval->num val2)))
                    (num-val
                        (+ num1 num2)))))
        (mul-exp (exp1 exp2)
            (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
                (let ((num1 (expval->num val1))
                    (num2 (expval->num val2)))
                    (num-val
                        (* num1 num2)))))
        (quot-exp (exp1 exp2)
            (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
                (let ((num1 (expval->num val1))
                    (num2 (expval->num val2)))
                    (num-val
                        (quotient num1 num2)))))
        (zero?-exp (exp1)
            (let ((val1 (value-of exp1 env)))
                (let ((num1 (expval->num val1)))
                (if (zero? num1)
                    (bool-val #t)
                    (bool-val #f)))))
        (if-exp (exp1 exp2 exp3)
            (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
                (value-of exp2 env)
                (value-of exp3 env))))
        (let-exp (var exp1 body)
            (let ((val1 (value-of exp1 env)))
                (value-of body (extend-env var val1 env))))
        (letproc-exp (name procvar procbody body)
            (value-of body (extend-env name (proc-val (procedure procvar procbody env)) env)))
        (letrec-exp (name procvar procbody body)
            (value-of body (extend-env-rec name procvar procbody env)))
        (minus-expression (exp1)
            (let ((val1 (value-of exp1 env)))
                (- (expval->num val1))))
        (emptylist-exp () (emptylist-val))
        (cons-exp (new list)
            (cons-val new list))
        (null?-exp (expr)
            (let ((val (value-of expr env)))
                (let ((bool1 (expval->emptylist? val)))
                    (bool-val bool1))))
        (car-exp (expr)
            (let ((val (value-of expr env)))
                (expval->car val)))
        (cdr-exp (expr)
            (let ((val (value-of expr env)))
                (expval->cdr val))))))

;; init-env : -> Env
;; (init-env) builds an environment in which i is bound to the
;; expressed value 1, v is bound to the expressed value 5, and x is
;; bound to the expressed value 10.
(define init-env
(lambda ()
    (extend-env
        'i (num-val 1)
            (extend-env
                'v (num-val 5)
                (extend-env
                    'x (num-val 10)
                    (empty-env))))))

(value-of-program (
    scan&parse 
        "letproc f (x) -(x, 11) in (f (f 77))"))
(value-of-program (
    scan&parse 
        "let f = proc (x, y) +(x, y) in (f 4 3)"))

; factorial: number -> number
(value-of-program (
    scan&parse 
        "letproc f (x, z) 
            if zero? (x) 
                then
                1
                else
                *(x, (z -(x, 1) z)) in 
            let factorial = traceproc (n)
                (f n f)
            in (factorial 4)"))

; factorial recursive
; factorial: number -> number
(value-of-program (
    scan&parse 
        "letrec factorial (n) =
            if zero? (n)
                then 1
                else *(n, (factorial -(n, 1)))
            in (factorial 4)"))
