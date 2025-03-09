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
        ("zero?" "(" expression ")")
        zero?-exp)
    (expression
        ("if" expression "then" expression "else" expression)
        if-exp)
    (expression (identifier) var-exp)
    (expression
        ("let" identifier "=" expression "in" expression)
        let-exp)
    (expression ; Extension minus of exercise 3.6
        ("minus" "(" expression ")")
        minus-expression))) 

(sllgen:make-define-datatypes the-lexical-spec the-grammar)
(define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))

(define-datatype expval expval?
    (num-val
        (num number?))
    (bool-val
        (bool boolean?)))

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
        (eopl:error "Variable ~s not found in environment" search-var)))

(define empty-env
(lambda ()
    (lambda (search-var) 
        (report-missing-environment-var search-var))))

(define extend-env
    (lambda (key val env)
        (lambda (search-var)
            (if (eqv? search-var key)
                    val
                    (apply-env env search-var)))))

(define apply-env
    (lambda (env search-var)
        (env search-var)))

(define value-of
(lambda (exp env)
    (cases expression exp
        (const-exp (num) (num-val num))
        (var-exp (var) (apply-env env var))
        (diff-exp (exp1 exp2)
            (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
                (let ((num1 (expval->num val1))
                    (num2 (expval->num val2)))
                    (num-val
                    (- num1 num2)))))
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
        (minus-expression (exp1)
            (let ((val1 (value-of exp1 env)))
                (- (expval->num val1)))))))

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


(value-of-program (scan&parse "minus(v)"))
