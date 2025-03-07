#lang racket
(require eopl/eopl)
(require eopl)

(define-datatype s-list s-list?
  (empty-s-list)
  (non-empty-s-list
    (first s-exp?)
    (rest s-list?)
  )
)

(define-datatype s-exp s-exp?
  (symbol-s-exp (sym symbol?))
  (s-list-s-exp (slst s-list?))
)

(define-datatype environment environment?
  (empty-env)
  (non-empty-env
    (key s-exp?)
    (value s-exp?)
    (next environment?)))

(define has-bindings?
  (lambda (env var)
    (cases environment env
      (empty-env () #f)  ; Base case: empty environment returns #f
      (non-empty-env (key value next)
        (or
          (eqv? (
            cases s-exp key (symbol-s-exp (sym) sym) (else #f) # Extract value of s-exp from expression
          ) var)  ; Check if the current key matches var
          (has-bindings? next var))))))  ; Recur on the rest of the environment

(define test-env
  (non-empty-env (symbol-s-exp 'x) (symbol-s-exp 'r)
    (non-empty-env (symbol-s-exp 'y) (symbol-s-exp 'a)
      (non-empty-env (symbol-s-exp 'z) (symbol-s-exp 'c)
        (empty-env)))))  ; Environment: {x -> r, y -> a, z -> c}

(display (has-bindings? test-env 'x))  ; Output: #t
(newline)
(display (has-bindings? test-env 'y))  ; Output: #t
(newline)
(display (has-bindings? test-env 'z))  ; Output: #t
(newline)
(display (has-bindings? test-env 'a))  ; Output: #f
(newline)
(display (has-bindings? (empty-env) 'x))  ; Output: #f
(newline)

