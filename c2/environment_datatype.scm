#lang racket
(require eopl/eopl)
(require eopl)

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
          (eqv? key var)  ; Check if the current key matches var
          (has-bindings? next var))))))  ; Recur on the rest of the environment

(define test-env
  (non-empty-env 'x 10
    (non-empty-env 'y 20
      (non-empty-env 'z 30
        (empty-env)))))  ; Environment: {x -> 10, y -> 20, z -> 30}

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

