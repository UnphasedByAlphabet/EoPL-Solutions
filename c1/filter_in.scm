; filter-in: List -> List
; usage: (filter-in pred lst) = returns all elements in list which satisfy pred
(define filter-in
  (lambda (pred lst)
    (if (null? lst)
      '()
      (if (pred (car lst))
        (cons (car lst) (filter-in pred (cdr lst)))
        (filter-in pred (cdr lst))))))

(filter-in number? '(a 2 (1 3) b 7))
