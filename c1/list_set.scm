; list-set: List -> List
; usage: (list-set l n x) = sets l[n] <- x and returns l
(define list-set
  (lambda (l n x)
    (if (null? l)
      '()
      (if (zero? n)
        (cons x (cdr l))
        (cons (car l) (list-set (cdr l) (- n 1) x))))))

(list-set '(a b c d) 2 '(1 2))
