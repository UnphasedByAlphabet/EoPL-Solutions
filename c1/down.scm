; down: List -> List
; usage: (down l) -> wraps each top level element of list in parentheses
(define down
  (lambda (l)
    (if (null? l)
      '()
      (cons (list (car l)) (down (cdr l))))))

(down '(1 2 3))
(down '(a (more (complicated)) (list)))
