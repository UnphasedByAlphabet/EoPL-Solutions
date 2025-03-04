; invert: List -> List
; usage: (invert list) = list is a list of 2-tuples. returns list of reversed 2-tuples
(define invert
  (lambda (l)
    (if (null? l)
      '()
      (cons 
        (invert-tuple (car l))
        (invert (cdr l))
))))


(define invert-tuple
  (lambda (tuple)
    (list (cadr tuple) (car tuple))))

(invert '((a 1) (b 2)))
