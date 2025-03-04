; path: Int x List -> List
; usage (path n lst) = path to n in bintree
(define path
  (lambda (n lst)
    (cond 
      ((null? lst) '())
      ((= (car lst) n) '())
      ((< (car lst) n) 
        (cons 'right (path n (caddr lst))))
      (else (cons 'left (path n (cadr lst)))))))

(path 17 '(14 (7 () (12 () () ()))
              (26 (20 (17 () ()))
              ())
              (31 () ())))
