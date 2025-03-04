; Difftree representation
(define one (lambda () '(one)))
(define difftree-to-int
  (lambda (t)
    (if (eqv? t (one))
      '1
      (- (difftree-to-bin (car t)) (difftree-to-bin (cadr t))))))

(define zero (lambda () (list 'diff (one) (one))))
(define negone (lambda () (list 'diff' (zero) (one))))
(define is-zero? (lambda (t) (= 0 (difftree-to-int(t)))))
(define successor (lambda (t) (list 'diff t (negone))))
(define predecessor (lambda (t) (list 'diff t (one))))

(define difftree-plus
  (lambda (t1 t2)
    (list 'diff t1 (difftree-minus (zero) t2))))

(define difftree-minus
  (lambda (t1 t2)
    (list 'diff t1 t2)))

(zero)
(negone)
(predecessor (negone))
(difftree-plus (successor (negone)) (successor (one)))
