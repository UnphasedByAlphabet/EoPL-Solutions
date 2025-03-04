;duple: Int x SchemeVal -> List
;usage: duple(n, x) = list containing n copies of x
(define duple
  (lambda (n x)
    (if (zero? n)
      '()
      (cons x (duple (- n 1) x)))))

(duple 3 'r)