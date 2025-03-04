(define N 16)

(define zero (lambda () '()))

(define is-zero? (lambda (n) (null? n)))

(define successor
  (lambda (n)
    (if (is-zero? n)
        '(1)  ; Base case for zero
        (let ((r (car n)) (q (cdr n)))  ; Extract first digit and the rest
          (if (< (+ r 1) N)  ; No carry required
              (cons (+ r 1) q)
              (cons 0 (successor q)))))))  ; Carry over to next digit

(define predecessor
  (lambda (n)
    (if (is-zero? n)
        '()  ; Base case for zero
        (let ((r (car n)) (q (cdr n)))
          (if (= r 0)
              (if (is-zero? q)
                  '()  ; Avoid infinite recursion
                  (cons (- N 1) (predecessor q)))  ; Borrow from next digit
              (cons (- r 1) q))))))

;; Test cases
(define a (zero))

(display a) (newline)
(display (successor a)) (newline)
(display (successor (successor a))) (newline)
(display (successor (successor (successor a)))) (newline)
(display (predecessor (successor (successor (successor a))))) (newline)
(display (predecessor (successor (successor a)))) (newline)
(display (predecessor (successor a))) (newline)
