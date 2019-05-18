(define (replicate x n)
  (if (= n 0)
      '()
      (cons x (replicate x (- n 1)))))