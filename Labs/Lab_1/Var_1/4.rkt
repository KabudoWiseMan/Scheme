(define (cycle xs n)
  (if (= n 0)
      '()
      (append xs (cycle xs (- n 1)))))