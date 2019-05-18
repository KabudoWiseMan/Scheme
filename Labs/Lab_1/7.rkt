(define (find-number a b c)
  (and (<= a b)
       (if (integer? (/ a c))
           a
           (find-number (+ a 1) b c))))