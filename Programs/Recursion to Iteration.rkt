(define (fact n)
  (define (iter p c)
    (if (> c n)
        p
        (iter (* c p)
              (+ c 1))))
  (iter 1 1))