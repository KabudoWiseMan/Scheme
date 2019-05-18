(define (how-many n)
  (case n
    ((0) "none")
    ((1 2) "one or two")
    (else "many")))