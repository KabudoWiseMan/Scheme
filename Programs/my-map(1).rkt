(define (my-map proc xs)
  (if (null? xs)
      '()
      (cons (proc (car xs)) (my-map proc (cdr xs)))))