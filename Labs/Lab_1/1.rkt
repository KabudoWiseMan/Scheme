(define (count x xs)
  (define (counter xs n)
    (if (= (length xs) 0)
        n
        (if (equal? x (car xs))
            (counter (cdr xs) (+ n 1))
            (counter (cdr xs) n))))
  (counter xs 0))