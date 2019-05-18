(define (intersperse e xs)
  (if (> (length xs) 1)
      (append (list (car xs) e) (intersperse e (cdr xs)))
      xs))
      