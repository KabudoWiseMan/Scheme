(define (horner lst)
  (define (*horner lst acc)
    (if (null? lst)
        acc
        (*horner (cdr lst) (+ (* acc 10) (car lst)))))
  (*horner lst 0))