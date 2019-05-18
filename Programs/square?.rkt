(define (square? expr)
  (and (list expr)
       (= (length expr) 3)
       (eq? (car expr) 'expt)
       (or (number? (cadr expr))
           (number? (caddr expr)))))