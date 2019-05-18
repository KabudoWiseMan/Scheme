(define (map1/x xs)
  (call-with-current-continuation
   (lambda (escape)
     (map (lambda (x)
          (if (and (number? x)
                   (not (zero? x)))
              (/ 1 x)
              (escape #f)))
     xs))))