(define (f n)
  (display "evaluated")
  (newline)
  (+ n 1))

(define p (delay (f 1)))