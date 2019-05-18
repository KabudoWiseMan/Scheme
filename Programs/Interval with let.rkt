(define (interval x d)
  (let ((xd (/ (* x d) 100.0)))
    (list (- x xd) (+ x xd))))