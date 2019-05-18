(define x 1)
(define y 2)
(define-syntax swap!
  (syntax-rules ()
    ((swap! a b)
     (let ((c a))
       (set! a b)
       (set! b c)))))