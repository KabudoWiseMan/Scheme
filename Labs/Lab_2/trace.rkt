(define-syntax trace-ex
  (syntax-rules ()
    ((_ expr) (begin
                (let ((v expr))
                (display  (quote expr))
                (display " => ")
                (write v)
                (newline)
                v)))))