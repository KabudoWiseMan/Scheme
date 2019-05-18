(define-syntax trace-ex
  (syntax-rules ()
    ((_ args) (begin
                (display (quote args))
                (display " => ")
                (display args)
                (newline)
                args))))