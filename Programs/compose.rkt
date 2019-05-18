(define-syntax compose
  (syntax-rules ()
    ((_ () () expr) expr)
    ((_ () (f . rfs) (lambda (x) expr))
     (compose () rfs (lambda (x) (f expr))))
    ((_ (f . fs) rfs expr)
     (compose fs (f . rfs) expr))
    ((_ f . fs)
     (compose (f . fs) () (lambda (x) x)))))