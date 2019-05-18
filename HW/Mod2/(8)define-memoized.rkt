(define-syntax define-memoized
  (syntax-rules ()
    ((_ (name args ...) body ...) (define name
                                    (let ((memo '()))
                                      (lambda (args ...)
                                        (let ((memoized (assoc `(,args ...) memo)))
                                          (if memoized
                                              (cadr memoized)
                                              (let ((new-value
                                                     (begin body ...)))
                                                (set! memo (cons (list `(,args ...) new-value) memo))
                                                new-value)))))))
    ((_ name (lambda-function (args ...) body ...)) (define name
                                                      (let ((memo '()))
                                                        (lambda (args ...)
                                                          (let ((memoized (assoc `(,args ...) memo)))
                                                            (if memoized
                                                                (cadr memoized)
                                                                (let ((new-value
                                                                       (begin body ...)))
                                                                  (set! memo (cons (list `(,args ...) new-value) memo))
                                                                  new-value)))))))))