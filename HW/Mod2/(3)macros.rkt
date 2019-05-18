(define-syntax when
  (syntax-rules ()
    ((_ cond? . body) (or (and cond? (begin . body)) (values)))))
;
(define-syntax unless
  (syntax-rules ()
    ((_ cond? . body) (or (and (not cond?) (begin . body)) (values)))))
;
;
;
(define-syntax for
  (syntax-rules (in as)
    ((_ x in xs body ...) (for-each (lambda (x) (and body ...)) xs))
    ((_ xs as x body ...) (for-each (lambda (x) (and body ...)) xs))))
;
;
;
(define-syntax while
  (syntax-rules ()
    ((_ cond? . body)
     (letrec ((loop (lambda ()
                      (if cond?
                          (begin (begin . body)
                                 (loop))
                          (values)))))
       (loop)))))
;
(define-syntax repeat
  (syntax-rules ()
    ((_ (body ...) until cond?)
     (letrec ((loop (lambda ()
                      (if (not cond?)
                          (begin body ...
                                 (loop))
                          (values)))))
       (loop)))))
;
;
;
(define-syntax cout
  (syntax-rules ()
    ((_ tail) (letrec ((id (lambda (i)
                               (if (= i (length tail))
                                   (values)
                                   (cond ((equal? (list-ref tail i) '<<) (id (+ i 1)))
                                         ((equal? (list-ref tail i) 'endl) (begin (newline) (id (+ i 1))))
                                         (else (begin (display (list-ref tail i)) (id (+ i 1)))))))))
                (id 0)))
    ((_ head . tail) (cout (quote tail)))))