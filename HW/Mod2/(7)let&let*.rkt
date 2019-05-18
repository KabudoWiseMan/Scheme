(define-syntax my-let
  (syntax-rules ()
    ((_ ((name value) ...) . body) ((lambda (name ...) (begin . body)) value ...))
    ((_ variable ((name value) ...) . body) ((letrec ((variable (lambda (name ...) (begin . body))))
                                               variable)
                                             value ...))))
;
(define-syntax my-let*
  (syntax-rules ()
    ((_ () . body) (begin . body))
    ((_ ((name1 value1) (name2 value2) ...) . body) (my-let ((name1 value1))
                                     (my-let* ((name2 value2) ...) . body)))))