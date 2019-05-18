(define-syntax my-if
  (syntax-rules ()
    ((_ return_case) (force return_case))
    ((_ condition expr) (or (and condition expr) (values)))
    ((_ condition if_true if_false)
     (my-if (or (and condition (delay if_true)) (delay if_false))))))