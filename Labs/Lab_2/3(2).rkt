(define (ref xs . args)
  (define (helper xs id new-element i new-xs type)
    (cond ((vector? xs) (helper (vector->list xs) id new-element i new-xs "vector"))
          ((string? xs) (helper (string->list xs) id new-element i new-xs "string"))
          ((list? xs) (begin
                        (if (= id i)
                            (begin
                              (cond
                                ((eq? type "vector")(list->vector new-xs))
                                ((eq? type "string")(begin
                                                      (if (char? (car new-element))
                                                          (list->string new-xs)
                                                          #f)))
                                (else new-xs)))
                            (helper (cdr xs) id new-element (+ i 1) (append (list (car xs)) new-element (cdr xs)) type))))
          (else #f)))
  (if (or (null? xs) (null? args) (null? (cdr args)))
      #f
      (helper xs (car args) (cdr args) 0 #f "non")))