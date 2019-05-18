;
;association
;
(define-syntax ref
  (syntax-rules ()
    ((_ list id) (id_element list id))
    ((_ list id element) (replace list id element))))
;
;id_element
;
(define (id_element xs id)
  (define (helper xs i)
    (cond ((null? xs) #f)
          ((vector? xs) (helper (vector->list xs) i))
          ((string? xs) (helper (string->list xs) i))
          ((list? xs) (begin
                        (if (= id i)
                            (car xs)
                            (helper (cdr xs) (+ i 1)))))
          (else #f)))
  (helper xs 0))
;
;replace
;
(define (replace xs id new-element)
  (define (helper xs id new-element i new-xs type)
    (cond ((vector? xs) (helper (vector->list xs) id new-element i new-xs "vector"))
          ((string? xs) (helper (string->list xs) id new-element i new-xs "string"))
          ((list? xs) (begin
                        (if (= id i)
                            (begin
                              (cond
                                ((eq? type "vector")(list->vector new-xs))
                                ((eq? type "string")(begin
                                                      (if (char? new-element)
                                                          (list->string new-xs)
                                                          #f)))
                                (else new-xs)))
                            (helper (cdr xs) id new-element (+ i 1) (append (list (car xs)) (list new-element) (cdr xs)) type))))
          (else #f)))
  (if (or (null? xs) (null? id) (null? new-element))
      #f
      (helper xs id new-element 0 #f "non")))